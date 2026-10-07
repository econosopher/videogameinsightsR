# v3 endpoint functions replayed against recorded fixtures
# (tests/testthat/api/v3, recorded 2026-10-07 by
# dev/record_fixtures.R for Dressmaker, Steam 4019220). A wrong path or query
# string fails the fixture lookup, so each test also pins the URL contract.

dressmaker <- 4019220L

test_that("vgi_game_metadata returns the flat v3 record with company and tag columns", {
  with_vgi_fixtures({
    meta <- vgi_game_metadata(dressmaker, auth_token = "test")
  })
  expect_s3_class(meta, "tbl_df")
  expect_equal(nrow(meta), 1)
  expect_equal(meta$steam_app_id, dressmaker)
  expect_equal(meta$id, dressmaker)
  expect_equal(meta$name, "Dressmaker")
  expect_equal(meta$price, 14.99)
  expect_equal(meta$release_date, "2026-09-21")
  expect_equal(meta$publisher_id, 28663L)
  expect_equal(meta$publisher_name, "Free Lives")
  expect_type(meta$steam_tags, "list")
  expect_true("Crafting" %in% meta$steam_tags[[1]])
  expect_equal(meta$publishers[[1]]$company_id, 28663L)
})

test_that("vgi_game_metadata returns an empty tibble for an unknown game (404)", {
  with_vgi_fixtures({
    meta <- vgi_game_metadata(999999999, auth_token = "test")
  })
  expect_s3_class(meta, "tbl_df")
  expect_equal(nrow(meta), 0)
})

test_that("vgi_game_metadata_batch keeps successful rows and warns on failures", {
  with_vgi_fixtures({
    expect_warning(
      batch <- vgi_game_metadata_batch(c(dressmaker, 999999999), auth_token = "test"),
      "No metadata found for game 999999999"
    )
  })
  expect_equal(batch$steam_app_id, dressmaker)
  expect_equal(batch$name, "Dressmaker")
})

test_that("vgi_game_rankings reads the catalogue listing and a single game's ranks", {
  with_vgi_fixtures({
    listing <- vgi_game_rankings(limit = 3, auth_token = "test")
    one <- vgi_game_rankings(steam_app_id = dressmaker, auth_token = "test")
  })
  expect_equal(listing$steam_app_id, c(10L, 20L, 30L))
  expect_equal(listing$positive_reviews_rank, c(104L, 2306L, 2733L))
  expect_type(listing$total_revenue_prct, "double")
  expect_type(listing$yesterday_units_sold_rank, "integer")

  expect_equal(nrow(one), 1)
  expect_equal(one$steam_app_id, dressmaker)
  expect_equal(one$total_revenue_rank, 2502L)
  expect_equal(one$total_units_sold_rank, 3061L)
  expect_setequal(names(one), names(listing))
})

test_that("vgi_game_rankings warns once when the removed date argument is used", {
  withr::local_options(vgi.deprecation_frequency = "always")
  with_vgi_fixtures({
    expect_warning(vgi_game_rankings(limit = 3, date = "2026-09-28", auth_token = "test"),
                   class = "vgi_deprecated")
  })
})

test_that("vgi_top_games ranks by the requested metric and attaches names when available", {
  with_vgi_fixtures({
    suppressWarnings(top <- vgi_top_games("revenue", limit = 3, auth_token = "test"))
  })
  expect_true(all(c("steam_app_id", "rank", "percentile", "value") %in% names(top)))
  expect_equal(top$rank, sort(top$rank))
  expect_equal(top$value, top$percentile)
  expect_lte(nrow(top), 3)
})

test_that("vgi_insights_ccu returns the daily CCU history sorted by date", {
  with_vgi_fixtures({
    ccu <- vgi_insights_ccu(dressmaker, version = "v3", auth_token = "test")
  })
  expect_equal(ccu$steam_app_id, dressmaker)
  hist <- ccu$player_history
  expect_named(hist, c("date", "avg", "median", "max", "min"))
  expect_s3_class(hist$date, "Date")
  expect_equal(hist$date, sort(hist$date))
  expect_equal(hist$date[1], as.Date("2026-09-21"))
  day8 <- hist[hist$date == as.Date("2026-09-28"), ]
  expect_equal(day8$max, 32107)
  expect_equal(day8$avg, 25661)
})

test_that("vgi_insights_revenue and vgi_insights_units return cumulative and daily figures", {
  with_vgi_fixtures({
    rev <- vgi_insights_revenue(dressmaker, version = "v3", auth_token = "test")
    units <- vgi_insights_units(dressmaker, version = "v3", auth_token = "test")
  })
  expect_named(rev, c("steam_app_id", "date", "revenue_change", "revenue_total"))
  expect_named(units, c("steam_app_id", "date", "units_sold_change", "units_sold_total"))
  expect_equal(rev$revenue_change[1], 90501)
  expect_equal(rev$revenue_total[1], 90501)
  expect_equal(rev$revenue_total[2], 266993)
  expect_equal(units$units_sold_total[1], 8386L)
  # cumulative totals are non-decreasing
  expect_true(all(diff(rev$revenue_total) >= 0))
  expect_true(all(diff(units$units_sold_total) >= 0))
})

test_that("vgi_insights_reviews derives totals and positive ratio", {
  with_vgi_fixtures({
    reviews <- vgi_insights_reviews(dressmaker, version = "v3", auth_token = "test")
  })
  first <- reviews[1, ]
  expect_equal(first$positive, 294L)
  expect_equal(first$negative, 12L)
  expect_equal(first$total, 306L)
  expect_equal(first$positive_ratio, 294 / 306)
  expect_equal(first$positive_change, 294L)
})

test_that("vgi_insights_wishlists returns the wishlist change series", {
  with_vgi_fixtures({
    wl <- vgi_insights_wishlists(dressmaker, version = "v3", auth_token = "test")
  })
  expect_equal(wl$steam_app_id, dressmaker)
  expect_named(wl$wishlist_changes, c("date", "wishlists_total", "wishlists_change"))
  expect_true(nrow(wl$wishlist_changes) > 5)
  expect_true(all(diff(wl$wishlist_changes$wishlists_total) >= 0))
})

test_that("vgi_insights_price_history returns price periods for one currency", {
  with_vgi_fixtures({
    usd <- vgi_insights_price_history(dressmaker, currency = "USD", version = "v3", auth_token = "test")
  })
  expect_equal(usd$currency, "USD")
  pc <- usd$price_changes
  expect_named(pc, c("currency", "price_initial", "price_final", "first_date", "last_date"))
  expect_equal(nrow(pc), 2)
  # newest period first, open-ended
  expect_equal(pc$first_date[1], as.Date("2026-09-29"))
  expect_true(is.na(pc$last_date[1]))
  launch <- pc[pc$first_date == as.Date("2026-09-21"), ]
  expect_equal(launch$price_final, 13.49)
  expect_equal(launch$last_date, as.Date("2026-09-28"))
})

test_that("vgi_historical_data splits the v3 daily table into per-metric series", {
  with_vgi_fixtures({
    hist <- vgi_historical_data(dressmaker, version = "v3", auth_token = "test")
  })
  expect_setequal(names(hist), c("steam_app_id", "revenue", "units_sold", "concurrent_players",
                                 "active_players", "reviews", "wishlists", "followers",
                                 "price_history", "daily"))
  expect_s3_class(hist$concurrent_players$date, "Date")
  expect_equal(nrow(hist$daily), nrow(hist$revenue))
  day8 <- hist$concurrent_players[hist$concurrent_players$date == as.Date("2026-09-28"), ]
  expect_equal(day8$ccu_max, 32107)
  expect_equal(hist$revenue$revenue[hist$revenue$date == as.Date("2026-09-28")], 1674666)
  expect_equal(hist$price_history$price_final[hist$price_history$date == as.Date("2026-09-28")], 13.49)
  expect_s3_class(hist$followers, "tbl_df")
  expect_named(hist$followers, c("date", "followers", "followers_change"))
})

test_that("vgi_insights_playtime and vgi_top_countries shape the player-insight objects", {
  with_vgi_fixtures({
    pt <- vgi_insights_playtime(dressmaker, version = "v3", auth_token = "test")
    tc <- vgi_top_countries(dressmaker, version = "v3", auth_token = "test")
  })
  expect_equal(pt$steam_app_id, dressmaker)
  expect_equal(pt$median_playtime, 442)
  expect_equal(pt$avg_playtime_rank, 6037L)
  expect_named(pt$playtime_ranges, c("range", "percentage"))
  expect_equal(pt$playtime_ranges$range[1], "0")

  expect_named(tc, c("country", "country_name", "percentage", "rank"))
  expect_equal(tc$country[1], "US")
  expect_equal(tc$percentage[1], 33.33)
  expect_equal(tc$rank, seq_len(nrow(tc)))
})

test_that("vgi_player_overlap (v3) returns overlaps sorted by units overlap and limited", {
  with_vgi_fixtures({
    ov <- vgi_player_overlap(dressmaker, limit = 5, auth_token = "test")
  })
  expect_equal(ov$steam_app_id, dressmaker)
  expect_true(is.na(ov$vgi_id))
  po <- ov$player_overlaps
  expect_equal(nrow(po), 5)
  expect_equal(po$steam_app_id[1], 10L)
  expect_equal(po$units_sold_overlap[1], 10174)
  expect_equal(po$units_sold_overlap_percentage, sort(po$units_sold_overlap_percentage, decreasing = TRUE))
})

test_that("publisher functions return v3 company metrics and Steam game ids", {
  with_vgi_fixtures({
    info <- vgi_publisher_info(28663, auth_token = "test")
    games <- vgi_publisher_games(28663, auth_token = "test")
    listing <- vgi_publishers(limit = 2, auth_token = "test")
  })
  expect_equal(info$company_id, 28663L)
  expect_equal(info$name, "Free Lives")
  expect_equal(info$games_published, 2L)
  expect_equal(info$revenue_total, 3680104)
  expect_equal(games, c(3799100L, 4019220L))
  expect_equal(listing$company_id, c(1L, 3L))
  expect_equal(listing$name[1], "Square Enix")
  expect_true(is.na(listing$revenue_median_per_game[2]))
})
