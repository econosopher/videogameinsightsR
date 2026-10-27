# Endpoint functions replayed against recorded fixtures
# (tests/testthat/api/v4, recorded 2026-10-07 and 2026-10-27 by
# dev/record_fixtures.R for Dressmaker: Steam 4019220, vgiId 2047657,
# slug "dressmaker"; publisher Free Lives vgiCompanyId 28663).

dressmaker <- 4019220L
free_lives <- 28663L

test_that("vgi_games_metadata flattens the multi-platform record and exposes the cursor", {
  with_vgi_fixtures({
    by_steam <- vgi_games_metadata(steam_app_ids = dressmaker, auth_token = "test")
    by_slug <- vgi_games_metadata(slugs = "dressmaker", auth_token = "test")
  })
  expect_equal(nrow(by_steam), 1)
  expect_equal(by_steam$vgi_id, 2047657L)
  expect_equal(by_steam$slug, "dressmaker")
  expect_equal(by_steam$name, "Dressmaker")
  expect_equal(by_steam$steam_app_id, dressmaker)
  expect_equal(by_steam$price_steam, 14.99)
  expect_true(is.na(by_steam$price_xbox))
  expect_equal(by_steam$release_date_steam, "2026-09-21")
  expect_equal(by_steam$platforms[[1]], "steam")
  expect_true(28663L %in% by_steam$publishers[[1]]$company_id)
  expect_equal(attr(by_steam, "next_cursor"), 2047657L)
  expect_equal(by_slug$vgi_id, by_steam$vgi_id)
})

test_that("vgi_games_metadata pages the catalogue from the cursor", {
  with_vgi_fixtures({
    page <- vgi_games_metadata(limit = 2, auth_token = "test")
  })
  expect_equal(nrow(page), 2)
  expect_equal(page$vgi_id, c(610501L, 610502L))
  expect_equal(attr(page, "next_cursor"), 610502L)
})

test_that("vgi_historical_data_by_date returns the day's snapshot with steam ids", {
  with_vgi_fixtures({
    snap <- vgi_historical_data_by_date("2026-09-28", steam_app_ids = dressmaker, auth_token = "test")
  })
  expect_equal(nrow(snap), 1)
  expect_equal(snap$steam_app_id, dressmaker)
  expect_equal(snap$vgi_id, 2047657L)
  expect_equal(snap$date, "2026-09-28")
  expect_equal(snap$ccu_max, 32107L)
  expect_equal(snap$revenue_total, 1674666)
  expect_equal(snap$dau, 83050L)
  expect_false(any(grepl("^__", names(snap))))
})

test_that("vgi_historical_data_by_date breaks the snapshot down by country", {
  with_vgi_fixtures({
    snap <- vgi_historical_data_by_date("2026-09-28", steam_app_ids = dressmaker,
                                        countries = c("US", "DE"), auth_token = "test")
  })
  expect_setequal(snap$country, c("US", "DE"))
  expect_equal(snap$dau[snap$country == "DE"], 2467L)
  expect_true(snap$mau[snap$country == "US"] > snap$mau[snap$country == "DE"])
})

test_that("by-date wrappers derive their columns from the v4 snapshot", {
  with_vgi_fixtures({
    ccu <- vgi_concurrent_players_by_date("2026-09-28", steam_app_ids = dressmaker, auth_token = "test")
    rev <- vgi_revenue_by_date("2026-09-28", steam_app_ids = dressmaker, auth_token = "test")
    active <- vgi_active_players_by_date("2026-09-28", steam_app_ids = dressmaker, auth_token = "test")
  })
  expect_equal(ccu$peak_concurrent, 32107L)
  expect_equal(ccu$avg_concurrent, 25661L)
  expect_equal(rev$revenue, 1674666)
  expect_equal(rev$daily_revenue, 253493)
  expect_equal(active$dau, 83050L)
  expect_equal(active$dau_mau_ratio, 83050 / 155194)
})

test_that("vgi_price_history returns one row per currency period and filters currencies", {
  with_vgi_fixtures({
    all_cur <- vgi_price_history(steam_app_id = dressmaker, auth_token = "test")
    usd <- vgi_price_history(steam_app_id = dressmaker, currency = "usd", auth_token = "test")
  })
  expect_named(all_cur, c("platform", "external_id", "currency", "price_initial", "price_final",
                          "first_date", "last_date"))
  expect_true(length(unique(all_cur$currency)) > 20)
  expect_equal(unique(all_cur$platform), "steam")
  expect_equal(nrow(usd), 2)
  expect_equal(usd$price_final[usd$first_date == as.Date("2026-09-21")], 13.49)
  # exactly one open-ended (current) period, and it is the latest one
  expect_equal(sum(is.na(usd$last_date)), 1)
  expect_equal(usd$first_date[is.na(usd$last_date)], max(usd$first_date))
})

test_that("company functions select by vgi id and return cursors", {
  with_vgi_fixtures({
    overview <- vgi_publishers_overview(vgi_ids = 28663, auth_token = "test")
    games <- vgi_all_publisher_games(vgi_ids = 28663, auth_token = "test")
    listing <- vgi_publisher_list(limit = 3, auth_token = "test")
  })
  expect_equal(overview$vgi_company_id, 28663L)
  expect_equal(overview$slug, "free-lives")
  expect_equal(overview$country, "South Africa")
  expect_equal(overview$platform_data.steam.revenue_total, 3680104)
  expect_equal(attr(overview, "next_cursor"), 28663L)

  expect_equal(games$publisher_id, 28663L)
  expect_true(2047657L %in% games$game_ids[[1]])
  expect_equal(games$game_count, length(games$game_ids[[1]]))

  expect_equal(nrow(listing), 3)
  expect_named(listing, c("id", "name", "slug", "vgi_url"))
  expect_equal(listing$name, sort(listing$name))
  expect_equal(attr(listing, "next_cursor"), max(listing$id))
})

test_that("multi-game player-insights functions nest per-game breakdowns", {
  with_vgi_fixtures({
    tc <- vgi_all_games_top_countries(steam_app_ids = dressmaker, auth_token = "test")
    pt <- vgi_all_games_playtime(steam_app_ids = dressmaker, countries = "US", auth_token = "test")
  })
  expect_equal(tc$steam_app_id, dressmaker)
  expect_equal(tc$vgi_id, 2047657L)
  expect_equal(tc$top_country, "US")
  expect_equal(tc$top_country_pct, 33.33)
  expect_equal(tc$country_count, nrow(tc$top_countries[[1]]))
  expect_named(tc$top_countries[[1]], c("country", "country_name", "percentage", "rank"))

  expect_equal(pt$steam_app_id, dressmaker)
  expect_equal(pt$median_playtime, 444)
  expect_equal(pt$playtime_rank, 1L)
  expect_equal(sum(pt$playtime_ranges[[1]]$percentage), 100, tolerance = 0.01)
})

test_that("vgi_steam_market_data returns monthly totals with a platform column", {
  with_vgi_fixtures({
    market <- vgi_steam_market_data(auth_token = "test")
  })
  expect_true(all(c("platform", "period", "units_total", "revenue_total", "accounts_total") %in% names(market)))
  expect_equal(market$period[1], "2018-01-01")
  expect_equal(market$units_total[1], 46705118L)
  expect_true(nrow(market) > 90)
})

test_that("per-game time series come from the v4 daily history", {
  with_vgi_fixtures({
    ccu <- vgi_insights_ccu(dressmaker, auth_token = "test")$player_history
    rev <- vgi_insights_revenue(dressmaker, auth_token = "test")
    units <- vgi_insights_units(dressmaker, auth_token = "test")
    reviews <- vgi_insights_reviews(dressmaker, auth_token = "test")
    wl <- vgi_insights_wishlists(dressmaker, auth_token = "test")$wishlist_changes
    hist <- vgi_historical_data(dressmaker, auth_token = "test")
  })
  # one row per day, sorted (the API repeats its page when paged by cursor)
  expect_false(anyDuplicated(ccu$date) > 0)
  expect_equal(ccu$date, sort(ccu$date))
  expect_equal(ccu$date[1], as.Date("2026-09-15"))   # pre-release days are included in v4

  day8 <- as.Date("2026-09-28")
  expect_equal(ccu$max[ccu$date == day8], 32107)
  expect_equal(ccu$avg[ccu$date == day8], 25661)
  # v4 premiumRevenue* / unitsOwned* map onto revenue_* / units_sold_*
  expect_named(rev, c("steam_app_id", "date", "revenue_change", "revenue_total"))
  expect_equal(rev$revenue_total[rev$date == day8], 1674666)
  expect_equal(rev$revenue_change[rev$date == day8], 253493)
  expect_equal(units$units_sold_total[units$date == as.Date("2026-09-21")], 8386L)
  expect_equal(units$units_sold_total[units$date == day8], 155177L)
  expect_equal(reviews$positive[reviews$date == as.Date("2026-09-21")], 294L)
  expect_equal(wl$wishlists_total[wl$date == as.Date("2026-09-15")], 142792L)

  expect_equal(hist$revenue$revenue[hist$revenue$date == day8], 1674666)
  expect_equal(nrow(hist$daily), nrow(ccu))
  expect_false(any(grepl("^__|fields_below", names(hist$daily))))
  expect_true(all(hist$daily$country == "WW"))
})

test_that("per-game player insights read the steam row", {
  with_vgi_fixtures({
    pt <- vgi_insights_playtime(dressmaker, auth_token = "test")
    regions <- vgi_top_regions(dressmaker, auth_token = "test")
    countries <- vgi_top_countries(dressmaker, auth_token = "test")
    wish <- vgi_top_wishlist_countries(dressmaker, auth_token = "test")
  })
  expect_equal(pt$steam_app_id, dressmaker)
  expect_equal(pt$median_playtime, 442)
  expect_equal(pt$avg_playtime_rank, 6037L)
  expect_named(pt$playtime_ranges, c("range", "percentage"))
  expect_equal(nrow(pt$playtime_ranges), 8)

  expect_named(regions, c("region_name", "rank", "percentage"))
  expect_equal(regions$region_name[1], "North America")
  expect_equal(regions$percentage[1], 38.2)

  expect_equal(countries$country[1], "US")
  expect_equal(countries$percentage[1], 33.33)
  expect_named(wish, c("country", "country_name", "percentage", "rank"))
  expect_equal(wish$country[1:2], c("US", "BR"))
  expect_equal(wish$percentage[1], 21.63)
})

test_that("vgi_insights_price_history returns the Steam price periods", {
  with_vgi_fixtures({
    usd <- vgi_insights_price_history(dressmaker, currency = "USD", auth_token = "test")
    all_cur <- vgi_insights_price_history(dressmaker, auth_token = "test")
  })
  expect_equal(usd$currency, "USD")
  pc <- usd$price_changes
  expect_named(pc, c("currency", "price_initial", "price_final", "first_date", "last_date"))
  expect_equal(nrow(pc), 2)
  expect_true(is.na(pc$last_date[1]))            # newest, open-ended period first
  launch <- pc[pc$first_date == as.Date("2026-09-21"), ]
  expect_equal(launch$price_final, 13.49)
  expect_equal(launch$last_date, as.Date("2026-09-28"))

  expect_equal(all_cur$currency, "ALL")
  expect_true(length(unique(all_cur$price_changes$currency)) > 20)
  expect_equal(all_cur$price_changes$currency, sort(all_cur$price_changes$currency))
})

test_that("vgi_game_metadata returns one v4 row and an empty tibble for unknown games", {
  with_vgi_fixtures({
    meta <- vgi_game_metadata(dressmaker, auth_token = "test")
    missing <- vgi_game_metadata(999999999, auth_token = "test")
  })
  expect_equal(nrow(meta), 1)
  expect_equal(meta$name, "Dressmaker")
  expect_equal(meta$vgi_id, 2047657L)
  expect_equal(meta$steam_app_id, dressmaker)
  expect_true(all(c("genre", "subgenre", "steam_genres", "release_date_steam", "publisher_id") %in% names(meta)))
  expect_null(attr(meta, "next_cursor"))
  expect_equal(nrow(missing), 0)
  expect_true("steam_app_id" %in% names(missing))
})

test_that("vgi_game_metadata_batch keeps known games and warns about the rest", {
  with_vgi_fixtures({
    expect_warning(
      batch <- vgi_game_metadata_batch(c(dressmaker, 999999999), auth_token = "test"),
      "999999999"
    )
  })
  expect_equal(batch$steam_app_id, dressmaker)
  expect_equal(batch$name, "Dressmaker")
})

test_that("vgi_player_overlap returns overlaps sorted by units overlap and limited", {
  with_vgi_fixtures({
    ov <- vgi_player_overlap(dressmaker, limit = 5, auth_token = "test")
  })
  expect_equal(ov$steam_app_id, dressmaker)
  expect_equal(ov$vgi_id, 2047657L)
  po <- ov$player_overlaps
  expect_equal(nrow(po), 5)
  expect_equal(po$steam_app_id[1], 413150L)
  expect_equal(po$units_sold_overlap_percentage[1], 50)
  expect_true(all(diff(po$units_sold_overlap_percentage) <= 0))
  expect_true(all(c("vgi_id", "units_sold_overlap", "wishlist_overlap_index") %in% names(po)))
})

test_that("vgi_all_games_player_overlap walks the cursor one game per request", {
  with_vgi_fixtures({
    all_ov <- vgi_all_games_player_overlap(limit = 2, auth_token = "test")
  })
  expect_equal(nrow(all_ov), 2)
  expect_setequal(all_ov$vgi_id, c(610512L, 610517L))
  expect_equal(all_ov$top_overlap_pct, sort(all_ov$top_overlap_pct, decreasing = TRUE))
  expect_equal(attr(all_ov, "next_cursor"), 610517L)
  expect_s3_class(all_ov$top_overlaps[[1]], "tbl_df")
})

test_that("company info uses the Steam platform figures and *_games returns Steam ids", {
  with_vgi_fixtures({
    pub <- vgi_publisher_info(free_lives, auth_token = "test")
    dev <- vgi_developer_info(free_lives, auth_token = "test")
    pub_games <- vgi_publisher_games(free_lives, auth_token = "test")
    dev_games <- vgi_developer_games(free_lives, auth_token = "test")
  })
  expect_equal(pub$company_id, 28663L)
  expect_equal(pub$name, "Free Lives")
  expect_equal(pub$games_published, 2L)
  expect_equal(pub$revenue_total, 5470477)
  expect_equal(dev$games_developed, 9L)
  expect_true(all(c("games_released_steam", "games_released_xbox", "vgi_url") %in% names(dev)))
  expect_equal(pub_games, c(3799100L, 4019220L, 4508230L))
  expect_true(274190L %in% dev_games)   # Broforce; Free Lives publishes but did not develop Dressmaker
  expect_false(dressmaker %in% dev_games)
  expect_equal(dev_games, sort(dev_games))
})
