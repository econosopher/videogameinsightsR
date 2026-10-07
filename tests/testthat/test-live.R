# Live smoke tests against the real API. They run only when VGI_AUTH_TOKEN is
# set and pin facts that are stable for Dressmaker (Steam 4019220, released
# 2026-09-21, publisher Free Lives 28663, vgiId 2047657).

skip_if_live_disabled <- function() {
  testthat::skip_if(!nzchar(Sys.getenv("VGI_AUTH_TOKEN")), "VGI_AUTH_TOKEN not set")
  testthat::skip_on_cran()
}

test_that("live: v3 metadata, rankings and CCU history agree on Dressmaker", {
  skip_if_live_disabled()
  meta <- vgi_game_metadata(4019220)
  expect_equal(meta$name, "Dressmaker")
  expect_equal(meta$publisher_id, 28663L)
  expect_equal(meta$release_date, "2026-09-21")

  ranks <- vgi_game_rankings(steam_app_id = 4019220)
  expect_equal(ranks$steam_app_id, 4019220L)
  expect_false(is.na(ranks$total_revenue_rank))

  ccu_v3 <- vgi_insights_ccu(4019220, version = "v3")$player_history
  expect_gte(nrow(ccu_v3), 10)
  expect_equal(min(ccu_v3$date), as.Date("2026-09-21"))
  expect_gt(max(ccu_v3$max, na.rm = TRUE), 1000)
})

test_that("live: v4 (default) and v3 per-game series agree on Dressmaker", {
  skip_if_live_disabled()
  ccu_v4 <- vgi_insights_ccu(4019220)$player_history
  expect_false(anyDuplicated(ccu_v4$date) > 0)
  expect_lt(min(ccu_v4$date), as.Date("2026-09-21"))
  expect_equal(ccu_v4$max[ccu_v4$date == as.Date("2026-09-28")], 32107)

  units_v4 <- vgi_insights_units(4019220)
  units_v3 <- vgi_insights_units(4019220, version = "v3")
  day <- as.Date("2026-09-28")
  expect_equal(units_v4$units_sold_total[units_v4$date == day],
               units_v3$units_sold_total[units_v3$date == day])
  expect_gt(max(units_v4$units_sold_total, na.rm = TRUE), 100000)
})

test_that("live: v4 metadata resolves the same game by Steam id and slug", {
  skip_if_live_disabled()
  by_id <- vgi_games_metadata(steam_app_ids = 4019220)
  by_slug <- vgi_games_metadata(slugs = "dressmaker")
  expect_equal(by_id$vgi_id, 2047657L)
  expect_equal(by_slug$vgi_id, 2047657L)
  expect_equal(by_id$steam_app_id, 4019220L)

  overlap <- vgi_player_overlap(slug = "dressmaker", limit = 5)
  expect_equal(overlap$steam_app_id, 4019220L)
  expect_s3_class(overlap$player_overlaps, "tbl_df")

  snap <- vgi_historical_data_by_date("2026-09-28", steam_app_ids = 4019220)
  expect_equal(snap$ccu_max, 32107L)
})
