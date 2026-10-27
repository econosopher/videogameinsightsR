# Live smoke tests against the real API. They run only when VGI_AUTH_TOKEN is
# set and pin facts that are stable for Dressmaker (Steam 4019220, released
# 2026-09-21, publisher Free Lives 28663, vgiId 2047657).

skip_if_live_disabled <- function() {
  testthat::skip_if(!nzchar(Sys.getenv("VGI_AUTH_TOKEN")), "VGI_AUTH_TOKEN not set")
  testthat::skip_on_cran()
}

test_that("live: metadata, company and CCU history agree on Dressmaker", {
  skip_if_live_disabled()
  meta <- vgi_game_metadata(4019220)
  expect_equal(meta$name, "Dressmaker")
  expect_true(28663L %in% meta$publishers[[1]]$company_id)
  expect_equal(meta$release_date_steam, "2026-09-21")

  expect_true(4019220L %in% vgi_publisher_games(28663))
  expect_equal(vgi_publisher_info(28663)$name, "Free Lives")

  ccu <- vgi_insights_ccu(4019220)$player_history
  expect_false(anyDuplicated(ccu$date) > 0)
  expect_lt(min(ccu$date), as.Date("2026-09-21"))
  expect_equal(ccu$max[ccu$date == as.Date("2026-09-28")], 32107)
  expect_gt(max(vgi_insights_units(4019220)$units_sold_total, na.rm = TRUE), 100000)
})

test_that("live: metadata, overlap and snapshots resolve the same game by Steam id and slug", {
  skip_if_live_disabled()
  by_id <- vgi_games_metadata(steam_app_ids = 4019220)
  by_slug <- vgi_games_metadata(slugs = "dressmaker")
  expect_equal(by_id$vgi_id, 2047657L)
  expect_equal(by_slug$vgi_id, 2047657L)
  expect_equal(by_id$steam_app_id, 4019220L)

  overlap <- vgi_player_overlap(slug = "dressmaker", limit = 5)
  expect_equal(overlap$steam_app_id, 4019220L)
  expect_s3_class(overlap$player_overlaps, "tbl_df")
  expect_false(all(is.na(overlap$player_overlaps$units_sold_overlap_percentage)))

  snap <- vgi_historical_data_by_date("2026-09-28", steam_app_ids = 4019220)
  expect_equal(snap$ccu_max, 32107L)
})
