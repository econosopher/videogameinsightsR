test_that("vgi_insights_ccu maps snake_case concurrentPlayers columns", {
  mock_hist <- list(
    concurrentPlayers = data.frame(
      date = c("2025-07-25", "2025-07-24"),
      ccu_avg = c(20, 10),
      ccu_median = c(18, 9),
      ccu_max = c(30, 15),
      stringsAsFactors = FALSE
    )
  )

  testthat::local_mocked_bindings(
    vgi_historical_data = function(steam_app_id, auth_token = Sys.getenv("VGI_AUTH_TOKEN"), headers = list()) {
      mock_hist
    },
    .package = "VideoGameInsightsR"
  )

  result <- vgi_insights_ccu(steam_app_id = 2807960, auth_token = "test")

  expect_equal(result$steamAppId, 2807960)
  expect_equal(names(result$playerHistory), c("date", "avg", "median", "max"))
  expect_equal(as.character(result$playerHistory$date), c("2025-07-24", "2025-07-25"))
  expect_equal(result$playerHistory$avg, c(10, 20))
  expect_equal(result$playerHistory$median, c(9, 18))
  expect_equal(result$playerHistory$max, c(15, 30))
})

test_that("vgi_insights_ccu still maps camelCase concurrentPlayers columns", {
  mock_hist <- list(
    concurrentPlayers = data.frame(
      date = c("2024-01-01", "2024-01-02"),
      ccuAvg = c(100, 110),
      ccuMedian = c(90, 95),
      ccuMax = c(150, 160),
      stringsAsFactors = FALSE
    )
  )

  testthat::local_mocked_bindings(
    vgi_historical_data = function(steam_app_id, auth_token = Sys.getenv("VGI_AUTH_TOKEN"), headers = list()) {
      mock_hist
    },
    .package = "VideoGameInsightsR"
  )

  result <- vgi_insights_ccu(steam_app_id = 730, auth_token = "test")

  expect_equal(result$steamAppId, 730)
  expect_equal(result$playerHistory$avg, c(100, 110))
  expect_equal(result$playerHistory$median, c(90, 95))
  expect_equal(result$playerHistory$max, c(150, 160))
})
