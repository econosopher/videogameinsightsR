test_that("vgi_insights_price_history maps snake_case price columns", {
  mock_hist <- list(
    priceHistory = data.frame(
      date = c("2024-01-01", "2024-01-02", "2024-01-03", "2024-01-04"),
      price_initial = c(50, 50, 50, 50),
      price_final = c(50, 25, 25, 50),
      stringsAsFactors = FALSE
    )
  )

  testthat::local_mocked_bindings(
    vgi_historical_data = function(steam_app_id, auth_token = Sys.getenv("VGI_AUTH_TOKEN"), headers = list()) {
      mock_hist
    },
    .package = "VideoGameInsightsR"
  )

  result <- vgi_insights_price_history(steam_app_id = 730, auth_token = "test")

  expect_equal(result$steamAppId, 730)
  expect_equal(nrow(result$priceChanges), 3)
  expect_equal(
    names(result$priceChanges),
    c("price_initial", "price_final", "first_date", "last_date")
  )
  expect_true(any(result$priceChanges$price_final == 25))
})

test_that("vgi_insights_price_history maps camelCase price columns", {
  mock_hist <- list(
    priceHistory = data.frame(
      date = c("2024-02-01", "2024-02-02", "2024-02-03"),
      priceInitial = c(60, 60, 60),
      priceFinal = c(60, 30, 60),
      stringsAsFactors = FALSE
    )
  )

  testthat::local_mocked_bindings(
    vgi_historical_data = function(steam_app_id, auth_token = Sys.getenv("VGI_AUTH_TOKEN"), headers = list()) {
      mock_hist
    },
    .package = "VideoGameInsightsR"
  )

  result <- vgi_insights_price_history(steam_app_id = 123, auth_token = "test")

  expect_equal(result$steamAppId, 123)
  expect_equal(nrow(result$priceChanges), 3)
  expect_true(any(result$priceChanges$price_final == 30))
})

test_that("vgi_insights_price_history applies currency filter when available", {
  mock_hist <- list(
    priceHistory = data.frame(
      date = c("2024-03-01", "2024-03-02", "2024-03-03", "2024-03-04"),
      currency = c("USD", "USD", "EUR", "EUR"),
      price_initial = c(40, 40, 40, 40),
      price_final = c(40, 20, 40, 10),
      stringsAsFactors = FALSE
    )
  )

  testthat::local_mocked_bindings(
    vgi_historical_data = function(steam_app_id, auth_token = Sys.getenv("VGI_AUTH_TOKEN"), headers = list()) {
      mock_hist
    },
    .package = "VideoGameInsightsR"
  )

  usd <- vgi_insights_price_history(steam_app_id = 999, currency = "USD", auth_token = "test")
  gbp <- vgi_insights_price_history(steam_app_id = 999, currency = "GBP", auth_token = "test")

  expect_equal(nrow(usd$priceChanges), 2)
  expect_true(any(usd$priceChanges$price_final == 20))
  expect_equal(nrow(gbp$priceChanges), 0)
})
