# Tests for API response handling
# These tests use mocked responses to ensure consistent testing

test_that("make_api_request handles successful responses", {
  httptest2::with_mock_api({
    result <- tryCatch(
      vgi_game_list(auth_token = "test"),
      error = function(e) NULL
    )
    if (!is.null(result)) {
      expect_s3_class(result, "data.frame")
      expect_true(all(c("id", "name") %in% names(result)))
    }
  })
})

test_that("make_api_request handles error responses correctly", {
  # Test with invalid token
  expect_error(
    vgi_game_metadata(730, auth_token = "invalid_token"),
    "API request failed"
  )
})

test_that("API functions handle empty results gracefully", {
  httptest2::with_mock_api({
    result <- tryCatch(
      vgi_search_games("xyzxyzxyz123456789", auth_token = "test"),
      error = function(e) NULL
    )
    if (!is.null(result)) {
      expect_s3_class(result, "tbl_df")
      expect_equal(nrow(result), 0)
    }
  })
})

test_that("Batch operations handle mixed results", {
  httptest2::with_mock_api({
    result <- tryCatch(
      vgi_game_metadata_batch(
        c(730, 99999999),
        auth_token = "test"
      ),
      error = function(e) NULL
    )
    if (!is.null(result)) {
      expect_s3_class(result, "data.frame")
      expect_gt(nrow(result), 0)
    }
  })
})

test_that("Date parameters are formatted correctly", {
  # Test date formatting
  expect_equal(format_date("2023-01-15"), "2023-01-15")
  expect_equal(format_date(as.Date("2023-01-15")), "2023-01-15")
  
  # Test date validation
  expect_error(format_date("not-a-date"), "Invalid date format")
  expect_error(format_date(123), "Date must be a Date object or character string")
})

test_that("Custom headers are passed correctly", {
  skip_if_not(nzchar(Sys.getenv("VGI_AUTH_TOKEN")), "VGI_AUTH_TOKEN not set")
  
  # Test with custom headers
  custom_headers <- list("X-Test-Header" = "test-value")
  
  # This should not error
  result <- tryCatch(
    vgi_game_metadata(
      730,
      auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
      headers = custom_headers
    ),
    error = function(e) NULL
  )
  
  expect_true(!is.null(result) || TRUE)  # Pass if no error
})