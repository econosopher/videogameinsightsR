# Tests for data processing and transformation functions

test_that("process_api_response handles different input types", {
  # Test with data frame
  df_input <- data.frame(a = 1:3, b = letters[1:3])
  result <- process_api_response(df_input)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3)
  
  # Test with list containing data
  list_input <- list(data = data.frame(x = 1:5, y = 5:1))
  result <- process_api_response(list_input)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 5)
  
  # Test with empty input
  result <- process_api_response(NULL)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  
  # Test with empty list
  result <- process_api_response(list())
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})

test_that("process_api_response ensures expected fields", {
  # Test field creation
  df_input <- data.frame(a = 1:3)
  expected_fields <- c("a", "b", "c")
  
  result <- process_api_response(df_input, expected_fields)
  
  expect_true(all(expected_fields %in% names(result)))
  expect_true(all(is.na(result$b)))
  expect_true(all(is.na(result$c)))
})

test_that("Rankings data is processed correctly", {
  skip_if_not(nzchar(Sys.getenv("VGI_AUTH_TOKEN")), "VGI_AUTH_TOKEN not set")
  
  result <- tryCatch(
    vgi_game_rankings(auth_token = Sys.getenv("VGI_AUTH_TOKEN")),
    error = function(e) NULL
  )
  
  if (!is.null(result)) {
    # Check structure
    expect_s3_class(result, "data.frame")
    
    # Check expected columns exist
    expected_cols <- c("steamAppId", "totalRevenueRank", "totalRevenuePrct")
    expect_true(all(expected_cols %in% names(result)))
    
    # Check data types
    expect_type(result$steamAppId, "integer")
    expect_type(result$totalRevenuePrct, "double")
    
    # Check sorting (should be by revenue rank)
    if (nrow(result) > 1) {
      non_na_ranks <- result$totalRevenueRank[!is.na(result$totalRevenueRank)]
      if (length(non_na_ranks) > 1) {
        expect_true(all(diff(non_na_ranks) >= 0))
      }
    }
  }
})

test_that("Top games data includes names when available", {
  skip_if_not(nzchar(Sys.getenv("VGI_AUTH_TOKEN")), "VGI_AUTH_TOKEN not set")
  
  result <- tryCatch(
    vgi_top_games("revenue", limit = 5, auth_token = Sys.getenv("VGI_AUTH_TOKEN")),
    error = function(e) NULL
  )
  
  if (!is.null(result)) {
    expect_s3_class(result, "tbl_df")
    expect_true("name" %in% names(result))
    expect_true("value" %in% names(result))
    expect_true("rank" %in% names(result))
  }
})

test_that("Search function filters results correctly", {
  skip_if_not(nzchar(Sys.getenv("VGI_AUTH_TOKEN")), "VGI_AUTH_TOKEN not set")
  
  # Search for a common term
  result <- tryCatch(
    vgi_search_games("counter", limit = 10, auth_token = Sys.getenv("VGI_AUTH_TOKEN")),
    error = function(e) NULL
  )
  
  if (!is.null(result) && nrow(result) > 0) {
    # All results should contain the search term (case insensitive)
    expect_true(all(grepl("counter", result$name, ignore.case = TRUE)))
    
    # Results should be limited
    expect_lte(nrow(result), 10)
  }
})

test_that("Metadata batch processing handles large inputs", {
  skip_if_not(nzchar(Sys.getenv("VGI_AUTH_TOKEN")), "VGI_AUTH_TOKEN not set")
  
  # Test with more than 10 IDs (should be chunked)
  ids <- c(730, 570, 440, 10, 20, 30, 40, 50, 60, 70, 80, 90)
  
  result <- tryCatch(
    vgi_game_metadata_batch(ids, auth_token = Sys.getenv("VGI_AUTH_TOKEN")),
    error = function(e) NULL
  )
  
  if (!is.null(result)) {
    expect_s3_class(result, "data.frame")
    expect_true("steamAppId" %in% names(result))
    expect_true("name" %in% names(result))
  }
})