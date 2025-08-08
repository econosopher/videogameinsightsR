test_that("vgi_game_metadata validates inputs correctly", {
  # Test missing steam_app_id
  expect_error(
    vgi_game_metadata(NULL),
    "steam_app_id is required"
  )
  
  expect_error(
    vgi_game_metadata(""),
    "steam_app_id is required"
  )
})

test_that("vgi_game_metadata_batch validates inputs correctly", {
  # Test empty vector
  expect_error(
    vgi_game_metadata_batch(c()),
    "steam_app_ids must be a non-empty vector"
  )
  
  # Test NULL input
  expect_error(
    vgi_game_metadata_batch(NULL),
    "steam_app_ids must be a non-empty vector"
  )
  
  # Test invalid values
  expect_error(
    vgi_game_metadata_batch(c("abc", "def")),
    "steam_app_ids contains invalid values"
  )
})

test_that("vgi_search_games validates inputs correctly", {
  # Test missing query
  expect_error(
    vgi_search_games(NULL),
    "query parameter is required"
  )
  
  expect_error(
    vgi_search_games(""),
    "query parameter is required"
  )
  
  # Test invalid limit
  expect_error(
    vgi_search_games("test", limit = 0),
    "limit must be at least 1"
  )
  
  expect_error(
    vgi_search_games("test", limit = 1001),
    "limit must be at most 1000"
  )
})

test_that("vgi_top_games validates inputs correctly", {
  # Test invalid metric
  expect_error(
    vgi_top_games("invalid_metric"),
    "Invalid metric"
  )
  
  # Test invalid platform
  expect_error(
    vgi_top_games("revenue", platform = "invalid_platform"),
    "Invalid platform"
  )
  
  # Test limit validation
  expect_error(
    vgi_top_games("revenue", limit = 0),
    "limit must be at least 1"
  )
})

test_that("validate_platform works correctly", {
  # Valid platforms should not error
  expect_silent(validate_platform("steam"))
  expect_silent(validate_platform("playstation"))
  expect_silent(validate_platform("xbox"))
  expect_silent(validate_platform("nintendo"))
  expect_silent(validate_platform("all"))
  
  # Invalid platform should error
  expect_error(
    validate_platform("pc"),
    "Invalid platform"
  )
})

test_that("validate_numeric works correctly", {
  # Valid numeric values
  expect_silent(validate_numeric(10, "test"))
  expect_silent(validate_numeric(5, "test", min_val = 1, max_val = 10))
  
  # Non-numeric value
  expect_error(
    validate_numeric("abc", "test"),
    "test must be numeric"
  )
  
  # Below minimum
  expect_error(
    validate_numeric(0, "test", min_val = 1),
    "test must be at least 1"
  )
  
  # Above maximum
  expect_error(
    validate_numeric(11, "test", max_val = 10),
    "test must be at most 10"
  )
})

test_that("get_auth_token handles missing token correctly", {
  # Store current token
  old_token <- Sys.getenv("VGI_AUTH_TOKEN")
  
  # Clear token
  Sys.unsetenv("VGI_AUTH_TOKEN")
  
  # Should error when no token
  expect_error(
    get_auth_token(),
    "Authentication token is required"
  )
  
  # Should use provided token
  expect_equal(get_auth_token("test_token"), "test_token")
  
  # Restore token
  if (old_token != "") {
    Sys.setenv(VGI_AUTH_TOKEN = old_token)
  }
})

# Legacy Publisher Functions Tests
test_that("legacy vgi_publisher_info validates inputs correctly", {
  # Test non-numeric input
  expect_error(
    vgi_publisher_info("not_a_number"),
    "company_id must be numeric"
  )
  
  expect_error(
    vgi_publisher_info(NULL),
    "company_id must be numeric"
  )
})

test_that("legacy vgi_publisher_games validates inputs correctly", {
  # Test non-numeric publisher_id
  expect_error(
    vgi_publisher_games("not_a_number"),
    "company_id must be numeric"
  )
  
  expect_error(
    vgi_publisher_games(NULL),
    "company_id must be numeric"
  )
})

# Units Insights Tests
test_that("vgi_insights_units validates inputs correctly", {
  # Test non-numeric steam_app_id
  expect_error(
    vgi_insights_units("not_a_number"),
    "steam_app_id must be numeric"
  )
  
  # Test NULL steam_app_id
  expect_error(
    vgi_insights_units(NULL),
    "steam_app_id must be numeric"
  )
})

# Reviews Insights Tests
test_that("vgi_insights_reviews validates inputs correctly", {
  # Test non-numeric steam_app_id
  expect_error(
    vgi_insights_reviews("not_a_number"),
    "steam_app_id must be numeric"
  )
  
  # Test NULL steam_app_id
  expect_error(
    vgi_insights_reviews(NULL),
    "steam_app_id must be numeric"
  )
})

test_that("format_date works correctly", {
  # Test character date
  expect_equal(format_date("2023-01-15"), "2023-01-15")
  
  # Test Date object
  expect_equal(format_date(as.Date("2023-01-15")), "2023-01-15")
  
  # Test clearly invalid format
  expect_error(format_date("not-a-date-at-all"), "Invalid date format")
  
  # Test non-date input
  expect_error(format_date(123), "Date must be a Date object or character string")
})

# Price History Tests
test_that("vgi_insights_price_history validates inputs correctly", {
  # Test non-numeric steam_app_id
  expect_error(
    vgi_insights_price_history("not_a_number"),
    "steam_app_id must be numeric"
  )
  
  # Test empty currency
  expect_error(
    vgi_insights_price_history(730, currency = ""),
    "currency must be a non-empty character string"
  )
})

# DAU/MAU Tests
test_that("vgi_insights_dau_mau validates inputs correctly", {
  # Test non-numeric steam_app_id
  expect_error(
    vgi_insights_dau_mau("not_a_number"),
    "steam_app_id must be numeric"
  )
  
  # Test NULL steam_app_id
  expect_error(
    vgi_insights_dau_mau(NULL),
    "steam_app_id must be numeric"
  )
})

test_that("null coalescing operator works", {
  expect_equal(NULL %||% "default", "default")
  expect_equal("value" %||% "default", "value")
  expect_equal(0 %||% "default", 0)
  expect_equal(FALSE %||% "default", FALSE)
})

# Playtime Tests
test_that("vgi_insights_playtime validates inputs correctly", {
  # Test non-numeric steam_app_id
  expect_error(
    vgi_insights_playtime("not_a_number"),
    "steam_app_id must be numeric"
  )
  
  expect_error(
    vgi_insights_playtime(NULL),
    "steam_app_id must be numeric"
  )
})

# Player Regions Tests
test_that("vgi_insights_player_regions validates inputs correctly", {
  # Test non-numeric steam_app_id
  expect_error(
    vgi_insights_player_regions("not_a_number"),
    "steam_app_id must be numeric"
  )
  
  expect_error(
    vgi_insights_player_regions(NULL),
    "steam_app_id must be numeric"
  )
})

# Wishlists Tests
test_that("vgi_insights_wishlists validates inputs correctly", {
  # Test non-numeric steam_app_id
  expect_error(
    vgi_insights_wishlists("not_a_number"),
    "steam_app_id must be numeric"
  )
  
  expect_error(
    vgi_insights_wishlists(NULL),
    "steam_app_id must be numeric"
  )
})

# Followers Tests
test_that("vgi_insights_followers validates inputs correctly", {
  # Test non-numeric steam_app_id
  expect_error(
    vgi_insights_followers("not_a_number"),
    "steam_app_id must be numeric"
  )
  
  expect_error(
    vgi_insights_followers(NULL),
    "steam_app_id must be numeric"
  )
})

# Game Rankings Tests
test_that("vgi_game_rankings returns data frame", {
  httptest2::with_mock_api({
    expect_error(
      vgi_game_rankings(auth_token = ""),
      "Authentication token is required"
    )
  })
})

# Player Overlap Tests
test_that("vgi_player_overlap validates inputs correctly", {
  # Test non-numeric steam_app_id
  expect_error(
    vgi_player_overlap("not_a_number"),
    "steam_app_id must be numeric"
  )
  
  expect_error(
    vgi_player_overlap(NULL),
    "steam_app_id must be numeric"
  )
  
  # Test invalid limit
  expect_error(
    vgi_player_overlap(730, limit = 0),
    "limit must be at least 1"
  )
  
  # Test invalid offset
  expect_error(
    vgi_player_overlap(730, offset = -1),
    "offset must be at least 0"
  )
})

# Steam Market Data Tests
test_that("vgi_steam_market_data works correctly", {
  httptest2::with_mock_api({
    expect_error(
      vgi_steam_market_data(auth_token = ""),
      "Authentication token is required"
    )
  })
})

# Historical Data Tests
test_that("vgi_historical_data validates inputs correctly", {
  # Test non-numeric steam_app_id
  expect_error(
    vgi_historical_data("not_a_number"),
    "steam_app_id must be numeric"
  )
  
  expect_error(
    vgi_historical_data(NULL),
    "steam_app_id must be numeric"
  )
})

# Developer Info Tests
test_that("vgi_developer_info validates inputs correctly", {
  # Test non-numeric company_id
  expect_error(
    vgi_developer_info("not_a_number"),
    "company_id must be numeric"
  )
  
  expect_error(
    vgi_developer_info(NULL),
    "company_id must be numeric"
  )
})

# Developer Games Tests
test_that("vgi_developer_games validates inputs correctly", {
  # Test non-numeric company_id
  expect_error(
    vgi_developer_games("not_a_number"),
    "company_id must be numeric"
  )
  
  expect_error(
    vgi_developer_games(NULL),
    "company_id must be numeric"
  )
})

# New Publisher Info Tests
test_that("new vgi_publisher_info validates inputs correctly", {
  # Test non-numeric company_id
  expect_error(
    vgi_publisher_info("not_a_number"),
    "company_id must be numeric"
  )
  
  expect_error(
    vgi_publisher_info(NULL),
    "company_id must be numeric"
  )
})

# New Publisher Games Tests
test_that("new vgi_publisher_games validates inputs correctly", {
  # Test non-numeric company_id
  expect_error(
    vgi_publisher_games("not_a_number"),
    "company_id must be numeric"
  )
  
  expect_error(
    vgi_publisher_games(NULL),
    "company_id must be numeric"
  )
})

# Game List Tests
test_that("vgi_game_list works correctly", {
  httptest2::with_mock_api({
    expect_error(
      vgi_game_list(auth_token = ""),
      "Authentication token is required"
    )
  })
})

# Developer List Tests
test_that("vgi_developer_list works correctly", {
  # Function should work without parameters
  # (but will fail without auth token)
  expect_error(
    vgi_developer_list(auth_token = ""),
    "Authentication token is required"
  )
})

# Publisher List Tests
test_that("vgi_publisher_list works correctly", {
  # Function should work without parameters
  # (but will fail without auth token)
  expect_error(
    vgi_publisher_list(auth_token = ""),
    "Authentication token is required"
  )
})

# Top Countries Tests
test_that("vgi_top_countries validates inputs correctly", {
  # Test non-numeric steam_app_id
  expect_error(
    vgi_top_countries("not_a_number"),
    "steam_app_id must be numeric"
  )
  
  expect_error(
    vgi_top_countries(NULL),
    "steam_app_id must be numeric"
  )
})

# Top Wishlist Countries Tests
test_that("vgi_top_wishlist_countries validates inputs correctly", {
  # Test non-numeric steam_app_id
  expect_error(
    vgi_top_wishlist_countries("not_a_number"),
    "steam_app_id must be numeric"
  )
  
  expect_error(
    vgi_top_wishlist_countries(NULL),
    "steam_app_id must be numeric"
  )
})

# Reviews by Date Tests
test_that("vgi_reviews_by_date validates inputs correctly", {
  # Test invalid date
  expect_error(
    vgi_reviews_by_date("not-a-date"),
    "Invalid date format"
  )
  
  expect_error(
    vgi_reviews_by_date(123),
    "Date must be a Date object or character string"
  )
})

# Wishlists by Date Tests
test_that("vgi_wishlists_by_date validates inputs correctly", {
  # Test invalid date
  expect_error(
    vgi_wishlists_by_date("not-a-date"),
    "Invalid date format"
  )
  
  expect_error(
    vgi_wishlists_by_date(123),
    "Date must be a Date object or character string"
  )
})

# Followers by Date Tests
test_that("vgi_followers_by_date validates inputs correctly", {
  # Test invalid date
  expect_error(
    vgi_followers_by_date("not-a-date"),
    "Invalid date format"
  )
  
  expect_error(
    vgi_followers_by_date(123),
    "Date must be a Date object or character string"
  )
})

# Concurrent Players by Date Tests
test_that("vgi_concurrent_players_by_date validates inputs correctly", {
  # Test invalid date
  expect_error(
    vgi_concurrent_players_by_date("not-a-date"),
    "Invalid date format"
  )
  
  expect_error(
    vgi_concurrent_players_by_date(123),
    "Date must be a Date object or character string"
  )
})

# Active Players by Date Tests
test_that("vgi_active_players_by_date validates inputs correctly", {
  # Test invalid date
  expect_error(
    vgi_active_players_by_date("not-a-date"),
    "Invalid date format"
  )
  
  expect_error(
    vgi_active_players_by_date(123),
    "Date must be a Date object or character string"
  )
})