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

# Player Overlap Tests
test_that("vgi_player_overlap validates inputs correctly", {
  # Test non-numeric steam_app_id
  expect_error(
    vgi_player_overlap("not_a_number"),
    "steam_app_id must be numeric"
  )
  
  expect_error(
    vgi_player_overlap(NULL),
    "steam_app_id is required"
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
test_that("vgi_publisher_info validates inputs correctly", {
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
test_that("vgi_publisher_games validates inputs correctly", {
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
    "Date must be a Date object"
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
    "Date must be a Date object"
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
    "Date must be a Date object"
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
    "Date must be a Date object"
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
    "Date must be a Date object"
  )
})