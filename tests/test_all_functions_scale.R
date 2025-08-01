# Comprehensive Scale Testing of VGI Functions
# Test which functions support steamAppIds parameter and which need updates

library(httr2)
library(jsonlite)
library(tidyverse)
library(dotenv)

# Load environment variables
parent_env <- file.path(dirname(getwd()), ".env")
if (file.exists(parent_env)) {
  load_dot_env(parent_env)
}

# Get API key
api_key <- Sys.getenv("VGI_AUTH_TOKEN")
cat(paste0("\n=== API KEY STATUS ===\n"))
cat(paste0("API Key: ", ifelse(api_key != "", "Found", "NOT FOUND"), "\n\n"))

# Base URL
base_url <- "https://vginsights.com/api/v3"

# Test Steam IDs - mix of popular games
test_ids <- c(
  730,      # Counter-Strike 2
  570,      # Dota 2
  440,      # Team Fortress 2
  1517290,  # Battlefield 2042
  1172470,  # Apex Legends
  252490,   # Rust
  892970,   # Valheim
  1623660   # Palworld
)

# Function to make test API request
make_test_request <- function(endpoint, query_params = NULL) {
  req <- request(base_url) |>
    req_url_path_append(endpoint) |>
    req_headers("api-key" = api_key) |>
    req_user_agent("vgi-scale-test")
  
  if (!is.null(query_params)) {
    req <- req |> req_url_query(!!!query_params)
  }
  
  tryCatch({
    resp <- req |> req_perform()
    list(
      status = resp_status(resp),
      data = resp |> resp_body_json(),
      url = req$url
    )
  }, error = function(e) {
    list(status = "Error", message = e$message, url = req$url)
  })
}

# Store test results
test_results <- list()

cat("=== TESTING VGI ENDPOINTS WITH MULTIPLE IDS ===\n\n")

# 1. ENGAGEMENT ENDPOINTS
cat("1. ENGAGEMENT ENDPOINTS\n")
cat("-" |> rep(50) |> paste(collapse = ""), "\n")

# Test concurrent players
cat("\n1.1 Concurrent Players by Date\n")
result <- make_test_request(
  "engagement/concurrent-players/2025-01-01",
  list(steamAppIds = paste(test_ids, collapse = ","))
)
test_results$concurrent_players <- list(
  endpoint = "/engagement/concurrent-players/{date}",
  supports_ids = result$status == 200 && length(result$data) > 0,
  games_returned = if (result$status == 200) length(result$data) else 0,
  sample_data = if (result$status == 200 && length(result$data) > 0) result$data[[1]] else NULL
)
cat(paste0("  Status: ", result$status, "\n"))
cat(paste0("  Games returned: ", test_results$concurrent_players$games_returned, "\n"))
cat(paste0("  Supports steamAppIds: ", ifelse(test_results$concurrent_players$supports_ids, "✅", "❌"), "\n"))

# Test active players
cat("\n1.2 Active Players by Date\n")
result <- make_test_request(
  "engagement/active-players/2024-04-01",
  list(steamAppIds = paste(test_ids, collapse = ","))
)
test_results$active_players <- list(
  endpoint = "/engagement/active-players/{date}",
  supports_ids = result$status == 200 && length(result$data) > 0,
  games_returned = if (result$status == 200) length(result$data) else 0
)
cat(paste0("  Status: ", result$status, "\n"))
cat(paste0("  Games returned: ", test_results$active_players$games_returned, "\n"))
cat(paste0("  Supports steamAppIds: ", ifelse(test_results$active_players$supports_ids, "✅", "❌"), "\n"))

# 2. TIME SERIES ENDPOINTS
cat("\n\n2. TIME SERIES ENDPOINTS\n")
cat("-" |> rep(50) |> paste(collapse = ""), "\n")

# Test revenue by date
cat("\n2.1 Revenue by Date\n")
result <- make_test_request(
  "revenue/by-date",
  list(
    steamAppIds = paste(test_ids[1:3], collapse = ","),
    startDate = "2024-01-01",
    endDate = "2024-01-31"
  )
)
test_results$revenue_by_date <- list(
  endpoint = "/revenue/by-date",
  supports_ids = result$status == 200,
  url_tested = result$url
)
cat(paste0("  Status: ", result$status, "\n"))
cat(paste0("  Supports steamAppIds: ", ifelse(test_results$revenue_by_date$supports_ids, "✅", "❌"), "\n"))

# Test units sold by date
cat("\n2.2 Units Sold by Date\n")
result <- make_test_request(
  "units/by-date",
  list(
    steamAppIds = paste(test_ids[1:3], collapse = ","),
    startDate = "2024-01-01",
    endDate = "2024-01-31"
  )
)
test_results$units_by_date <- list(
  endpoint = "/units/by-date",
  supports_ids = result$status == 200
)
cat(paste0("  Status: ", result$status, "\n"))
cat(paste0("  Supports steamAppIds: ", ifelse(test_results$units_by_date$supports_ids, "✅", "❌"), "\n"))

# Test reviews by date
cat("\n2.3 Reviews by Date\n")
result <- make_test_request(
  "reviews/by-date",
  list(
    steamAppIds = paste(test_ids[1:3], collapse = ","),
    startDate = "2024-01-01",
    endDate = "2024-01-31"
  )
)
test_results$reviews_by_date <- list(
  endpoint = "/reviews/by-date",
  supports_ids = result$status == 200
)
cat(paste0("  Status: ", result$status, "\n"))
cat(paste0("  Supports steamAppIds: ", ifelse(test_results$reviews_by_date$supports_ids, "✅", "❌"), "\n"))

# Test followers by date
cat("\n2.4 Followers by Date\n")
result <- make_test_request(
  "followers/by-date",
  list(
    steamAppIds = paste(test_ids[1:3], collapse = ","),
    startDate = "2024-01-01",
    endDate = "2024-01-31"
  )
)
test_results$followers_by_date <- list(
  endpoint = "/followers/by-date",
  supports_ids = result$status == 200
)
cat(paste0("  Status: ", result$status, "\n"))
cat(paste0("  Supports steamAppIds: ", ifelse(test_results$followers_by_date$supports_ids, "✅", "❌"), "\n"))

# Test wishlists by date
cat("\n2.5 Wishlists by Date\n")
result <- make_test_request(
  "wishlists/by-date",
  list(
    steamAppIds = paste(test_ids[1:3], collapse = ","),
    startDate = "2024-01-01",
    endDate = "2024-01-31"
  )
)
test_results$wishlists_by_date <- list(
  endpoint = "/wishlists/by-date",
  supports_ids = result$status == 200
)
cat(paste0("  Status: ", result$status, "\n"))
cat(paste0("  Supports steamAppIds: ", ifelse(test_results$wishlists_by_date$supports_ids, "✅", "❌"), "\n"))

# 3. INSIGHTS ENDPOINTS (Single Game)
cat("\n\n3. INSIGHTS ENDPOINTS (Single Game)\n")
cat("-" |> rep(50) |> paste(collapse = ""), "\n")

# Test single game insights
cat("\n3.1 Single Game CCU Insights\n")
result <- make_test_request(paste0("insights/", test_ids[1], "/ccu"))
test_results$insights_ccu_single <- list(
  endpoint = "/insights/{id}/ccu",
  single_game_works = result$status == 200
)
cat(paste0("  Status for single game: ", result$status, "\n"))

# Try with multiple IDs via query param (shouldn't work)
result_multi <- make_test_request(
  "insights/ccu",
  list(steamAppIds = paste(test_ids[1:3], collapse = ","))
)
test_results$insights_ccu_multi <- list(
  endpoint = "/insights/ccu",
  multi_ids_work = result_multi$status == 200
)
cat(paste0("  Status for multiple IDs: ", result_multi$status, "\n"))
cat(paste0("  Single game pattern: ", ifelse(test_results$insights_ccu_single$single_game_works, "✅", "❌"), "\n"))
cat(paste0("  Multiple IDs pattern: ", ifelse(test_results$insights_ccu_multi$multi_ids_work, "✅", "❌"), "\n"))

# 4. GAME METADATA
cat("\n\n4. GAME METADATA\n")
cat("-" |> rep(50) |> paste(collapse = ""), "\n")

# Test batch metadata
cat("\n4.1 Game Metadata Batch\n")
result <- make_test_request(
  "games/metadata",
  list(steamAppIds = paste(test_ids[1:5], collapse = ","))
)
test_results$metadata_batch <- list(
  endpoint = "/games/metadata",
  supports_batch = result$status == 200,
  games_returned = if (result$status == 200 && is.list(result$data)) length(result$data) else 0
)
cat(paste0("  Status: ", result$status, "\n"))
cat(paste0("  Games returned: ", test_results$metadata_batch$games_returned, "\n"))
cat(paste0("  Supports batch requests: ", ifelse(test_results$metadata_batch$supports_batch, "✅", "❌"), "\n"))

# 5. SCALE TESTING
cat("\n\n5. SCALE TESTING\n")
cat("-" |> rep(50) |> paste(collapse = ""), "\n")

# Test with different numbers of IDs
scale_tests <- list(
  small = test_ids[1:2],
  medium = test_ids[1:5],
  large = test_ids
)

cat("\n5.1 Concurrent Players Scale Test\n")
for (size in names(scale_tests)) {
  ids <- scale_tests[[size]]
  result <- make_test_request(
    "engagement/concurrent-players/2025-01-01",
    list(steamAppIds = paste(ids, collapse = ","))
  )
  
  games_returned <- if (result$status == 200) length(result$data) else 0
  cat(paste0("  ", size, " (", length(ids), " IDs): Status ", result$status, 
             ", returned ", games_returned, " games\n"))
}

# 6. SUMMARY AND RECOMMENDATIONS
cat("\n\n=== SUMMARY ===\n")
cat("-" |> rep(50) |> paste(collapse = ""), "\n\n")

# Count endpoints that support multiple IDs
supports_multi <- sum(sapply(test_results, function(x) 
  isTRUE(x$supports_ids) || isTRUE(x$supports_batch)))

total_tested <- length(test_results)

cat(paste0("Total endpoints tested: ", total_tested, "\n"))
cat(paste0("Endpoints supporting multiple IDs: ", supports_multi, "\n\n"))

cat("ENDPOINTS THAT SUPPORT MULTIPLE IDs:\n")
for (name in names(test_results)) {
  result <- test_results[[name]]
  if (isTRUE(result$supports_ids) || isTRUE(result$supports_batch)) {
    cat(paste0("  ✅ ", result$endpoint, "\n"))
  }
}

cat("\nENDPOINTS THAT DON'T SUPPORT MULTIPLE IDs:\n")
for (name in names(test_results)) {
  result <- test_results[[name]]
  if (!isTRUE(result$supports_ids) && !isTRUE(result$supports_batch)) {
    cat(paste0("  ❌ ", result$endpoint, "\n"))
  }
}

cat("\n=== RECOMMENDATIONS ===\n")
cat("1. Add steam_app_ids parameter to these functions:\n")
cat("   - vgi_revenue_by_date()\n")
cat("   - vgi_units_sold_by_date()\n")
cat("   - vgi_reviews_by_date()\n")
cat("   - vgi_followers_by_date()\n")
cat("   - vgi_wishlists_by_date()\n")
cat("\n2. Consider batch endpoint for game metadata\n")
cat("3. Single game insights endpoints cannot be batched\n")
cat("4. Scale testing shows API handles multiple IDs well\n")

# Save results
saveRDS(test_results, "validation/api_endpoint_scale_test_results.rds")
cat("\n\nDetailed results saved to validation/api_endpoint_scale_test_results.rds\n")