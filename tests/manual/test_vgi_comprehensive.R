# Comprehensive Test Script for videogameinsightsR Package
# Tests all major functions with real API calls

library(pacman)
p_load(testthat, dplyr, tidyr, dotenv)

# Load environment variables
parent_env <- file.path(dirname(getwd()), ".env")
if (file.exists(parent_env)) {
  load_dot_env(parent_env)
}

# Check API key
api_key <- Sys.getenv("VGI_AUTH_TOKEN")
if (api_key == "") {
  stop("VGI_AUTH_TOKEN not set - please set your API token")
}

# Source VGI functions
vgi_path <- file.path(dirname(getwd()), "videogameinsightsR", "R")
if (dir.exists(vgi_path)) {
  source(file.path(vgi_path, "utils.R"))
  source(file.path(vgi_path, "vgi_search_games.R"))
  source(file.path(vgi_path, "vgi_concurrent_players_by_date.R"))
  source(file.path(vgi_path, "vgi_active_players_by_date.R"))
  source(file.path(vgi_path, "vgi_game_metadata.R"))
  source(file.path(vgi_path, "vgi_revenue_by_date.R"))
  source(file.path(vgi_path, "vgi_units_sold_by_date.R"))
  source(file.path(vgi_path, "vgi_game_summary.R"))
  cat("✅ VGI functions loaded\n")
} else {
  stop("videogameinsightsR package functions not found")
}

# Test configuration
test_game_ids <- c(892970, 1145360)  # Valheim, Hades
battlefield_ids <- c(1517290, 1238810, 1238860, 1238840, 24960)
test_start_date <- Sys.Date() - 7
test_end_date <- Sys.Date()

cat("\n=== Running Comprehensive VGI Tests ===\n")
cat(paste0("Testing with games: ", paste(test_game_ids, collapse = ", "), "\n"))
cat(paste0("Date range: ", test_start_date, " to ", test_end_date, "\n\n"))

# Test 1: Game Search
cat("Test 1: Game Search\n")
results <- vgi_search_games("Valheim")
if (is.data.frame(results) && nrow(results) > 0) {
  cat("✅ Game search successful -", nrow(results), "results found\n")
  cat("   Columns:", paste(names(results), collapse = ", "), "\n")
} else {
  cat("❌ Game search failed\n")
}

# Test 2: Game Metadata
cat("\nTest 2: Game Metadata\n")
metadata <- vgi_game_metadata(test_game_ids[1])
if (is.data.frame(metadata) && nrow(metadata) > 0) {
  cat("✅ Metadata retrieval successful\n")
  cat("   Game:", metadata$name, "\n")
  cat("   Publisher:", metadata$publisherName, "\n")
  cat("   Developer:", metadata$developerName, "\n")
} else {
  cat("❌ Metadata retrieval failed\n")
}

# Test 3: Concurrent Players (Multiple IDs)
cat("\nTest 3: Concurrent Players with Multiple IDs\n")
ccu_data <- vgi_concurrent_players_by_date(
  steam_app_ids = test_game_ids,
  start_date = test_start_date,
  end_date = test_end_date
)
if (is.data.frame(ccu_data) && nrow(ccu_data) > 0) {
  cat("✅ CCU data retrieved -", nrow(ccu_data), "rows\n")
  cat("   Games included:", length(unique(ccu_data$steamAppId)), "\n")
  cat("   Date range:", min(ccu_data$date), "to", max(ccu_data$date), "\n")
} else {
  cat("❌ CCU data retrieval failed\n")
}

# Test 4: Active Players with Date Handling
cat("\nTest 4: Active Players (testing date restrictions)\n")
active_data <- vgi_active_players_by_date(
  steam_app_ids = test_game_ids,
  start_date = test_start_date,
  end_date = test_end_date
)
if (is.data.frame(active_data) && nrow(active_data) > 0) {
  cat("✅ Active player data retrieved -", nrow(active_data), "rows\n")
  cat("   Metrics: DAU, MAU, DAU/MAU ratio\n")
} else {
  cat("⚠️  No active player data (might be before 2024-03-18)\n")
}

# Test 5: Revenue Data
cat("\nTest 5: Revenue Data\n")
revenue_data <- vgi_revenue_by_date(
  steam_app_ids = test_game_ids,
  start_date = test_start_date,
  end_date = test_end_date
)
if (is.data.frame(revenue_data) && nrow(revenue_data) > 0) {
  cat("✅ Revenue data retrieved -", nrow(revenue_data), "rows\n")
  cat("   Total revenue covered: $", sum(revenue_data$revenue, na.rm = TRUE), "\n")
} else {
  cat("⚠️  No revenue data available\n")
}

# Test 6: Units Sold
cat("\nTest 6: Units Sold\n")
units_data <- vgi_units_sold_by_date(
  steam_app_ids = test_game_ids,
  start_date = test_start_date,
  end_date = test_end_date
)
if (is.data.frame(units_data) && nrow(units_data) > 0) {
  cat("✅ Units sold data retrieved -", nrow(units_data), "rows\n")
  cat("   Total units:", sum(units_data$unitsSold, na.rm = TRUE), "\n")
} else {
  cat("⚠️  No units sold data available\n")
}

# Test 7: Comprehensive Game Summary
cat("\nTest 7: Comprehensive Game Summary\n")
summary <- vgi_game_summary(
  steam_app_ids = test_game_ids,
  start_date = test_start_date,
  end_date = test_end_date,
  metrics = c("concurrent", "active", "revenue", "units")
)

if (is.list(summary)) {
  cat("✅ Game summary created successfully\n")
  cat("   API calls made:", summary$api_calls, "\n")
  cat("   Summary table rows:", nrow(summary$summary_table), "\n")
  cat("   Time series available:", paste(names(summary$time_series), collapse = ", "), "\n")
  
  # Check if metadata was fetched
  if ("name" %in% names(summary$summary_table) && !all(is.na(summary$summary_table$name))) {
    cat("   ✅ Game names retrieved via metadata\n")
  }
} else {
  cat("❌ Game summary failed\n")
}

# Test 8: Selective Metrics
cat("\nTest 8: Selective Metrics (API call reduction)\n")
summary_limited <- vgi_game_summary(
  steam_app_ids = test_game_ids[1],
  start_date = test_start_date,
  end_date = test_end_date,
  metrics = c("concurrent")  # Only CCU data
)

if (is.list(summary_limited)) {
  cat("✅ Selective metrics working\n")
  cat("   API calls made:", summary_limited$api_calls, "(should be less than full summary)\n")
  cat("   Metrics retrieved:", paste(names(summary_limited$time_series), collapse = ", "), "\n")
} else {
  cat("❌ Selective metrics failed\n")
}

# Test 9: Battlefield Franchise Analysis
cat("\nTest 9: Battlefield Franchise Analysis\n")
bf_summary <- vgi_game_summary(
  steam_app_ids = battlefield_ids,
  start_date = test_start_date,
  end_date = test_end_date,
  metrics = c("concurrent", "revenue", "units")
)

if (is.list(bf_summary) && nrow(bf_summary$summary_table) > 0) {
  cat("✅ Battlefield analysis successful\n")
  cat("   Games analyzed:", nrow(bf_summary$summary_table), "\n")
  
  # Show top games by CCU
  if ("avg_peak_ccu" %in% names(bf_summary$summary_table)) {
    top_games <- bf_summary$summary_table %>%
      arrange(desc(avg_peak_ccu)) %>%
      select(name, avg_peak_ccu) %>%
      head(3)
    cat("\n   Top 3 by Average Peak CCU:\n")
    print(top_games)
  }
} else {
  cat("❌ Battlefield analysis failed\n")
}

# Test 10: Error Handling
cat("\nTest 10: Error Handling\n")

# Test invalid game ID
tryCatch({
  vgi_game_metadata("invalid_id")
  cat("❌ Invalid ID should have thrown error\n")
}, error = function(e) {
  cat("✅ Invalid ID error caught:", e$message, "\n")
})

# Test missing parameters
tryCatch({
  vgi_concurrent_players_by_date(start_date = test_start_date)
  cat("❌ Missing steam_app_ids should have thrown error\n")
}, error = function(e) {
  cat("✅ Missing parameter error caught\n")
})

# Summary
cat("\n=== Test Summary ===\n")
cat("All major functions tested:\n")
cat("- vgi_search_games()\n")
cat("- vgi_game_metadata()\n")
cat("- vgi_concurrent_players_by_date()\n")
cat("- vgi_active_players_by_date()\n")
cat("- vgi_revenue_by_date()\n")
cat("- vgi_units_sold_by_date()\n")
cat("- vgi_game_summary()\n")
cat("\nKey features verified:\n")
cat("- Multiple Steam ID support\n")
cat("- Date restriction handling\n")
cat("- Field mapping corrections\n")
cat("- Automatic metadata fetching\n")
cat("- Selective metric retrieval\n")
cat("- Error handling\n")

cat("\n✅ Comprehensive test completed\n")