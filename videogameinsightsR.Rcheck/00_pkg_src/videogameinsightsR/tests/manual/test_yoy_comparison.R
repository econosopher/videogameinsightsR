# Test script for Year-over-Year comparison functionality
library(pacman)
p_load(dplyr, dotenv)

# Load environment variables
parent_env <- file.path(dirname(getwd()), ".env")
if (file.exists(parent_env)) {
  load_dot_env(parent_env)
}

# Source VGI functions
vgi_path <- file.path(dirname(getwd()), "videogameinsightsR", "R")
if (dir.exists(vgi_path)) {
  source(file.path(vgi_path, "utils.R"))
  source(file.path(vgi_path, "vgi_game_metadata.R"))
  source(file.path(vgi_path, "vgi_concurrent_players_by_date.R"))
  source(file.path(vgi_path, "vgi_revenue_by_date.R"))
  source(file.path(vgi_path, "vgi_units_sold_by_date.R"))
  source(file.path(vgi_path, "vgi_game_summary.R"))
  source(file.path(vgi_path, "vgi_game_summary_yoy.R"))
  cat("✅ VGI functions loaded\n")
} else {
  stop("videogameinsightsR package functions not found")
}

# Check API key
api_key <- Sys.getenv("VGI_AUTH_TOKEN")
if (api_key == "") {
  stop("VGI_AUTH_TOKEN not set")
}

cat("\n=== Testing Year-over-Year Comparison Functionality ===\n\n")

# Test 1: Month-based comparison
cat("Test 1: Q1 Comparison (Jan-Mar) for 2024 vs 2025\n")
cat(paste(rep("=", 50), collapse = ""), "\n")

q1_comparison <- tryCatch({
  vgi_game_summary_yoy(
    steam_app_ids = c(892970, 1145360),  # Valheim, Hades
    years = c(2024, 2025),
    start_month = "January",
    end_month = "March",
    metrics = c("concurrent", "revenue", "units")
  )
}, error = function(e) {
  cat("❌ Error:", e$message, "\n")
  NULL
})

if (!is.null(q1_comparison)) {
  cat("✅ Q1 comparison completed\n")
  cat("  - Period:", q1_comparison$period, "\n")
  cat("  - Years:", paste(q1_comparison$years, collapse = ", "), "\n")
  cat("  - API calls:", q1_comparison$api_calls, "\n")
  cat("\nComparison table:\n")
  print(q1_comparison$comparison_table)
}

# Test 2: Specific date range comparison
cat("\n\nTest 2: Holiday Season Comparison (Nov 15 - Jan 15)\n")
cat(paste(rep("=", 50), collapse = ""), "\n")

holiday_comparison <- tryCatch({
  vgi_game_summary_yoy(
    steam_app_ids = 892970,  # Valheim
    years = c(2023, 2024),
    start_date = "11-15",
    end_date = "01-15",
    metrics = c("concurrent", "revenue")
  )
}, error = function(e) {
  cat("❌ Error:", e$message, "\n")
  NULL
})

if (!is.null(holiday_comparison)) {
  cat("✅ Holiday comparison completed\n")
  cat("  - Period:", holiday_comparison$period, "\n")
  cat("  - Note: Period crosses year boundary\n")
  print(holiday_comparison)
}

# Test 3: Single month comparison
cat("\n\nTest 3: Single Month Comparison (July)\n")
cat(paste(rep("=", 50), collapse = ""), "\n")

july_comparison <- tryCatch({
  vgi_game_summary_yoy(
    steam_app_ids = c(1517290, 1238810),  # Battlefield games
    years = c(2024, 2025),
    start_month = "Jul",
    end_month = "Jul"
  )
}, error = function(e) {
  cat("❌ Error:", e$message, "\n")
  NULL
})

if (!is.null(july_comparison)) {
  cat("✅ July comparison completed\n")
  
  # Check if we have growth data
  if ("avg_peak_ccu_yoy_growth" %in% names(july_comparison$comparison_table)) {
    growth_data <- july_comparison$comparison_table %>%
      filter(!is.na(avg_peak_ccu_yoy_growth)) %>%
      select(name, year, avg_peak_ccu, avg_peak_ccu_yoy_growth)
    
    cat("\nYear-over-Year Growth:\n")
    print(growth_data)
  }
}

# Test 4: Data structure validation
if (!is.null(q1_comparison)) {
  cat("\n\nTest 4: Data Structure Validation\n")
  cat(paste(rep("=", 50), collapse = ""), "\n")
  
  # Check comparison table structure
  cat("Checking comparison table structure...\n")
  if (!is.null(q1_comparison$comparison_table)) {
    cat("  - Rows:", nrow(q1_comparison$comparison_table), "\n")
    cat("  - Columns:", paste(names(q1_comparison$comparison_table), collapse = ", "), "\n")
    
    # Check for growth columns
    growth_cols <- grep("_yoy_growth$", names(q1_comparison$comparison_table), value = TRUE)
    if (length(growth_cols) > 0) {
      cat("  - Growth columns found:", paste(growth_cols, collapse = ", "), "\n")
    }
  }
  
  # Check time series structure
  cat("\nChecking time series structure...\n")
  if (!is.null(q1_comparison$time_series_comparison)) {
    ts_types <- names(q1_comparison$time_series_comparison)
    cat("  - Available time series:", paste(ts_types, collapse = ", "), "\n")
    
    # Check normalized dates
    if ("concurrent" %in% ts_types && !is.null(q1_comparison$time_series_comparison$concurrent)) {
      sample_data <- head(q1_comparison$time_series_comparison$concurrent, 3)
      if ("normalized_date" %in% names(sample_data)) {
        cat("  - ✅ Normalized dates present for cross-year comparison\n")
      }
    }
  }
}

# Test 5: Edge cases
cat("\n\nTest 5: Edge Cases\n")
cat(paste(rep("=", 50), collapse = ""), "\n")

# Test with invalid month
tryCatch({
  vgi_game_summary_yoy(
    steam_app_ids = 892970,
    years = 2024,
    start_month = "InvalidMonth"
  )
  cat("❌ Should have failed with invalid month\n")
}, error = function(e) {
  cat("✅ Correctly caught invalid month error:", e$message, "\n")
})

# Test with no parameters
tryCatch({
  result <- vgi_game_summary_yoy(
    steam_app_ids = 892970,
    years = 2024
  )
  cat("✅ Defaulted to full year comparison\n")
}, warning = function(w) {
  cat("⚠️  Warning:", w$message, "\n")
})

cat("\n=== Year-over-Year Testing Complete ===\n")