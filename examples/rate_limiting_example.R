# Rate Limiting Example for videogameinsightsR
# This script demonstrates how to use rate limiting to be gentle on the API

library(videogameinsightsR)

# Method 1: Environment Variables (affects all functions)
# ======================================================

# Set global rate limiting configuration
Sys.setenv(VGI_BATCH_SIZE = "10")   # Make 10 API calls before pausing
Sys.setenv(VGI_BATCH_DELAY = "1")   # Pause 1 second between batches

# Now any function that makes multiple API calls will respect these limits
game_summary <- vgi_game_summary(
  steam_app_ids = c(892970, 1145360),
  start_date = "2024-07-01",
  end_date = "2024-07-31"  # 31 days = multiple batches
)

# Method 2: Using Rate Limiter Directly
# =====================================

# Create a custom rate limiter
limiter <- create_rate_limiter(
  calls_per_batch = 5,     # More conservative - only 5 calls per batch
  delay_seconds = 2,       # Longer pause between batches
  show_messages = TRUE     # Show when rate limiting occurs
)

# Example: Fetching data for multiple dates with manual rate limiting
dates <- seq(as.Date("2024-07-01"), as.Date("2024-07-15"), by = "day")
results <- list()

for (i in seq_along(dates)) {
  date <- dates[i]
  
  # Make API call
  daily_data <- vgi_concurrent_players_by_date(
    date = as.character(date),
    steam_app_ids = c(892970, 1145360)
  )
  
  results[[as.character(date)]] <- daily_data
  
  # Apply rate limiting
  limiter$increment()
  
  # Optional: Show progress
  if (i %% 5 == 0) {
    stats <- limiter$get_stats()
    cat(sprintf("Progress: %d/%d dates (%.1f calls/second)\n",
               i, length(dates), stats$calls_per_second))
  }
}

# Method 3: Simple Rate Limiting in Loops
# ======================================

# For simple cases, use apply_rate_limit() function
concurrent_data <- list()

for (i in seq_along(dates)) {
  date <- dates[i]
  
  # Fetch data
  daily_ccu <- vgi_concurrent_players_by_date(
    date = as.character(date),
    steam_app_ids = 892970
  )
  
  concurrent_data[[as.character(date)]] <- daily_ccu
  
  # Apply rate limiting based on position in loop
  apply_rate_limit(
    date_position = i,
    total_dates = length(dates),
    calls_per_batch = 10,
    delay_seconds = 1.5
  )
}

# Method 4: Automatic Rate Limiting Decision
# =========================================

# Let the package decide if rate limiting is needed
num_dates <- 90  # 3 months
num_metrics <- 4 # concurrent, revenue, units, active

if (should_use_rate_limiting(num_dates, num_metrics)) {
  cat("Rate limiting recommended for this request\n")
  
  # Use conservative settings for large requests
  Sys.setenv(VGI_BATCH_SIZE = "5")
  Sys.setenv(VGI_BATCH_DELAY = "2")
} else {
  cat("Rate limiting not necessary for this request\n")
}

# Best Practices
# =============

cat("\nRate Limiting Best Practices:\n")
cat("1. For < 50 API calls: No rate limiting needed\n")
cat("2. For 50-200 calls: Use default settings (10 calls/batch, 1 second delay)\n")
cat("3. For 200-500 calls: Use conservative settings (5 calls/batch, 2 second delay)\n")
cat("4. For 500+ calls: Consider breaking into smaller date ranges\n")
cat("5. For overnight/automated jobs: Use very conservative settings\n")

# Clean up environment variables
Sys.unsetenv("VGI_BATCH_SIZE")
Sys.unsetenv("VGI_BATCH_DELAY")