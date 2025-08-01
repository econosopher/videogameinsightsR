# Year-over-Year Comparison Example
# This script demonstrates how to use the YoY comparison functionality

library(videogameinsightsR)
library(dplyr)

# Ensure API token is set
if (Sys.getenv("VGI_AUTH_TOKEN") == "") {
  stop("Please set VGI_AUTH_TOKEN environment variable")
}

# Example 1: Compare Q1 performance across years
cat("Example 1: Q1 Performance Comparison\n")
cat("===================================\n")

# Compare Q1 2024 vs Q1 2025 for popular games
q1_comparison <- vgi_game_summary_yoy(
  steam_app_ids = c(892970, 1145360),  # Valheim, Hades
  years = c(2024, 2025),
  start_month = "January",
  end_month = "March",
  metrics = c("concurrent", "revenue", "units")
)

# View the comparison summary
print(q1_comparison)

# Show year-over-year changes
cat("\nYear-over-Year Changes:\n")
yoy_changes <- q1_comparison$comparison_table %>%
  filter(!is.na(avg_peak_ccu_yoy_growth)) %>%
  select(name, year, avg_peak_ccu, avg_peak_ccu_yoy_growth,
         total_revenue, total_revenue_yoy_growth)

print(yoy_changes)

# Example 2: Holiday season comparison
cat("\n\nExample 2: Holiday Season Comparison\n")
cat("====================================\n")

# Compare holiday seasons (crosses year boundary)
holiday_comparison <- vgi_game_summary_yoy(
  steam_app_ids = c(1517290, 1238810),  # Battlefield 2042, Battlefield V
  years = c(2023, 2024),
  start_date = "11-15",  # November 15
  end_date = "01-15",    # January 15 (next year)
  metrics = c("concurrent", "revenue")
)

cat("Holiday period:", holiday_comparison$period, "\n")
cat("Note: This period crosses year boundaries\n\n")

# Display comparison
print(holiday_comparison$comparison_table)

# Example 3: Monthly comparison
cat("\n\nExample 3: July Performance Analysis\n")
cat("===================================\n")

july_comparison <- vgi_game_summary_yoy(
  steam_app_ids = c(892970, 1145360, 1517290),
  years = c(2023, 2024, 2025),
  start_month = "July",
  end_month = "July"
)

# Show growth trends
growth_data <- july_comparison$comparison_table %>%
  filter(!is.na(total_revenue_yoy_growth)) %>%
  select(name, year, total_revenue, total_revenue_yoy_growth,
         total_units, total_units_yoy_growth) %>%
  arrange(name, year)

cat("\nYear-over-Year Growth Trends:\n")
print(growth_data)

# Example 4: Working with the time series data
cat("\n\nExample 4: Analyzing Time Series Data\n")
cat("====================================\n")

# Access the normalized time series data
if (!is.null(july_comparison$time_series_comparison$concurrent)) {
  ccu_data <- july_comparison$time_series_comparison$concurrent
  
  # Calculate average CCU by game and year
  avg_ccu_summary <- ccu_data %>%
    group_by(steamAppId, year) %>%
    summarise(
      avg_peak = mean(peakConcurrent, na.rm = TRUE),
      max_peak = max(peakConcurrent, na.rm = TRUE),
      min_peak = min(peakConcurrent, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(steamAppId, year)
  
  cat("\nConcurrent User Statistics by Year:\n")
  print(avg_ccu_summary)
}

# Example 5: Full year comparison
cat("\n\nExample 5: Full Year Comparison\n")
cat("===============================\n")

# Compare full years
full_year <- vgi_game_summary_yoy(
  steam_app_ids = 892970,  # Valheim
  years = c(2023, 2024),
  start_month = "January",
  end_month = "December"
)

cat("Period analyzed:", full_year$period, "\n")
cat("Total API calls made:", full_year$api_calls, "\n\n")

# Show summary
summary_data <- full_year$comparison_table %>%
  select(year, avg_peak_ccu, total_revenue, total_units,
         avg_peak_ccu_yoy_growth, total_revenue_yoy_growth)

print(summary_data)

cat("\n✅ Year-over-Year comparison examples completed!\n")
cat("\nNote: You can use the returned data with your preferred visualization library\n")
cat("(ggplot2, plotly, etc.) to create custom charts and graphs.\n")