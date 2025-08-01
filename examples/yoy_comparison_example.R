# Year-over-Year Comparison Example
# This script demonstrates how to use the YoY comparison functionality

library(videogameinsightsR)
library(ggplot2)
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

# Create visualizations
p1 <- vgi_plot_yoy(q1_comparison, metric = "ccu", type = "line")
ggsave("q1_ccu_comparison.png", p1, width = 12, height = 8)

p2 <- vgi_plot_yoy(q1_comparison, metric = "revenue", type = "bar")
ggsave("q1_revenue_comparison.png", p2, width = 10, height = 6)

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

# Example 3: Monthly comparison with growth visualization
cat("\n\nExample 3: July Performance with Growth Rates\n")
cat("============================================\n")

july_comparison <- vgi_game_summary_yoy(
  steam_app_ids = c(892970, 1145360, 1517290),
  years = c(2023, 2024, 2025),
  start_month = "July",
  end_month = "July"
)

# Plot growth rates
if (length(july_comparison$years) > 1) {
  p3 <- vgi_plot_yoy(july_comparison, metric = "revenue", type = "growth")
  ggsave("july_revenue_growth.png", p3, width = 10, height = 8)
  cat("Growth visualization saved to july_revenue_growth.png\n")
}

# Example 4: Custom analysis with the data
cat("\n\nExample 4: Custom Analysis\n")
cat("=========================\n")

# Access the raw time series data for custom analysis
if (!is.null(july_comparison$time_series_comparison$concurrent)) {
  ccu_data <- july_comparison$time_series_comparison$concurrent
  
  # Calculate average CCU by game and year
  avg_ccu_summary <- ccu_data %>%
    group_by(steamAppId, year) %>%
    summarise(
      avg_peak = mean(peakConcurrent, na.rm = TRUE),
      max_peak = max(peakConcurrent, na.rm = TRUE),
      .groups = "drop"
    )
  
  cat("\nAverage Peak CCU by Year:\n")
  print(avg_ccu_summary)
}

cat("\n✅ Year-over-Year comparison examples completed!\n")