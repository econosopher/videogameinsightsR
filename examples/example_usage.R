# videogameinsightsR Example Usage
# This script demonstrates the key functionality of the videogameinsightsR package

# Load the package
library(videogameinsightsR)
library(ggplot2)
library(dplyr)

# Set your API token (get one from https://vginsights.com)
# Sys.setenv(VGI_AUTH_TOKEN = "your_token_here")

# ==========================================
# 1. BASIC GAME INFORMATION
# ==========================================

# Get metadata for a specific game (by Steam App ID)
game_info <- vgi_game_metadata(730)  # Counter-Strike 2
print(game_info)

# Search for games by name
rpg_games <- vgi_search_games("final fantasy", limit = 10)
print(rpg_games)

# Get the complete list of all games
all_games <- vgi_game_list()
cat("Total games in database:", nrow(all_games), "\n")

# ==========================================
# 2. RANKINGS AND TOP GAMES
# ==========================================

# Get top games by revenue
top_revenue <- vgi_top_games("revenue", limit = 10)
print(top_revenue)

# Get top games by concurrent users
top_ccu <- vgi_top_games("ccu", limit = 10)
print(top_ccu)

# Get comprehensive game rankings
rankings <- vgi_game_rankings()
# Find top 5 by total revenue
top_5_revenue <- head(rankings[order(rankings$totalRevenueRank), ], 5)
print(top_5_revenue)

# ==========================================
# 3. TIME SERIES DATA
# ==========================================

# Get revenue over time for a specific game
revenue_history <- vgi_revenue_by_date(
  steam_app_id = 730,
  start_date = "2024-01-01",
  end_date = "2024-12-31"
)

# Plot revenue over time
if (nrow(revenue_history) > 0) {
  ggplot(revenue_history, aes(x = as.Date(date), y = revenue)) +
    geom_line(color = "blue", size = 1) +
    geom_point(color = "blue") +
    scale_y_continuous(labels = scales::dollar) +
    labs(title = "Counter-Strike 2 Revenue Over Time",
         x = "Date", y = "Revenue (USD)") +
    theme_minimal()
}

# Get player count history
players_history <- vgi_active_players_by_date(
  steam_app_id = 730,
  start_date = "2024-01-01",
  end_date = "2024-12-31"
)

# ==========================================
# 4. INSIGHTS AND ANALYTICS
# ==========================================

# Get detailed insights for a game
ccu_insights <- vgi_insights_ccu(730)
revenue_insights <- vgi_insights_revenue(730)
playtime_insights <- vgi_insights_playtime(730)

# Get player regions
player_regions <- vgi_insights_player_regions(730)
if (nrow(player_regions) > 0) {
  # Plot player distribution by region
  ggplot(player_regions, aes(x = reorder(region, percentage), y = percentage)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    labs(title = "Player Distribution by Region",
         x = "Region", y = "Percentage") +
    theme_minimal()
}

# ==========================================
# 5. DEVELOPER AND PUBLISHER DATA
# ==========================================

# Get list of all developers
all_developers <- vgi_developer_list()
cat("Total developers:", nrow(all_developers), "\n")

# Get info about a specific developer
valve_info <- vgi_developer_info(12)  # Valve
print(valve_info)

# Get all games by a developer
valve_games <- vgi_developer_games(12)
print(valve_games)

# Get publisher information
publisher_info <- vgi_publisher_info(12)  # Valve as publisher
publisher_games <- vgi_publisher_games(12)

# ==========================================
# 6. BATCH OPERATIONS
# ==========================================

# Get metadata for multiple games at once
game_ids <- c(730, 570, 440)  # CS2, Dota 2, TF2
batch_metadata <- vgi_game_metadata_batch(game_ids)
print(batch_metadata)

# Get all games metadata (warning: large dataset)
# all_games_meta <- vgi_all_games_metadata()

# ==========================================
# 7. ADVANCED ANALYTICS
# ==========================================

# Analyze player overlap between games
overlap <- vgi_player_overlap(730, 570)  # CS2 and Dota 2
print(overlap)

# Get top countries for a game
top_countries <- vgi_top_countries(730)
print(top_countries)

# Get wishlist data by country
wishlist_countries <- vgi_top_wishlist_countries(730)
print(wishlist_countries)

# ==========================================
# 8. CUSTOM HEADERS EXAMPLE
# ==========================================

# You can add custom headers to any API request
custom_game_info <- vgi_game_metadata(
  steam_app_id = 730,
  headers = list(
    "X-Custom-Header" = "my-value",
    "User-Agent" = "my-app/1.0"
  )
)

# ==========================================
# 9. ERROR HANDLING
# ==========================================

# The package handles errors gracefully
tryCatch({
  # Try to get data for a non-existent game
  bad_game <- vgi_game_metadata(99999999)
}, error = function(e) {
  cat("Error caught:", e$message, "\n")
})

# ==========================================
# 10. DATA EXPORT
# ==========================================

# Export data for further analysis
write.csv(top_revenue, "top_games_by_revenue.csv", row.names = FALSE)
saveRDS(rankings, "game_rankings.rds")

cat("\nExample completed successfully!\n")