# Example usage of videogameinsightsR package

# Load the package
library(videogameinsightsR)

# Ensure you have set your API token
# Sys.setenv(VGI_AUTH_TOKEN = "your_token_here")

# Example 1: Get metadata for a single game
cat("Example 1: Getting metadata for Valheim\n")
valheim <- vgi_game_metadata(892970)
print(valheim)

# Example 2: Get metadata for multiple games at once
cat("\nExample 2: Getting metadata for multiple games\n")
game_ids <- c(892970, 1245620, 105600)  # Valheim, Elden Ring, Terraria
games_batch <- vgi_game_metadata_batch(game_ids)
print(games_batch)

# Example 3: Search for games
cat("\nExample 3: Searching for RPG games\n")
rpg_games <- vgi_search_games("rpg", limit = 20)
print(rpg_games)

# Example 4: Get top games by revenue
cat("\nExample 4: Top 10 games by revenue\n")
top_revenue <- vgi_top_games("revenue", limit = 10)
print(top_revenue)

# Example 5: Get top Steam games by CCU
cat("\nExample 5: Top Steam games by concurrent users\n")
top_ccu <- vgi_top_games(
  metric = "ccu",
  platform = "steam",
  limit = 25
)
print(top_ccu)

# Example 6: Get top games for a specific date range
cat("\nExample 6: Top games by units sold in January 2024\n")
top_january <- vgi_top_games(
  metric = "units",
  start_date = "2024-01-01",
  end_date = "2024-01-31",
  limit = 15
)
print(top_january)