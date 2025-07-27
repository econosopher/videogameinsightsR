#' Get Playtime Data for All Games
#'
#' Retrieve playtime statistics for all games in the database, providing
#' a comprehensive view of player engagement across the market.
#'
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{steamAppId}{Integer. The Steam App ID}
#'   \item{avgPlaytime}{Numeric. Average playtime in hours}
#'   \item{medianPlaytime}{Numeric. Median playtime in hours}
#'   \item{totalPlaytime}{Numeric. Total playtime across all players in hours}
#'   \item{playtimeRank}{Integer. Rank by average playtime}
#' }
#'
#' @details
#' Playtime data is crucial for understanding:
#' \itemize{
#'   \item Game engagement and stickiness
#'   \item Content depth and replayability
#'   \item Player satisfaction (high playtime often = high satisfaction)
#'   \item Genre-specific engagement patterns
#'   \item Value proposition (playtime per dollar)
#' }
#' 
#' Average playtime can be skewed by dedicated players, while median
#' provides a better sense of typical player engagement.
#'
#' @export
#' @examples
#' \dontrun{
#' # Get playtime data for all games
#' playtime_data <- vgi_all_games_playtime()
#' 
#' # Top 20 most played games by average playtime
#' top_played <- head(playtime_data, 20)
#' cat("Top 20 games by average playtime:\n")
#' print(top_played[, c("steamAppId", "avgPlaytime", "medianPlaytime")])
#' 
#' # Find games with high engagement
#' high_engagement <- playtime_data[playtime_data$avgPlaytime > 100, ]
#' cat("Games with >100 hours average playtime:", nrow(high_engagement), "\n")
#' 
#' # Analyze playtime distribution
#' hist(log10(playtime_data$avgPlaytime + 1),
#'      breaks = 40,
#'      main = "Distribution of Average Playtime (log scale)",
#'      xlab = "Log10(Avg Playtime + 1)",
#'      col = "orange")
#' 
#' # Compare average vs median to find games with dedicated players
#' playtime_data$avg_median_ratio <- playtime_data$avgPlaytime / 
#'                                   (playtime_data$medianPlaytime + 0.1)
#' 
#' # Games where average is much higher than median (cult followings)
#' cult_games <- playtime_data[playtime_data$avg_median_ratio > 5 & 
#'                            playtime_data$avgPlaytime > 20, ]
#' cat("Games with cult followings (avg >> median):", nrow(cult_games), "\n")
#' 
#' # Combine with revenue data for value analysis
#' revenue_data <- vgi_revenue_by_date(Sys.Date() - 1)
#' value_analysis <- merge(playtime_data, revenue_data, by = "steamAppId")
#' 
#' # Calculate hours per dollar (value metric)
#' units_data <- vgi_units_sold_by_date(Sys.Date() - 1)
#' value_analysis <- merge(value_analysis, units_data, by = "steamAppId")
#' value_analysis$avg_price <- value_analysis$revenue / 
#'                            (value_analysis$unitsSold + 1)
#' value_analysis$hours_per_dollar <- value_analysis$avgPlaytime / 
#'                                    (value_analysis$avg_price + 0.01)
#' 
#' # Best value games (high playtime, reasonable price)
#' best_value <- value_analysis[value_analysis$hours_per_dollar > 2 & 
#'                             value_analysis$avg_price < 60 &
#'                             value_analysis$unitsSold > 10000, ]
#' best_value <- head(best_value[order(-best_value$hours_per_dollar), ], 20)
#' cat("Best value games (hours per dollar):\n")
#' print(best_value[, c("steamAppId", "avgPlaytime", "avg_price", 
#'                     "hours_per_dollar")])
#' 
#' # Genre analysis (would need genre data)
#' # Multiplayer games typically have higher playtime
#' likely_multiplayer <- playtime_data[playtime_data$avgPlaytime > 50 & 
#'                                    playtime_data$medianPlaytime > 20, ]
#' cat("Likely multiplayer/service games:", nrow(likely_multiplayer), "\n")
#' }
vgi_all_games_playtime <- function(auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                                  headers = list()) {
  
  # Make API request
  result <- make_api_request(
    endpoint = "player-insights/games/playtime",
    auth_token = auth_token,
    method = "GET",
    headers = headers
  )
  
  # Convert to data frame
  if (is.list(result) && length(result) > 0) {
    df <- do.call(rbind, lapply(result, function(x) {
      data.frame(
        steamAppId = as.integer(x$steamAppId),
        avgPlaytime = as.numeric(x$avgPlaytime %||% 0),
        medianPlaytime = as.numeric(x$medianPlaytime %||% 0),
        totalPlaytime = as.numeric(x$totalPlaytime %||% 0),
        stringsAsFactors = FALSE
      )
    }))
    
    # Sort by average playtime descending and add rank
    df <- df[order(-df$avgPlaytime), ]
    df$playtimeRank <- seq_len(nrow(df))
    
    return(df)
  } else {
    # Return empty data frame with correct structure
    return(data.frame(
      steamAppId = integer(),
      avgPlaytime = numeric(),
      medianPlaytime = numeric(),
      totalPlaytime = numeric(),
      playtimeRank = integer(),
      stringsAsFactors = FALSE
    ))
  }
}