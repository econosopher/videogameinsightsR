#' Get Game Rankings
#'
#' Retrieve rankings for all games across various metrics including reviews, revenue,
#' units sold, followers, and playtime.
#'
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A data frame containing rankings for each game with columns:
#' \describe{
#'   \item{steamAppId}{Integer. The Steam App ID}
#'   \item{positiveReviewsRank}{Integer. Rank by positive reviews}
#'   \item{positiveReviewsPrct}{Numeric. Percentile for positive reviews}
#'   \item{totalRevenueRank}{Integer. Rank by total revenue}
#'   \item{totalRevenuePrct}{Numeric. Percentile for total revenue}
#'   \item{totalUnitsSoldRank}{Integer. Rank by total units sold}
#'   \item{totalUnitsSoldPrct}{Numeric. Percentile for total units sold}
#'   \item{yesterdayUnitsSoldRank}{Integer. Rank by yesterday's units sold}
#'   \item{yesterdayUnitsSoldPrct}{Numeric. Percentile for yesterday's units sold}
#'   \item{followersRank}{Integer. Rank by follower count}
#'   \item{followersPrct}{Numeric. Percentile for followers}
#'   \item{avgPlaytimeRank}{Integer. Rank by average playtime}
#'   \item{avgPlaytimePrct}{Numeric. Percentile for average playtime}
#' }
#'
#' @details
#' Rankings provide a comprehensive view of game performance across metrics:
#' \itemize{
#'   \item Lower rank numbers indicate better performance (1 = best)
#'   \item Percentiles show the percentage of games that rank below
#'   \item Use multiple metrics to get a balanced view of game success
#'   \item Recent sales rankings help identify trending games
#' }
#'
#' @export
#' @examples
#' \dontrun{
#' # Get all game rankings
#' rankings <- vgi_game_rankings()
#' 
#' # Find top 10 games by revenue
#' top_revenue <- head(rankings[order(rankings$totalRevenueRank), ], 10)
#' print(top_revenue[, c("steamAppId", "totalRevenueRank", "totalRevenuePrct")])
#' 
#' # Find games that rank well across multiple metrics
#' # (top 100 in both revenue and reviews)
#' top_overall <- rankings[
#'   rankings$totalRevenueRank <= 100 & 
#'   rankings$positiveReviewsRank <= 100, 
#' ]
#' print(paste("Games in top 100 for both revenue and reviews:", nrow(top_overall)))
#' 
#' # Identify trending games (high recent sales relative to total)
#' rankings$trending_score <- rankings$totalUnitsSoldRank / rankings$yesterdayUnitsSoldRank
#' trending <- head(rankings[order(rankings$trending_score, decreasing = TRUE), ], 20)
#' 
#' # Create a scatter plot of revenue vs reviews rankings
#' plot(rankings$totalRevenueRank, rankings$positiveReviewsRank,
#'      pch = 19, col = rgb(0, 0, 1, 0.1),
#'      xlab = "Revenue Rank", ylab = "Reviews Rank",
#'      main = "Game Rankings: Revenue vs Reviews")
#' abline(a = 0, b = 1, col = "red", lty = 2)
#' 
#' # Find hidden gems (great reviews but lower revenue)
#' hidden_gems <- rankings[
#'   rankings$positiveReviewsRank <= 50 & 
#'   rankings$totalRevenueRank > 200,
#' ]
#' print(paste("Hidden gems found:", nrow(hidden_gems)))
#' }
vgi_game_rankings <- function(auth_token = Sys.getenv("VGI_AUTH_TOKEN"), headers = list()) {
  
  # Make API request
  result <- make_api_request(
    endpoint = "games/rankings",
    auth_token = auth_token,
    method = "GET",
    headers = headers
  )
  
  # The result should be a data frame from jsonlite
  if (is.data.frame(result) && nrow(result) > 0) {
    # Already a data frame, just ensure column types are correct
    rankings_df <- result
    
    # Ensure numeric columns are properly typed
    numeric_cols <- c("positiveReviewsPrct", "totalRevenuePrct", "totalUnitsSoldPrct", 
                      "yesterdayUnitsSoldPrct", "followersPrct", "avgPlaytimePrct")
    for (col in numeric_cols) {
      if (col %in% names(rankings_df)) {
        rankings_df[[col]] <- as.numeric(rankings_df[[col]])
      }
    }
    
    # Ensure integer columns are properly typed
    int_cols <- c("steamAppId", "positiveReviewsRank", "totalRevenueRank", 
                  "totalUnitsSoldRank", "yesterdayUnitsSoldRank", 
                  "followersRank", "avgPlaytimeRank")
    for (col in int_cols) {
      if (col %in% names(rankings_df)) {
        rankings_df[[col]] <- as.integer(rankings_df[[col]])
      }
    }
    
    # Sort by total revenue rank by default (handle NAs)
    rankings_df <- rankings_df[order(rankings_df$totalRevenueRank, na.last = TRUE), ]
    
    return(rankings_df)
  } else {
    # Return empty data frame with correct structure
    return(data.frame(
      steamAppId = integer(),
      positiveReviewsRank = integer(),
      positiveReviewsPrct = numeric(),
      totalRevenueRank = integer(),
      totalRevenuePrct = numeric(),
      totalUnitsSoldRank = integer(),
      totalUnitsSoldPrct = numeric(),
      yesterdayUnitsSoldRank = integer(),
      yesterdayUnitsSoldPrct = numeric(),
      followersRank = integer(),
      followersPrct = numeric(),
      avgPlaytimeRank = integer(),
      avgPlaytimePrct = numeric(),
      stringsAsFactors = FALSE
    ))
  }
}