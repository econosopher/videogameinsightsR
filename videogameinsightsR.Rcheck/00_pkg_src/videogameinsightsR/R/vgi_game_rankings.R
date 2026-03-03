#' Get Game Rankings
#'
#' Retrieve rankings for games across various metrics including reviews, revenue,
#' units sold, followers, and playtime.
#'
#' @param offset Integer. The number of records to skip for pagination. Optional.
#' @param limit Integer. Maximum number of results to return. Without specifying 
#'   limit you will receive 5 results. The maximum limit is 1000. Optional.
#' @param date Character string or Date. Snapshot date to build rankings from.
#'   Defaults to 7 days ago to align with data availability lag.
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
#' Note: The API currently returns a limited set of games. Filtering by genre,
#' platform, or date is not supported by the API endpoint.
#'
#' @export
#' @examples
#' \dontrun{
#' # Get default rankings (5 games)
#' rankings <- vgi_game_rankings()
#' 
#' # Get top 100 games
#' rankings <- vgi_game_rankings(limit = 100)
#' 
#' # Get games starting from offset 50
#' rankings <- vgi_game_rankings(offset = 50, limit = 20)
#' 
#' # Find top 10 games by revenue
#' rankings <- vgi_game_rankings(limit = 100)
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
vgi_game_rankings <- function(offset = NULL,
                             limit = NULL,
                             date = Sys.Date() - 7,
                             auth_token = Sys.getenv("VGI_AUTH_TOKEN"), 
                             headers = list()) {
  
  # Validate inputs
  if (!is.null(offset)) {
    validate_numeric(offset, "offset", min_val = 0)
  }
  
  if (!is.null(limit)) {
    validate_numeric(limit, "limit", min_val = 1, max_val = 1000)
  }
  formatted_date <- format_date(date)

  # v4 no longer exposes /games/rankings; derive rankings from historical snapshot.
  fetch_limit <- if (is.null(limit)) 1000 else as.integer(limit + (offset %||% 0))
  result <- .vgi_historical_results(
    date = formatted_date,
    limit = fetch_limit,
    auth_token = auth_token,
    headers = headers
  )

  if (!is.data.frame(result) || nrow(result) == 0) {
    return(.vgi_clean_names(data.frame(
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
      totalRevenue = numeric(),
      totalUnitsSold = numeric(),
      yesterdayUnitsSold = numeric(),
      followers = numeric(),
      avgPlaytime = numeric(),
      stringsAsFactors = FALSE
    )))
  }

  rankings_df <- result
  if ("platform" %in% names(rankings_df)) {
    steam_rows <- rankings_df$platform == "steam"
    if (any(steam_rows, na.rm = TRUE)) {
      rankings_df <- rankings_df[steam_rows, , drop = FALSE]
    }
  }

  if (!"externalId" %in% names(rankings_df)) {
    stop("historical-data response is missing 'externalId' for Steam ID mapping.")
  }
  rankings_df$steamAppId <- suppressWarnings(as.integer(rankings_df$externalId))
  rankings_df <- rankings_df[!is.na(rankings_df$steamAppId), , drop = FALSE]
  if (nrow(rankings_df) == 0) {
    return(.vgi_clean_names(data.frame(
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
      totalRevenue = numeric(),
      totalUnitsSold = numeric(),
      yesterdayUnitsSold = numeric(),
      followers = numeric(),
      avgPlaytime = numeric(),
      stringsAsFactors = FALSE
    )))
  }

  rankings_df$totalRevenue <- as.numeric(rankings_df$revenueTotal %||% NA)
  rankings_df$totalUnitsSold <- as.numeric(rankings_df$unitsSoldTotal %||% NA)
  rankings_df$yesterdayUnitsSold <- as.numeric(rankings_df$unitsSoldChange %||% NA)
  rankings_df$followers <- as.numeric(rankings_df$followersTotal %||% NA)
  rankings_df$avgPlaytime <- as.numeric(rankings_df$ccuAvg %||% NA)
  rankings_df$positiveReviews <- as.numeric(rankings_df$positiveReviewsTotal %||% NA)

  add_rank_cols <- function(df, value_col, rank_col, prct_col) {
    values <- suppressWarnings(as.numeric(df[[value_col]]))
    ranks <- rank(-values, ties.method = "min", na.last = "keep")
    denom <- max(1, sum(!is.na(values)) - 1)
    prct <- ifelse(is.na(ranks), NA_real_, (as.numeric(ranks) - 1) / denom * 100)
    df[[rank_col]] <- as.integer(ranks)
    df[[prct_col]] <- as.numeric(prct)
    df
  }

  rankings_df <- add_rank_cols(rankings_df, "positiveReviews", "positiveReviewsRank", "positiveReviewsPrct")
  rankings_df <- add_rank_cols(rankings_df, "totalRevenue", "totalRevenueRank", "totalRevenuePrct")
  rankings_df <- add_rank_cols(rankings_df, "totalUnitsSold", "totalUnitsSoldRank", "totalUnitsSoldPrct")
  rankings_df <- add_rank_cols(rankings_df, "yesterdayUnitsSold", "yesterdayUnitsSoldRank", "yesterdayUnitsSoldPrct")
  rankings_df <- add_rank_cols(rankings_df, "followers", "followersRank", "followersPrct")
  rankings_df <- add_rank_cols(rankings_df, "avgPlaytime", "avgPlaytimeRank", "avgPlaytimePrct")

  rankings_df <- rankings_df[order(rankings_df$totalRevenueRank, na.last = TRUE), , drop = FALSE]
  if (!is.null(offset) && offset > 0 && nrow(rankings_df) > offset) {
    rankings_df <- rankings_df[(offset + 1):nrow(rankings_df), , drop = FALSE]
  } else if (!is.null(offset) && offset >= nrow(rankings_df)) {
    rankings_df <- rankings_df[0, , drop = FALSE]
  }
  if (!is.null(limit) && nrow(rankings_df) > limit) {
    rankings_df <- rankings_df[seq_len(limit), , drop = FALSE]
  }

  warn_if_stale_ids(rankings_df$steamAppId)
  .vgi_clean_names(rankings_df)
}