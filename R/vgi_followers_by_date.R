#' Get Followers Data by Date
#'
#' Retrieve follower counts for all games on a specific date, useful for
#' tracking market-wide community engagement trends.
#'
#' @param date Character string or Date. The date for which to retrieve data
#'   in "YYYY-MM-DD" format.
#' @param steam_app_ids Numeric vector. Optional. Steam App IDs to filter results.
#'   If not provided, returns data for all available games.
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{steamAppId}{Integer. The Steam App ID}
#'   \item{date}{Character. The date of the data}
#'   \item{followerCount}{Integer. Number of followers}
#'   \item{followerRank}{Integer. Rank by follower count}
#' }
#'
#' @details
#' Steam followers represent users who want to stay updated about a game.
#' This metric is valuable for:
#' \itemize{
#'   \item Measuring long-term community engagement
#'   \item Tracking marketing campaign effectiveness
#'   \item Identifying games with growing communities
#'   \item Benchmarking community size across genres
#'   \item Early warning for declining interest
#' }
#' 
#' Unlike wishlists, followers persist after game purchase, making
#' this a good metric for both released and unreleased games.
#'
#' @export
#' @examples
#' \dontrun{
#' # Get follower data for a specific date
#' followers <- vgi_followers_by_date("2024-01-15")
#' 
#' # Top 20 most followed games
#' top_followed <- head(followers, 20)
#' print(top_followed)
#' 
#' # Compare with wishlist data to find engagement patterns
#' wishlists <- vgi_wishlists_by_date("2024-01-15")
#' engagement <- merge(followers, wishlists, by = "steamAppId")
#' engagement$follow_to_wishlist_ratio <- 
#'   engagement$followerCount / (engagement$wishlistCount + 1)
#' 
#' # Games with high follower/wishlist ratio (strong community)
#' high_engagement <- engagement[engagement$follow_to_wishlist_ratio > 2 & 
#'                               engagement$followerCount > 10000, ]
#' cat("High community engagement games:", nrow(high_engagement), "\n")
#' 
#' # Monthly follower growth analysis
#' month_ago <- as.Date("2024-01-15") - 30
#' followers_prev <- vgi_followers_by_date(as.character(month_ago))
#' 
#' growth <- merge(followers, followers_prev,
#'                by = "steamAppId",
#'                suffixes = c("_now", "_prev"))
#' growth$monthly_change <- growth$followerCount_now - growth$followerCount_prev
#' growth$monthly_pct <- (growth$monthly_change / growth$followerCount_prev) * 100
#' 
#' # Fastest growing communities (min 5000 followers)
#' qualified <- growth[growth$followerCount_prev >= 5000, ]
#' fastest <- head(qualified[order(-qualified$monthly_pct), ], 20)
#' 
#' cat("Fastest growing communities (>5000 followers):\n")
#' print(fastest[, c("steamAppId", "followerCount_now", 
#'                  "monthly_change", "monthly_pct")])
#' 
#' # Find games losing followers
#' declining <- growth[growth$monthly_change < -500, ]
#' cat("Games losing 500+ followers this month:", nrow(declining), "\n")
#' 
#' # Analyze follower distribution by tier
#' follower_tiers <- cut(followers$followerCount,
#'                      breaks = c(0, 1000, 10000, 50000, 100000, Inf),
#'                      labels = c("<1K", "1K-10K", "10K-50K", "50K-100K", ">100K"))
#' tier_summary <- table(follower_tiers)
#' 
#' barplot(tier_summary,
#'         main = "Distribution of Games by Follower Count",
#'         xlab = "Follower Tier",
#'         ylab = "Number of Games",
#'         col = "skyblue")
#' }
vgi_followers_by_date <- function(date,
                                 steam_app_ids = NULL,
                                 auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                                 headers = list()) {
  
  # Validate and format date
  formatted_date <- format_date(date)
  
  # Build query parameters if steam_app_ids provided
  query_params <- list()
  if (!is.null(steam_app_ids)) {
    # Ensure numeric and convert to comma-separated string
    steam_app_ids <- as.numeric(steam_app_ids)
    ids_string <- paste(steam_app_ids, collapse = ",")
    query_params$steamAppIds <- ids_string
  }
  
  # Make API request
  result <- make_api_request(
    endpoint = paste0("interest-level/followers/", formatted_date),
    query_params = query_params,
    auth_token = auth_token,
    method = "GET",
    headers = headers
  )
  
  # Convert to data frame
  if (is.list(result) && length(result) > 0) {
    df <- do.call(rbind, lapply(result, function(x) {
      data.frame(
        steamAppId = as.integer(x$steamAppId),
        date = formatted_date,
        followerCount = as.integer(x$followerCount %||% 0),
        stringsAsFactors = FALSE
      )
    }))
    
    # Sort by follower count descending and add rank
    df <- df[order(-df$followerCount), ]
    df$followerRank <- seq_len(nrow(df))
    
    return(df)
  } else {
    # Return empty data frame with correct structure
    return(data.frame(
      steamAppId = integer(),
      date = character(),
      followerCount = integer(),
      followerRank = integer(),
      stringsAsFactors = FALSE
    ))
  }
}