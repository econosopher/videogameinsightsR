#' Get Wishlists Data by Date
#'
#' Retrieve wishlist counts for all games on a specific date, useful for
#' tracking market-wide interest and anticipation trends.
#'
#' @param date Character string or Date. The date for which to retrieve data
#'   in "YYYY-MM-DD" format.
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{steamAppId}{Integer. The Steam App ID}
#'   \item{date}{Character. The date of the data}
#'   \item{wishlistCount}{Integer. Number of wishlists}
#'   \item{wishlistRank}{Integer. Rank by wishlist count}
#' }
#'
#' @details
#' This endpoint enables:
#' \itemize{
#'   \item Market-wide wishlist trend analysis
#'   \item Identifying rising games before launch
#'   \item Tracking pre-release momentum
#'   \item Competitive wishlist benchmarking
#'   \item Seasonal wishlist pattern analysis
#' }
#' 
#' Wishlist data is particularly valuable for unreleased games
#' as it's often the primary engagement metric available.
#'
#' @export
#' @examples
#' \dontrun{
#' # Get wishlist data for a specific date
#' wishlists <- vgi_wishlists_by_date("2024-01-15")
#' 
#' # Top 20 most wishlisted games
#' top_wishlisted <- head(wishlists, 20)
#' print(top_wishlisted)
#' 
#' # Track weekly wishlist changes
#' week_ago <- as.Date("2024-01-15") - 7
#' wishlists_prev <- vgi_wishlists_by_date(as.character(week_ago))
#' 
#' # Calculate weekly growth
#' growth <- merge(wishlists, wishlists_prev, 
#'                by = "steamAppId", 
#'                suffixes = c("_now", "_prev"))
#' growth$weekly_change <- growth$wishlistCount_now - growth$wishlistCount_prev
#' growth$weekly_pct <- (growth$weekly_change / growth$wishlistCount_prev) * 100
#' 
#' # Find fastest growing games
#' min_wishlists <- 1000  # Only games with substantial wishlists
#' qualified <- growth[growth$wishlistCount_prev >= min_wishlists, ]
#' fastest_growing <- head(qualified[order(-qualified$weekly_pct), ], 20)
#' 
#' cat("Fastest growing games (>1000 wishlists):\n")
#' print(fastest_growing[, c("steamAppId", "wishlistCount_now", 
#'                          "weekly_change", "weekly_pct")])
#' 
#' # Analyze wishlist distribution
#' hist(log10(wishlists$wishlistCount + 1),
#'      breaks = 30,
#'      main = "Distribution of Wishlist Counts (log scale)",
#'      xlab = "Log10(Wishlist Count + 1)",
#'      col = "lightgreen")
#' 
#' # Find games losing wishlists
#' declining <- growth[growth$weekly_change < -100, ]
#' cat("Games losing 100+ wishlists this week:", nrow(declining), "\n")
#' 
#' # Seasonal analysis (if you have historical data)
#' # Compare with same date last year
#' last_year <- as.Date("2024-01-15") - 365
#' wishlists_ly <- vgi_wishlists_by_date(as.character(last_year))
#' 
#' yoy <- merge(wishlists, wishlists_ly,
#'             by = "steamAppId",
#'             suffixes = c("_now", "_lastyear"))
#' yoy$yoy_growth <- ((yoy$wishlistCount_now - yoy$wishlistCount_lastyear) / 
#'                   yoy$wishlistCount_lastyear) * 100
#' 
#' # Find games with massive YoY growth
#' breakout <- yoy[yoy$yoy_growth > 1000 & yoy$wishlistCount_now > 10000, ]
#' cat("Breakout games (>1000% YoY growth, >10k wishlists):", nrow(breakout), "\n")
#' }
vgi_wishlists_by_date <- function(date,
                                 auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                                 headers = list()) {
  
  # Validate and format date
  formatted_date <- format_date(date)
  
  # Make API request
  result <- make_api_request(
    endpoint = paste0("interest-level/wishlists/", formatted_date),
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
        wishlistCount = as.integer(x$wishlistCount %||% 0),
        stringsAsFactors = FALSE
      )
    }))
    
    # Sort by wishlist count descending and add rank
    df <- df[order(-df$wishlistCount), ]
    df$wishlistRank <- seq_len(nrow(df))
    
    return(df)
  } else {
    # Return empty data frame with correct structure
    return(data.frame(
      steamAppId = integer(),
      date = character(),
      wishlistCount = integer(),
      wishlistRank = integer(),
      stringsAsFactors = FALSE
    ))
  }
}