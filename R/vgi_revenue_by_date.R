#' Get Revenue Data by Date
#'
#' Retrieve revenue data for all games on a specific date, providing
#' a comprehensive view of market financial performance.
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
#'   \item{revenue}{Numeric. Cumulative revenue in USD as of this date}
#'   \item{dailyRevenue}{Numeric. Revenue generated on this specific day}
#'   \item{revenueRank}{Integer. Rank by total revenue}
#' }
#'
#' @details
#' Revenue data is the most important commercial metric for:
#' \itemize{
#'   \item Market size estimation
#'   \item Financial performance benchmarking
#'   \item ROI analysis
#'   \item Publisher/developer valuations
#'   \item Investment decisions
#' }
#' 
#' All revenue figures are in USD and represent gross revenue
#' before platform fees and taxes.
#'
#' @export
#' @examples
#' \dontrun{
#' # Get revenue data for a specific date
#' revenue_data <- vgi_revenue_by_date("2024-01-15")
#' 
#' # Top 20 highest-grossing games
#' top_revenue <- head(revenue_data, 20)
#' cat("Top 20 games by revenue:\n")
#' print(top_revenue[, c("steamAppId", "revenue", "dailyRevenue")])
#' 
#' # Format revenue for display
#' top_revenue$revenue_millions <- round(top_revenue$revenue / 1000000, 2)
#' cat("Top game revenue: $", top_revenue$revenue_millions[1], "M\n", sep = "")
#' 
#' # Calculate market concentration
#' total_revenue <- sum(revenue_data$revenue)
#' top_10_revenue <- sum(head(revenue_data$revenue, 10))
#' concentration <- (top_10_revenue / total_revenue) * 100
#' cat(sprintf("Top 10 games represent %.1f%% of total revenue\n", concentration))
#' 
#' # Daily revenue leaders
#' prev_date <- as.Date("2024-01-15") - 1
#' revenue_prev <- vgi_revenue_by_date(as.character(prev_date))
#' 
#' daily_rev <- merge(revenue_data, revenue_prev,
#'                   by = "steamAppId",
#'                   suffixes = c("_today", "_yesterday"))
#' daily_rev$revenue_today <- daily_rev$revenue_today - daily_rev$revenue_yesterday
#' 
#' # Games with highest daily revenue
#' top_daily_rev <- head(daily_rev[order(-daily_rev$revenue_today), ], 20)
#' cat("Top daily revenue generators:\n")
#' top_daily_rev$daily_rev_k <- round(top_daily_rev$revenue_today / 1000, 1)
#' print(top_daily_rev[, c("steamAppId", "daily_rev_k")])
#' 
#' # Revenue distribution analysis
#' revenue_tiers <- cut(revenue_data$revenue,
#'                     breaks = c(0, 10000, 100000, 1000000, 10000000, Inf),
#'                     labels = c("<$10K", "$10K-100K", "$100K-1M", 
#'                               "$1M-10M", ">$10M"))
#' tier_summary <- table(revenue_tiers)
#' 
#' barplot(tier_summary,
#'         main = "Games by Revenue Tier",
#'         xlab = "Revenue Tier",
#'         ylab = "Number of Games",
#'         col = "gold")
#' 
#' # Year-over-year growth analysis
#' last_year <- as.Date("2024-01-15") - 365
#' revenue_ly <- vgi_revenue_by_date(as.character(last_year))
#' 
#' yoy <- merge(revenue_data, revenue_ly,
#'             by = "steamAppId",
#'             suffixes = c("_now", "_lastyear"))
#' yoy$yoy_growth <- ((yoy$revenue_now - yoy$revenue_lastyear) / 
#'                   yoy$revenue_lastyear) * 100
#' 
#' # Fastest growing games by revenue
#' min_revenue <- 100000  # Only games with substantial revenue
#' qualified <- yoy[yoy$revenue_lastyear >= min_revenue, ]
#' fastest_growing <- head(qualified[order(-qualified$yoy_growth), ], 20)
#' 
#' cat("Fastest growing games (YoY revenue):\n")
#' print(fastest_growing[, c("steamAppId", "revenue_now", "yoy_growth")])
#' }
vgi_revenue_by_date <- function(date,
                               auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                               headers = list()) {
  
  # Validate and format date
  formatted_date <- format_date(date)
  
  # Make API request
  result <- make_api_request(
    endpoint = paste0("commercial-performance/revenue/", formatted_date),
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
        revenue = as.numeric(x$revenue %||% 0),
        dailyRevenue = as.numeric(x$dailyRevenue %||% NA),
        stringsAsFactors = FALSE
      )
    }))
    
    # Sort by revenue descending and add rank
    df <- df[order(-df$revenue), ]
    df$revenueRank <- seq_len(nrow(df))
    
    return(df)
  } else {
    # Return empty data frame with correct structure
    return(data.frame(
      steamAppId = integer(),
      date = character(),
      revenue = numeric(),
      dailyRevenue = numeric(),
      revenueRank = integer(),
      stringsAsFactors = FALSE
    ))
  }
}