#' Get Units Sold Data by Date
#'
#' Retrieve units sold data for all games on a specific date, providing
#' a market-wide view of sales performance.
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
#'   \item{unitsSold}{Integer. Cumulative units sold as of this date}
#'   \item{dailyUnits}{Integer. Units sold on this specific day}
#'   \item{salesRank}{Integer. Rank by total units sold}
#' }
#'
#' @details
#' Units sold data is crucial for:
#' \itemize{
#'   \item Market share analysis
#'   \item Sales velocity tracking
#'   \item Launch performance benchmarking
#'   \item Seasonal sales pattern identification
#'   \item Competitive analysis
#' }
#' 
#' The data represents lifetime units sold through the specified date,
#' with daily units calculated from sequential dates.
#'
#' @export
#' @examples
#' \dontrun{
#' # Get units sold data for a specific date
#' units_data <- vgi_units_sold_by_date("2024-01-15")
#' 
#' # Top 20 best-selling games
#' top_sellers <- head(units_data, 20)
#' cat("Top 20 best-selling games:\n")
#' print(top_sellers[, c("steamAppId", "unitsSold", "dailyUnits")])
#' 
#' # Calculate previous day's data for daily sales
#' prev_date <- as.Date("2024-01-15") - 1
#' units_prev <- vgi_units_sold_by_date(as.character(prev_date))
#' 
#' # Merge to calculate exact daily sales
#' daily_sales <- merge(units_data, units_prev,
#'                     by = "steamAppId",
#'                     suffixes = c("_today", "_yesterday"))
#' daily_sales$units_sold_today <- daily_sales$unitsSold_today - 
#'                                 daily_sales$unitsSold_yesterday
#' 
#' # Find games with highest daily sales
#' top_daily <- head(daily_sales[order(-daily_sales$units_sold_today), ], 20)
#' cat("Top 20 games by daily sales:\n")
#' print(top_daily[, c("steamAppId", "units_sold_today")])
#' 
#' # Analyze sales distribution
#' hist(log10(units_data$unitsSold + 1),
#'      breaks = 40,
#'      main = "Distribution of Total Units Sold (log scale)",
#'      xlab = "Log10(Units Sold + 1)",
#'      col = "darkgreen")
#' 
#' # Sales velocity analysis
#' units_data$sales_per_day <- units_data$unitsSold / 
#'   as.numeric(as.Date("2024-01-15") - as.Date("2020-01-01"))
#' 
#' # Games with sustained high sales velocity
#' high_velocity <- units_data[units_data$sales_per_day > 100 & 
#'                            units_data$unitsSold > 100000, ]
#' cat("Games averaging >100 sales/day:", nrow(high_velocity), "\n")
#' 
#' # Compare with revenue data for average price calculation
#' revenue_data <- vgi_revenue_by_date("2024-01-15")
#' pricing <- merge(units_data, revenue_data, by = "steamAppId")
#' pricing$avg_price <- pricing$revenue / (pricing$unitsSold + 1)
#' 
#' # Find premium-priced successful games
#' premium_games <- pricing[pricing$avg_price > 40 & 
#'                         pricing$unitsSold > 50000, ]
#' cat("Premium games (>$40) with >50k sales:", nrow(premium_games), "\n")
#' }
vgi_units_sold_by_date <- function(date,
                                  steam_app_ids = NULL,
                                  auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                                  headers = list()) {
  
  # Validate and format date
  formatted_date <- format_date(date)
  
  steam_app_ids <- if (is.null(steam_app_ids)) NULL else as.numeric(steam_app_ids)
  fetch_limit <- if (is.null(steam_app_ids)) 2000 else max(50, length(steam_app_ids) * 5)

  result <- .vgi_historical_results(
    date = formatted_date,
    steam_app_ids = steam_app_ids,
    limit = fetch_limit,
    auth_token = auth_token,
    headers = headers
  )

  if (!is.data.frame(result) || nrow(result) == 0) {
    return(.vgi_clean_names(tibble::tibble(
      steamAppId = integer(),
      date = character(),
      unitsSold = integer(),
      dailyUnits = integer(),
      salesRank = integer(),

    )))
  }

  if ("platform" %in% names(result)) {
    result <- result[result$platform == "steam", , drop = FALSE]
  }

  df <- tibble::tibble(
    steamAppId = as.integer(result$externalId %||% NA),
    date = formatted_date,
    unitsSold = as.integer(result$unitsSoldTotal %||% NA),
    dailyUnits = as.integer(result$unitsSoldChange %||% NA),

  )
  df <- df[!is.na(df$steamAppId), , drop = FALSE]
  if (nrow(df) == 0) {
    return(.vgi_clean_names(tibble::tibble(
      steamAppId = integer(),
      date = character(),
      unitsSold = integer(),
      dailyUnits = integer(),
      salesRank = integer(),

    )))
  }

  df <- df[order(-df$unitsSold), , drop = FALSE]
  df$salesRank <- seq_len(nrow(df))
  .vgi_clean_names(df)
}
