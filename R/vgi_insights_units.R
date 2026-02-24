#' Get Units Sold Data for a Game
#'
#' Retrieve historical units sold data for a specific game.
#'
#' @param steam_app_id Integer. The Steam App ID of the game.
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A data frame containing units sold history with columns:
#' \describe{
#'   \item{steamAppId}{Integer. The Steam App ID}
#'   \item{date}{Date. The date of the data point}
#'   \item{unitsSoldChange}{Integer. Units sold change from previous period}
#'   \item{unitsSoldTotal}{Integer. Total cumulative units sold}
#' }
#'
#' @details
#' The new API provides both incremental changes and cumulative totals for units sold.
#' This makes it easy to track both growth rates and absolute numbers.
#'
#' @export
#' @examples
#' \dontrun{
#' # Get units sold history for a game
#' units_data <- vgi_insights_units(steam_app_id = 730)
#' 
#' # Plot cumulative units sold over time
#' plot(units_data$date, units_data$unitsSoldTotal, 
#'      type = "l", main = "Total Units Sold Over Time",
#'      xlab = "Date", ylab = "Total Units")
#' 
#' # Calculate daily sales for recent period
#' recent_data <- tail(units_data, 30)
#' daily_sales <- mean(recent_data$unitsSoldChange, na.rm = TRUE)
#' print(paste("Average daily sales (last 30 days):", round(daily_sales)))
#' 
#' # Find peak sales day
#' peak_day <- units_data[which.max(units_data$unitsSoldChange), ]
#' print(paste("Peak sales:", peak_day$unitsSoldChange, "on", peak_day$date))
#' }
vgi_insights_units <- function(steam_app_id, 
                               auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                               headers = list()) {
  
  validate_numeric(steam_app_id, "steam_app_id")
  
  hist <- vgi_historical_data(steam_app_id, auth_token = auth_token, headers = headers)
  
  us <- hist$unitsSold
  if (is.null(us) || nrow(us) == 0) {
    return(data.frame(
      steamAppId = integer(), date = as.Date(character()),
      unitsSoldChange = integer(), unitsSoldTotal = integer(),
      stringsAsFactors = FALSE
    ))
  }
  
  data.frame(
    steamAppId = as.integer(steam_app_id),
    date = as.Date(us$date),
    unitsSoldChange = as.integer(us$dailyUnits),
    unitsSoldTotal = as.integer(us$unitsSold),
    stringsAsFactors = FALSE
  )
}
