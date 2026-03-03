#' Get Entitlements Data for a Game
#'
#' Retrieve historical entitlements (game ownership) data for a specific game.
#'
#' @param steam_app_id Integer. The Steam App ID of the game.
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A data frame containing entitlements history with columns:
#' \describe{
#'   \item{steamAppId}{Integer. The Steam App ID}
#'   \item{date}{Date. The date of the data point}
#'   \item{entitlementsChange}{Integer. Entitlements change from previous period}
#'   \item{entitlementsTotal}{Integer. Total cumulative entitlements}
#' }
#'
#' @details
#' The new API provides both incremental changes and cumulative totals for entitlements.
#' This makes it easy to track both growth rates and absolute numbers.
#'
#' @export
#' @examples
#' \dontrun{
#' # Get entitlements history for a game
#' entitlements_data <- vgi_insights_entitlements(steam_app_id = 730)
#' 
#' # Plot cumulative entitlements over time
#' plot(entitlements_data$date, entitlements_data$entitlementsTotal, 
#'      type = "l", main = "Total Entitlements Over Time",
#'      xlab = "Date", ylab = "Total Entitlements")
#' 
#' # Calculate daily sales for recent period
#' recent_data <- tail(entitlements_data, 30)
#' daily_sales <- mean(recent_data$entitlementsChange, na.rm = TRUE)
#' print(paste("Average daily sales (last 30 days):", round(daily_sales)))
#' 
#' # Find peak sales day
#' peak_day <- entitlements_data[which.max(entitlements_data$entitlementsChange), ]
#' print(paste("Peak entitlements:", peak_day$entitlementsChange, "on", peak_day$date))
#' }
vgi_insights_entitlements <- function(steam_app_id, 
                               auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                               headers = list()) {
  
  validate_numeric(steam_app_id, "steam_app_id")
  
  hist <- vgi_historical_data(steam_app_id, auth_token = auth_token, headers = headers)
  
  units <- hist$unitsSold
  if (is.null(units) || nrow(units) == 0) {
    return(.vgi_clean_names(data.frame(
      steamAppId = integer(), date = as.Date(character()),
      entitlementsChange = integer(), entitlementsTotal = integer(),
      stringsAsFactors = FALSE
    )))
  }
  
  .vgi_clean_names(data.frame(
    steamAppId = as.integer(steam_app_id),
    date = as.Date(units$date),
    entitlementsChange = as.integer(units$dailyUnits),
    entitlementsTotal = as.integer(units$unitsSold),
    stringsAsFactors = FALSE
  ))
}
