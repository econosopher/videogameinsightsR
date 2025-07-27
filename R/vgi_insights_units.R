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
  
  # Validate inputs
  validate_numeric(steam_app_id, "steam_app_id")
  
  # Make API request to the new endpoint
  result <- make_api_request(
    endpoint = paste0("commercial-performance/units-sold/games/", steam_app_id),
    auth_token = auth_token,
    method = "GET",
    headers = headers
  )
  
  # Convert date strings to Date objects if present
  if (!is.null(result) && length(result) > 0 && is.data.frame(result)) {
    if ("date" %in% names(result)) {
      result$date <- as.Date(result$date)
    }
  }
  
  return(result)
}

#' Format date for API
#' 
#' @param date Date or character string to format
#' @return Character string in YYYY-MM-DD format
#' @keywords internal
format_date <- function(date) {
  if (is.character(date)) {
    # Try to parse the date
    parsed <- tryCatch(
      as.Date(date),
      error = function(e) NA
    )
    if (is.na(parsed)) {
      stop("Invalid date format. Please use YYYY-MM-DD format.")
    }
    return(format(parsed, "%Y-%m-%d"))
  } else if (inherits(date, "Date")) {
    return(format(date, "%Y-%m-%d"))
  } else {
    stop("Date must be a Date object or character string in YYYY-MM-DD format.")
  }
}