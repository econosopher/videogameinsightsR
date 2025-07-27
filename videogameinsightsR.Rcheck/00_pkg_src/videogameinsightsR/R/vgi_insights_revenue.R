#' Get Revenue Insights from Video Game Insights
#'
#' Retrieve revenue history data for a specific game.
#'
#' @param steam_app_id Integer or character. The Steam App ID of the game.
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A [tibble][tibble::tibble] containing revenue history with columns:
#' \describe{
#'   \item{steamAppId}{Integer. The Steam App ID}
#'   \item{date}{Date. The date of the revenue data}
#'   \item{revenueChange}{Numeric. Revenue change amount}
#'   \item{revenueChangePercent}{Numeric. Revenue change percentage}
#' }
#'
#' @details
#' The new API provides revenue history as changes rather than absolute values.
#' Each entry shows the revenue change from the previous period.
#'
#' @examples
#' \dontrun{
#' # Get revenue history for a game
#' revenue_data <- vgi_insights_revenue(steam_app_id = 892970)
#' 
#' # Plot revenue changes over time
#' plot(revenue_data$date, revenue_data$revenueChange,
#'      type = "l",
#'      main = "Revenue Changes Over Time",
#'      xlab = "Date", 
#'      ylab = "Revenue Change")
#' }
#'
#' @export
vgi_insights_revenue <- function(steam_app_id,
                                auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                                headers = list()) {
  
  # Validate inputs
  if (is.null(steam_app_id) || steam_app_id == "") {
    stop("steam_app_id is required")
  }
  
  # Convert to character if numeric
  steam_app_id <- as.character(steam_app_id)
  
  # Make API request to the new endpoint
  response <- make_api_request(
    endpoint = paste0("commercial-performance/revenue/games/", steam_app_id),
    auth_token = auth_token,
    headers = headers
  )
  
  # Process response
  result <- process_api_response(response)
  
  # Convert date strings to Date objects if present
  if (!is.null(result) && "date" %in% names(result)) {
    result$date <- as.Date(result$date)
  }
  
  return(result)
}