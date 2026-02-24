#' Get Concurrent Users (CCU) Data
#'
#' Retrieve concurrent player count history for a specific game.
#'
#' @param steam_app_id Integer. The Steam App ID of the game.
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A list containing:
#' \describe{
#'   \item{steamAppId}{Integer. The Steam App ID}
#'   \item{playerHistory}{Data frame with columns:
#'     \itemize{
#'       \item date: Date of the data point
#'       \item avg: Average concurrent players
#'       \item median: Median concurrent players
#'       \item max: Maximum concurrent players
#'     }
#'   }
#' }
#'
#' @export
#' @examples
#' \dontrun{
#' # Get CCU data for Counter-Strike 2
#' ccu_data <- vgi_insights_ccu(steam_app_id = 730)
#' 
#' # Plot max concurrent players over time
#' plot(ccu_data$playerHistory$date, ccu_data$playerHistory$max,
#'      type = "l",
#'      main = "Peak Concurrent Players",
#'      xlab = "Date",
#'      ylab = "Players")
#' 
#' # Calculate average peak CCU
#' avg_peak <- mean(ccu_data$playerHistory$max, na.rm = TRUE)
#' print(paste("Average peak CCU:", round(avg_peak)))
#' }
vgi_insights_ccu <- function(steam_app_id,
                           auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                           headers = list()) {
  
  validate_numeric(steam_app_id, "steam_app_id")
  
  hist <- vgi_historical_data(steam_app_id, auth_token = auth_token, headers = headers)
  
  ccu <- hist$concurrentPlayers
  if (is.null(ccu) || nrow(ccu) == 0) {
    history_df <- data.frame(
      date = as.Date(character()), avg = numeric(),
      median = numeric(), max = numeric(),
      stringsAsFactors = FALSE
    )
  } else {
    history_df <- data.frame(
      date = as.Date(ccu$date),
      avg = as.numeric(ccu$ccuAvg),
      median = as.numeric(ccu$ccuMedian),
      max = as.numeric(ccu$ccuMax),
      stringsAsFactors = FALSE
    )
  }
  
  list(steamAppId = as.integer(steam_app_id), playerHistory = history_df)
}