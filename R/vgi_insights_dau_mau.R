#' Get Daily and Monthly Active Users Data
#'
#' Retrieve daily active users (DAU) and monthly active users (MAU) data for a specific game.
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
#'       \item dau: Daily active users count
#'       \item mau: Monthly active users count
#'     }
#'   }
#' }
#'
#' @details
#' You can calculate the DAU/MAU ratio from the returned data:
#' `dau_mau_ratio = dau / mau`
#' 
#' The DAU/MAU ratio is a key metric for measuring player engagement:
#' \itemize{
#'   \item A ratio of 1.0 means every monthly user plays daily (perfect retention)
#'   \item A ratio of 0.5 means the average player plays 15 days per month
#'   \item A ratio of 0.1 means the average player plays 3 days per month
#' }
#' 
#' Industry benchmarks:
#' \itemize{
#'   \item Casual games: 0.1-0.2
#'   \item Mid-core games: 0.2-0.4
#'   \item Hardcore games: 0.4-0.6
#' }
#'
#' @export
#' @examples
#' \dontrun{
#' # Get DAU/MAU data for a game
#' active_players <- vgi_insights_dau_mau(steam_app_id = 730)
#' 
#' # Calculate DAU/MAU ratios
#' active_players$playerHistory$dau_mau_ratio <- 
#'   active_players$playerHistory$dau / active_players$playerHistory$mau
#' 
#' # Calculate average DAU/MAU ratio
#' avg_ratio <- mean(active_players$playerHistory$dau_mau_ratio, na.rm = TRUE)
#' print(paste("Average DAU/MAU ratio:", round(avg_ratio, 3)))
#' 
#' # Plot DAU and MAU over time
#' old_par <- par(no.readonly = TRUE)
#' par(mfrow = c(2, 1))
#' plot(active_players$playerHistory$date, active_players$playerHistory$dau, 
#'      type = "l", col = "blue",
#'      main = "Daily Active Users", 
#'      xlab = "Date", ylab = "DAU")
#' plot(active_players$playerHistory$date, active_players$playerHistory$mau, 
#'      type = "l", col = "red",
#'      main = "Monthly Active Users", 
#'      xlab = "Date", ylab = "MAU")
#' 
#' # Analyze retention trends
#' plot(active_players$playerHistory$date, 
#'      active_players$playerHistory$dau_mau_ratio,
#'      type = "l", ylim = c(0, 1),
#'      main = "Player Retention (DAU/MAU Ratio)",
#'      xlab = "Date", ylab = "DAU/MAU Ratio")
#' abline(h = 0.3, col = "gray", lty = 2)  # Industry average
#' par(old_par)
#' }
vgi_insights_dau_mau <- function(steam_app_id, 
                               auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                               headers = list()) {
  
  validate_numeric(steam_app_id, "steam_app_id")

  hist <- vgi_historical_data(steam_app_id, auth_token = auth_token, headers = headers)
  
  ap <- hist$activePlayers
  if (is.null(ap) || nrow(ap) == 0) {
    player_history <- tibble::tibble(
      date = as.Date(character()), dau = integer(), mau = integer(),

    )
  } else {
    player_history <- tibble::tibble(
      date = as.Date(ap$date),
      dau = as.integer(ap$dau),
      mau = as.integer(ap$mau),

    )
    player_history <- player_history[order(player_history$date), , drop = FALSE]
  }
  
  .vgi_clean_list(list(
    steamAppId = as.integer(steam_app_id),
    playerHistory = player_history
  ))
}
