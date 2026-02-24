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
#' }
vgi_insights_dau_mau <- function(steam_app_id, 
                               auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                               headers = list()) {
  
  # Validate inputs
  validate_numeric(steam_app_id, "steam_app_id")

  # v4 no longer has per-game DAU history endpoint; derive latest available
  # snapshot from historical-data while keeping a backwards-compatible shape.
  candidate_dates <- as.character(Sys.Date() - c(7:14, 6:1))
  snapshot <- NULL

  for (d in candidate_dates) {
    rows <- tryCatch(
      .vgi_historical_results(
        date = d,
        steam_app_ids = as.integer(steam_app_id),
        limit = 20,
        auth_token = auth_token,
        headers = headers
      ),
      error = function(e) NULL
    )
    if (!is.data.frame(rows) || nrow(rows) == 0) next

    steam_rows <- rows
    if ("platform" %in% names(steam_rows)) {
      steam_rows <- steam_rows[steam_rows$platform == "steam", , drop = FALSE]
    }
    if ("externalId" %in% names(steam_rows)) {
      steam_rows <- steam_rows[as.integer(steam_rows$externalId) == as.integer(steam_app_id), , drop = FALSE]
    }
    if (nrow(steam_rows) == 0) next

    snapshot <- steam_rows[1, , drop = FALSE]
    break
  }

  if (is.null(snapshot)) {
    player_history <- data.frame(
      date = as.Date(character()),
      dau = integer(),
      mau = integer(),
      stringsAsFactors = FALSE
    )
  } else {
    player_history <- data.frame(
      date = as.Date(snapshot$date %||% NA_character_),
      dau = as.integer(snapshot$dau %||% NA),
      mau = as.integer(snapshot$mau %||% NA),
      stringsAsFactors = FALSE
    )
  }

  list(
    steamAppId = as.integer(steam_app_id),
    playerHistory = player_history
  )
}