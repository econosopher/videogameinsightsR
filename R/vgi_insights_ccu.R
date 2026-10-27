#' Get Concurrent Users (CCU) Data
#'
#' Retrieve the daily concurrent player history for a single Steam game.
#'
#' @param steam_app_id Integer. The Steam App ID of the game.
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A list containing:
#' \describe{
#'   \item{steam_app_id}{Integer. The Steam App ID}
#'   \item{player_history}{Tibble with columns `date`, `avg`, `median`, `max`,
#'     `min` (concurrent players), sorted by date}
#' }
#'
#' @export
#' @examples
#' \dontrun{
#' ccu <- vgi_insights_ccu(steam_app_id = 4019220)
#' plot(ccu$player_history$date, ccu$player_history$max, type = "l")
#' }
vgi_insights_ccu <- function(steam_app_id,
                             auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                             headers = list()) {

  validate_numeric(steam_app_id, "steam_app_id")

  hist <- .vgi_v4_game_history(steam_app_id, auth_token = auth_token, headers = headers)
  if (nrow(hist) == 0) {
    history_df <- tibble::tibble(
      date = as.Date(character()), avg = numeric(),
      median = numeric(), max = numeric(), min = numeric()
    )
  } else {
    history_df <- tibble::tibble(
      date = as.Date(hist$date),
      avg = as.numeric(.vgi_col(hist, c("avg", "ccuAvg"))),
      median = as.numeric(.vgi_col(hist, c("median", "ccuMedian"))),
      max = as.numeric(.vgi_col(hist, c("max", "ccuMax"))),
      min = as.numeric(.vgi_col(hist, c("min", "ccuMin")))
    )
    history_df <- history_df[order(history_df$date), , drop = FALSE]
  }

  .vgi_clean_list(list(steamAppId = as.integer(steam_app_id), playerHistory = history_df))
}
