#' Get Concurrent Users (CCU) Data
#'
#' Retrieve the daily concurrent player history for a single Steam game.
#'
#' @param steam_app_id Integer. The Steam App ID of the game.
#' @param version API generation. `"v4"` (default) takes the series from the
#'   v4 `/historical-data` endpoint, which covers every day since the game was
#'   first tracked (pre-release days included). `"v3"` calls the per-game v3
#'   endpoint ``/engagement/concurrent-players/games/{steamAppId}``, which starts closer to release.
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
#' ccu_v3 <- vgi_insights_ccu(4019220, version = "v3")
#' }
vgi_insights_ccu <- function(steam_app_id,
                             auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                             headers = list(),
                             version = c("v4", "v3")) {

  validate_numeric(steam_app_id, "steam_app_id")
  version <- .vgi_api_version(match.arg(version))

  hist <- .vgi_game_series(steam_app_id, version,
                           "engagement/concurrent-players/games/%s", "playerHistory",
                           auth_token = auth_token, headers = headers)
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
