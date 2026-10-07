#' Get Daily and Monthly Active Users (DAU/MAU)
#'
#' Retrieve the active player history for a single Steam game.
#'
#' @param steam_app_id Integer. The Steam App ID of the game.
#' @param version API generation. `"v4"` (default) takes the series from the
#'   v4 `/historical-data` endpoint, which covers every day since the game was
#'   first tracked (pre-release days included). `"v3"` calls the per-game v3
#'   endpoint ``/engagement/active-players/games/{steamAppId}``, which starts closer to release.
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A list containing:
#' \describe{
#'   \item{steam_app_id}{Integer. The Steam App ID}
#'   \item{player_history}{Tibble with columns `date`, `dau`, `mau`}
#' }
#'
#' @details DAU is available from 2024-03-18 and MAU from 2024-03-23.
#'
#' @export
#' @examples
#' \dontrun{
#' active <- vgi_insights_dau_mau(steam_app_id = 4019220)
#' tail(active$player_history)
#' }
vgi_insights_dau_mau <- function(steam_app_id,
                                 version = c("v4", "v3"),
                                 auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                                 headers = list()) {

  validate_numeric(steam_app_id, "steam_app_id")
  version <- .vgi_api_version(match.arg(version))

  hist <- .vgi_game_series(steam_app_id, version,
                           "engagement/active-players/games/%s", "playerHistory",
                           auth_token = auth_token, headers = headers)
  if (nrow(hist) == 0) {
    player_history <- tibble::tibble(date = as.Date(character()), dau = integer(), mau = integer())
  } else {
    player_history <- tibble::tibble(
      date = as.Date(hist$date),
      dau = as.integer(.vgi_col(hist, "dau")),
      mau = as.integer(.vgi_col(hist, "mau"))
    )
    player_history <- player_history[order(player_history$date), , drop = FALSE]
  }

  .vgi_clean_list(list(steamAppId = as.integer(steam_app_id), playerHistory = player_history))
}
