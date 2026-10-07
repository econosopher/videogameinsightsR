#' Get Follower History for a Game
#'
#' Retrieve the Steam follower history for a single game.
#'
#' @param steam_app_id Integer. The Steam App ID of the game.
#' @param version API generation. `"v4"` (default) takes the series from the
#'   v4 `/historical-data` endpoint, which covers every day since the game was
#'   first tracked (pre-release days included). `"v3"` calls the per-game v3
#'   endpoint ``/interest-level/followers/games/{steamAppId}``, which starts closer to release.
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A list containing:
#' \describe{
#'   \item{steam_app_id}{Integer. The Steam App ID}
#'   \item{followers_change}{Tibble with columns `date`, `followers_total`,
#'     `followers_change`}
#' }
#'
#' @export
#' @examples
#' \dontrun{
#' fol <- vgi_insights_followers(steam_app_id = 4019220)
#' tail(fol$followers_change)
#' }
vgi_insights_followers <- function(steam_app_id,
                                   auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                                   headers = list(),
                                   version = c("v4", "v3")) {

  validate_numeric(steam_app_id, "steam_app_id")
  version <- .vgi_api_version(match.arg(version))

  rows <- .vgi_game_series(steam_app_id, version,
                           "interest-level/followers/games/%s", "followersChange",
                           auth_token = auth_token, headers = headers)
  if (nrow(rows) == 0) {
    changes_df <- tibble::tibble(
      date = as.Date(character()), followersTotal = integer(), followersChange = integer()
    )
  } else {
    changes_df <- tibble::tibble(
      date = as.Date(rows$date),
      followersTotal = as.integer(.vgi_col(rows, "followersTotal")),
      followersChange = as.integer(.vgi_col(rows, "followersChange"))
    )
    changes_df <- changes_df[order(changes_df$date), , drop = FALSE]
  }

  .vgi_clean_list(list(steamAppId = as.integer(steam_app_id), followersChange = changes_df))
}
