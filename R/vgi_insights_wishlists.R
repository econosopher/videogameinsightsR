#' Get Wishlist History for a Game
#'
#' Retrieve the outstanding-wishlist history for a single Steam game.
#'
#' @param steam_app_id Integer. The Steam App ID of the game.
#' @param version API generation. `"v4"` (default) takes the series from the
#'   v4 `/historical-data` endpoint, which covers every day since the game was
#'   first tracked (pre-release days included). `"v3"` calls the per-game v3
#'   endpoint ``/interest-level/wishlists/games/{steamAppId}``, which starts closer to release.
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A list containing:
#' \describe{
#'   \item{steam_app_id}{Integer. The Steam App ID}
#'   \item{wishlist_changes}{Tibble with columns `date`, `wishlists_total`,
#'     `wishlists_change`}
#' }
#'
#' @export
#' @examples
#' \dontrun{
#' wl <- vgi_insights_wishlists(steam_app_id = 4019220)
#' tail(wl$wishlist_changes)
#' }
vgi_insights_wishlists <- function(steam_app_id,
                                   version = c("v4", "v3"),
                                   auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                                   headers = list()) {

  validate_numeric(steam_app_id, "steam_app_id")
  version <- .vgi_api_version(match.arg(version))

  rows <- .vgi_game_series(steam_app_id, version,
                           "interest-level/wishlists/games/%s", "wishlistChanges",
                           auth_token = auth_token, headers = headers)
  if (nrow(rows) == 0) {
    changes_df <- tibble::tibble(
      date = as.Date(character()), wishlistsTotal = integer(), wishlistsChange = integer()
    )
  } else {
    changes_df <- tibble::tibble(
      date = as.Date(rows$date),
      wishlistsTotal = as.integer(.vgi_col(rows, "wishlistsTotal")),
      wishlistsChange = as.integer(.vgi_col(rows, "wishlistsChange"))
    )
    changes_df <- changes_df[order(changes_df$date), , drop = FALSE]
  }

  .vgi_clean_list(list(steamAppId = as.integer(steam_app_id), wishlistChanges = changes_df))
}
