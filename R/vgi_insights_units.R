#' Get Units Sold History for a Game
#'
#' Retrieve the daily units history for a single Steam game. In v4 the figures
#' come from `unitsOwnedChange` / `unitsOwnedTotal` (the successors of the
#' deprecated `unitsSoldChange` / `unitsSoldTotal`; they include copies obtained
#' for free).
#'
#' @param steam_app_id Integer. The Steam App ID of the game.
#' @param version API generation. `"v4"` (default) takes the series from the
#'   v4 `/historical-data` endpoint, which covers every day since the game was
#'   first tracked (pre-release days included). `"v3"` calls the per-game v3
#'   endpoint ``/commercial-performance/units-sold/games/{steamAppId}``, which starts closer to release.
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A [tibble][tibble::tibble] with columns:
#' \describe{
#'   \item{steam_app_id}{Integer. The Steam App ID}
#'   \item{date}{Date}
#'   \item{units_sold_change}{Integer. Units sold on that day}
#'   \item{units_sold_total}{Integer. Cumulative units sold to date}
#' }
#'
#' @export
#' @examples
#' \dontrun{
#' units <- vgi_insights_units(steam_app_id = 4019220)
#' max(units$units_sold_total)
#' }
vgi_insights_units <- function(steam_app_id,
                               auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                               headers = list(),
                               version = c("v4", "v3")) {

  validate_numeric(steam_app_id, "steam_app_id")
  version <- .vgi_api_version(match.arg(version))

  rows <- .vgi_game_series(steam_app_id, version,
                           "commercial-performance/units-sold/games/%s",
                           auth_token = auth_token, headers = headers)
  if (nrow(rows) == 0) {
    return(.vgi_clean_names(tibble::tibble(
      steamAppId = integer(), date = as.Date(character()),
      unitsSoldChange = integer(), unitsSoldTotal = integer()
    )))
  }

  out <- tibble::tibble(
    steamAppId = as.integer(steam_app_id),
    date = as.Date(rows$date),
    unitsSoldChange = as.integer(.vgi_col(rows, c("unitsOwnedChange", "unitsSoldChange"))),
    unitsSoldTotal = as.integer(.vgi_col(rows, c("unitsOwnedTotal", "unitsSoldTotal")))
  )
  .vgi_clean_names(out[order(out$date), , drop = FALSE])
}
