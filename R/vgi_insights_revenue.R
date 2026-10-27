#' Get Revenue History for a Game
#'
#' Retrieve the daily revenue history for a single Steam game. In v4 the
#' figures come from `premiumRevenueChange` / `premiumRevenueTotal` (the
#' successors of the deprecated `revenueChange` / `revenueTotal`).
#'
#' @param steam_app_id Integer. The Steam App ID of the game.
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A [tibble][tibble::tibble] with columns:
#' \describe{
#'   \item{steam_app_id}{Integer. The Steam App ID}
#'   \item{date}{Date}
#'   \item{revenue_change}{Numeric. Revenue earned on that day (USD)}
#'   \item{revenue_total}{Numeric. Cumulative revenue to date (USD)}
#' }
#'
#' @export
#' @examples
#' \dontrun{
#' rev <- vgi_insights_revenue(steam_app_id = 4019220)
#' plot(rev$date, rev$revenue_change, type = "l")
#' }
vgi_insights_revenue <- function(steam_app_id,
                                 auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                                 headers = list()) {

  if (is.null(steam_app_id) || identical(steam_app_id, "")) stop("steam_app_id is required")
  steam_app_id <- suppressWarnings(as.numeric(steam_app_id))

  validate_numeric(steam_app_id, "steam_app_id")

  rows <- .vgi_v4_game_history(steam_app_id, auth_token = auth_token, headers = headers)
  if (nrow(rows) == 0) {
    return(.vgi_clean_names(tibble::tibble(
      steamAppId = integer(), date = as.Date(character()),
      revenueChange = numeric(), revenueTotal = numeric()
    )))
  }

  out <- tibble::tibble(
    steamAppId = as.integer(steam_app_id),
    date = as.Date(rows$date),
    revenueChange = as.numeric(.vgi_col(rows, c("premiumRevenueChange", "revenueChange"))),
    revenueTotal = as.numeric(.vgi_col(rows, c("premiumRevenueTotal", "revenueTotal")))
  )
  .vgi_clean_names(out[order(out$date), , drop = FALSE])
}
