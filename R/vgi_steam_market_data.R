#' Get Steam Market Data
#'
#' Retrieve monthly Steam-wide market totals (units, revenue, releases, users
#' online, accounts). The v4 `/market-data` endpoint (default) adds a
#' `platform` column; the v3 `/analytics/steam-market-data` endpoint returns
#' the Steam series only.
#'
#' @param version `"v4"` (default) or `"v3"`.
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A [tibble][tibble::tibble] with one row per month: `period`
#'   (`YYYY-MM-01`), `units_total`, `revenue_total`, `releases_total`,
#'   `users_online`, `users_ingame`, `accounts_total` and (v4) `platform`.
#'
#' @export
#' @examples
#' \dontrun{
#' market <- vgi_steam_market_data()
#' tail(market)
#' }
vgi_steam_market_data <- function(version = c("v4", "v3"),
                                  auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                                  headers = list()) {
  version <- match.arg(version)
  rows <- make_api_request(
    endpoint = if (version == "v3") "analytics/steam-market-data" else "market-data",
    auth_token = auth_token, method = "GET", headers = headers, version = version
  )
  rows <- .vgi_unwrap_results(rows)
  if (is.data.frame(rows) && nrow(rows) > 0) return(.vgi_clean_names(tibble::as_tibble(rows)))
  .vgi_clean_names(tibble::tibble(period = character(), unitsTotal = numeric(), revenueTotal = numeric()))
}
