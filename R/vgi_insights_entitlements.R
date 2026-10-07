#' Get Entitlements (Units Sold) History for a Game
#'
#' \strong{Deprecated.}
#'
#' The VGI API has no entitlements endpoint; this function has always returned
#' the units-sold history. Use [vgi_insights_units()] instead. The first call in
#' a session emits a deprecation warning.
#'
#' @inheritParams vgi_insights_units
#' @return A [tibble][tibble::tibble] with columns `steam_app_id`, `date`,
#'   `entitlements_change` and `entitlements_total` (aliases of the units-sold
#'   columns).
#' @keywords internal
#' @export
vgi_insights_entitlements <- function(steam_app_id,
                                      auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                                      headers = list(),
                                      version = c("v4", "v3")) {
  .vgi_deprecate("vgi_insights_entitlements()", "vgi_insights_units()")
  units <- vgi_insights_units(steam_app_id, version = version,
                              auth_token = auth_token, headers = headers)
  .vgi_clean_names(tibble::tibble(
    steamAppId = units$steam_app_id,
    date = units$date,
    entitlementsChange = units$units_sold_change,
    entitlementsTotal = units$units_sold_total
  ))
}

