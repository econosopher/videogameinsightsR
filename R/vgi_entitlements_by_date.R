#' Get Entitlements (Units Sold) Snapshot for a Date
#'
#' \strong{Deprecated.} The VGI API has no entitlements endpoint; this function
#' is an alias of [vgi_units_sold_by_date()] and emits a one-time warning.
#'
#' @inheritParams vgi_units_sold_by_date
#' @inherit vgi_units_sold_by_date return
#' @keywords internal
#' @export
vgi_entitlements_by_date <- function(date,
                                    steam_app_ids = NULL,
                                    auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                                    headers = list()) {
  .vgi_deprecate("vgi_entitlements_by_date()", "vgi_units_sold_by_date()")
  vgi_units_sold_by_date(date, steam_app_ids = steam_app_ids, auth_token = auth_token, headers = headers)
}
