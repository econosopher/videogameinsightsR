#' Get a Historical Data Snapshot for a Date (v4)
#'
#' Retrieve one day of metrics for many games at once from the v4
#' `/historical-data` endpoint. The v4 endpoint is multi-platform and can
#' break the numbers down by country or region.
#'
#' @param date Character string or Date. Snapshot date (`YYYY-MM-DD`).
#'   Earliest supported value is 2014-01-01. `NULL` lets the API choose its
#'   default date.
#' @param steam_app_ids Integer vector. Steam App IDs to select. Optional.
#' @param vgi_ids Integer vector. VGI internal game IDs to select. Optional.
#' @param slugs Character vector. VGI game slugs to select. Optional.
#' @param countries Character vector of 2-letter ISO country codes. When
#'   supplied the API returns one record per game and country.
#' @param regions Character vector of VGI region slugs. When supplied the API
#'   returns one record per game and region.
#' @param limit Integer. Games per page (API default 5, maximum 1000).
#' @param cursor Integer. Cursor from a previous page (`next_cursor` attribute).
#' @param all_pages Logical. Follow `nextCursor` until every page is fetched.
#' @param platforms Character vector. Keep only these platforms
#'   (`"steam"`, `"xbox"`, `"playstation"`). `NULL` keeps every platform.
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A tibble with one row per game (and platform / country / region when
#'   requested). Columns are the snake_case API fields, e.g. `vgi_id`,
#'   `platform`, `external_id`, `steam_app_id` (NA for non-steam rows), `date`,
#'   `country`, `revenue_total`, `revenue_change`, `units_sold_total`,
#'   `dau`, `mau`, `wishlists_total`, `ccu_max`, `price_final`. The attribute
#'   `next_cursor` carries the cursor for the next page (NULL when exhausted).
#'
#' @export
#' @examples
#' \dontrun{
#' snap <- vgi_historical_data_by_date("2026-09-28", steam_app_ids = 4019220)
#' snap[, c("steam_app_id", "ccu_max", "revenue_total")]
#'
#' # Country breakdown
#' vgi_historical_data_by_date("2026-09-28", steam_app_ids = 4019220,
#'                             countries = c("US", "DE"))
#' }
vgi_historical_data_by_date <- function(date,
                                        steam_app_ids = NULL,
                                        vgi_ids = NULL,
                                        slugs = NULL,
                                        countries = NULL,
                                        regions = NULL,
                                        limit = NULL,
                                        cursor = NULL,
                                        all_pages = FALSE,
                                        platforms = NULL,
                                        auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                                        headers = list()) {
  qp <- .vgi_v4_query(steam_app_ids = steam_app_ids, vgi_ids = vgi_ids, slugs = slugs,
                      limit = limit, cursor = cursor, countries = countries, regions = regions)
  if (!is.null(date)) qp$date <- format_date(date)

  page <- .vgi_fetch_v4_pages("historical-data", qp, auth_token = auth_token,
                              headers = headers, all_pages = all_pages)
  rows <- page$results
  if (!is.data.frame(rows) || nrow(rows) == 0) {
    out <- .vgi_clean_names(tibble::tibble(
      vgiId = integer(), platform = character(), externalId = integer(),
      steamAppId = integer(), date = character()
    ))
    attr(out, "next_cursor") <- NULL
    return(out)
  }

  # Drop the API's deprecated-fields marker column
  rows <- rows[, !grepl("^__", names(rows)), drop = FALSE]
  if (!is.null(platforms) && "platform" %in% names(rows)) {
    rows <- rows[rows$platform %in% platforms, , drop = FALSE]
  }
  rows$steamAppId <- .vgi_steam_ids(rows)
  if ("date" %in% names(rows)) rows$date <- as.character(rows$date)

  out <- .vgi_clean_names(tibble::as_tibble(rows))
  attr(out, "next_cursor") <- page$next_cursor
  out
}
