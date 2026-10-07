#' Get a v3 Daily Snapshot Across Games
#'
#' Thin wrapper over the v3 "by date" endpoints
#' (`/commercial-performance/revenue/{date}`, `/units-sold/{date}`,
#' `/engagement/concurrent-players/{date}`, `/active-players/{date}`,
#' `/reception/reviews/{date}`, `/interest-level/followers/{date}`,
#' `/wishlists/{date}`, `/historical-data/{date}`). Returns the raw rows for
#' one day as a tidy tibble. The higher-level `vgi_*_by_date()` functions use
#' the v4 snapshot endpoint instead; this function gives direct access to the
#' v3 generation when its Steam-only numbers are preferred.
#'
#' @param date Character string or Date (`YYYY-MM-DD`).
#' @param metric One of `"revenue"`, `"units-sold"`, `"concurrent-players"`,
#'   `"active-players"`, `"reviews"`, `"followers"`, `"wishlists"`,
#'   `"historical-data"`.
#' @param steam_app_ids Integer vector. Steam App IDs to select (not supported
#'   by `"active-players"`, which ignores it).
#' @param offset Integer. Records to skip.
#' @param limit Integer. Records to return (API default 5, maximum 1000).
#' @param all_pages Logical. Page through every record for the day.
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A [tibble][tibble::tibble] with `steam_app_id`, `date` and the
#'   metric's snake_case columns (e.g. `revenue_change`, `revenue_total`;
#'   `avg`, `median`, `max`, `min`; `dau`, `mau`).
#'
#' @export
#' @examples
#' \dontrun{
#' vgi_snapshot_by_date("2026-09-28", "concurrent-players", steam_app_ids = 4019220)
#' vgi_snapshot_by_date("2026-09-28", "revenue", limit = 100)
#' }
vgi_snapshot_by_date <- function(date,
                                 metric = c("revenue", "units-sold", "concurrent-players",
                                            "active-players", "reviews", "followers",
                                            "wishlists", "historical-data"),
                                 steam_app_ids = NULL,
                                 offset = NULL,
                                 limit = NULL,
                                 all_pages = FALSE,
                                 auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                                 headers = list()) {
  metric <- match.arg(metric)
  formatted_date <- format_date(date)
  if (!is.null(offset)) validate_numeric(offset, "offset", min_val = 0)
  if (!is.null(limit)) validate_numeric(limit, "limit", min_val = 1, max_val = 1000)

  prefix <- switch(metric,
    "revenue" = "commercial-performance/revenue",
    "units-sold" = "commercial-performance/units-sold",
    "concurrent-players" = "engagement/concurrent-players",
    "active-players" = "engagement/active-players",
    "reviews" = "reception/reviews",
    "followers" = "interest-level/followers",
    "wishlists" = "interest-level/wishlists",
    "historical-data" = "historical-data"
  )
  qp <- list()
  if (!is.null(offset)) qp$offset <- as.integer(offset)
  if (!is.null(limit)) qp$limit <- as.integer(limit)
  if (metric != "active-players") qp$steamAppIds <- .vgi_to_csv_ids(steam_app_ids)

  rows <- .vgi_fetch_v3_pages(sprintf("%s/%s", prefix, formatted_date), qp,
                              auth_token = auth_token, headers = headers,
                              all_pages = all_pages, page_size = as.integer(limit %||% 1000))
  if (!is.data.frame(rows) || nrow(rows) == 0) {
    return(.vgi_clean_names(tibble::tibble(steamAppId = integer(), date = character())))
  }
  rows$steamAppId <- as.integer(rows$steamAppId)
  if (!"date" %in% names(rows)) rows$date <- formatted_date
  rows <- rows[, c("steamAppId", "date", setdiff(names(rows), c("steamAppId", "date"))), drop = FALSE]
  .vgi_clean_names(tibble::as_tibble(rows))
}
