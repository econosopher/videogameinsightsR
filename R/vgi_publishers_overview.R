#' Get Publisher Overview (v4, multi-platform)
#'
#' Retrieve publisher summaries from the v4 `/companies/publishers` endpoint,
#' including per-platform revenue, release counts and genre breakdowns.
#'
#' @param vgi_ids Integer vector. VGI company IDs to select. Optional.
#' @param slugs Character vector. VGI company slugs to select. Optional.
#' @param cursor Integer. Cursor from a previous page.
#' @param limit Integer. Records per page (API default 200, maximum 1000).
#' @param all_pages Logical. Follow the cursor through every page.
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A [tibble][tibble::tibble] with one row per publisher:
#'   `vgi_company_id`, `name`, `slug`, `classification`, `country`, `vgi_url`
#'   and the flattened `platform_data.<platform>.<metric>` columns (for
#'   example `platform_data.steam.revenue_total`,
#'   `platform_data.steam.genre_breakdown.rpg`). The attribute `next_cursor`
#'   carries the cursor for the next page.
#'
#' @export
#' @examples
#' \dontrun{
#' vgi_publishers_overview(vgi_ids = 28663)
#' vgi_publishers_overview(limit = 500)
#' }
vgi_publishers_overview <- function(vgi_ids = NULL,
                                    slugs = NULL,
                                    cursor = NULL,
                                    limit = 100,
                                    auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                                    headers = list(),
                                    all_pages = FALSE) {
  .vgi_company_overview("publishers", vgi_ids, slugs, cursor, limit, all_pages, auth_token, headers)
}

.vgi_company_overview <- function(kind, vgi_ids, slugs, cursor, limit, all_pages, auth_token, headers) {
  qp <- .vgi_v4_query(vgi_ids = vgi_ids, slugs = slugs, limit = limit, cursor = cursor)
  page <- .vgi_fetch_v4_pages(sprintf("companies/%s", kind), qp, auth_token = auth_token,
                              headers = headers, all_pages = all_pages)
  rows <- page$results
  out <- if (is.data.frame(rows) && nrow(rows) > 0) {
    .vgi_clean_names(tibble::as_tibble(rows))
  } else {
    .vgi_clean_names(tibble::tibble(vgiCompanyId = integer(), name = character(), slug = character()))
  }
  attr(out, "next_cursor") <- page$next_cursor
  out
}
