#' Get Publisher Names and IDs
#'
#' Retrieve the publisher directory from `/companies/publishers/list`, which
#' pages with a cursor and includes slugs.
#'
#' @param search Character. Optional case-insensitive regular expression
#'   applied to publisher names after download.
#' @param limit Integer. Records per page (maximum 1000).
#' @param vgi_ids Integer vector. VGI company IDs to select.
#' @param slugs Character vector. VGI company slugs to select.
#' @param cursor Integer. Cursor from a previous page.
#' @param all_pages Logical. Follow the cursor through every page.
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A [tibble][tibble::tibble] with columns `id`, `name`, `slug` and
#'   `vgi_url`, sorted by name. The attribute `next_cursor` carries the cursor
#'   for the next page.
#'
#' @export
#' @examples
#' \dontrun{
#' vgi_publisher_list(search = "valve")
#' vgi_publisher_list(slugs = "free-lives")
#' }
vgi_publisher_list <- function(search = NULL,
                               limit = NULL,
                               auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                               headers = list(),
                               vgi_ids = NULL,
                               slugs = NULL,
                               cursor = NULL,
                               all_pages = FALSE) {
  .vgi_company_directory("publishers", search, limit, vgi_ids, slugs, cursor,
                         all_pages, auth_token, headers)
}

.vgi_company_directory <- function(kind, search, limit, vgi_ids, slugs, cursor,
                                   all_pages, auth_token, headers) {
  get_auth_token(auth_token)
  if (!is.null(limit) && limit > 1000) {
    warning("limit capped at 1000 (API maximum).")
    limit <- 1000
  }
  qp <- .vgi_v4_query(vgi_ids = vgi_ids, slugs = slugs, limit = limit, cursor = cursor)
  page <- .vgi_fetch_v4_pages(sprintf("companies/%s/list", kind), qp, auth_token = auth_token,
                              headers = headers, all_pages = all_pages)
  rows <- page$results
  df <- if (is.data.frame(rows) && nrow(rows) > 0) {
    tibble::tibble(
      id = as.integer(rows$vgiCompanyId),
      name = as.character(.vgi_col(rows, "name", NA_character_)),
      slug = as.character(.vgi_col(rows, "slug", NA_character_)),
      vgiUrl = as.character(.vgi_col(rows, "vgiUrl", NA_character_))
    )
  } else {
    tibble::tibble(id = integer(), name = character(), slug = character(), vgiUrl = character())
  }
  df <- df[!is.na(df$id), , drop = FALSE]
  if (!is.null(search) && nzchar(search)) {
    nm <- df$name
    nm[is.na(nm)] <- ""
    df <- df[grepl(search, nm, ignore.case = TRUE), , drop = FALSE]
  }
  out <- .vgi_clean_names(df[order(df$name), , drop = FALSE])
  attr(out, "next_cursor") <- page$next_cursor
  out
}
