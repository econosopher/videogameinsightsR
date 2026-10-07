#' Get Publisher Names and IDs
#'
#' Retrieve the publisher directory. The v4 `/companies/publishers/list`
#' endpoint (default) pages with a cursor and includes slugs; the v3
#' `/publishers/publisher-list` endpoint returns the whole directory in one
#' (large) response.
#'
#' @param search Character. Optional case-insensitive regular expression
#'   applied to publisher names after download.
#' @param limit Integer. Records per page (v4; maximum 1000). Ignored by v3.
#' @param min_games Deprecated and ignored.
#' @param vgi_ids Integer vector. VGI company IDs to select (v4 only).
#' @param slugs Character vector. VGI company slugs to select (v4 only).
#' @param cursor Integer. Cursor from a previous page (v4 only).
#' @param all_pages Logical. Follow the cursor through every page (v4 only).
#' @param version `"v4"` (default) or `"v3"`.
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A [tibble][tibble::tibble] with columns `id`, `name` and (v4)
#'   `slug`, `vgi_url`, sorted by name. The attribute `next_cursor` carries
#'   the v4 cursor for the next page.
#'
#' @export
#' @examples
#' \dontrun{
#' vgi_publisher_list(search = "valve")
#' vgi_publisher_list(slugs = "free-lives")
#' }
vgi_publisher_list <- function(search = NULL,
                              limit = NULL,
                              min_games = NULL,
                              vgi_ids = NULL,
                              slugs = NULL,
                              cursor = NULL,
                              all_pages = FALSE,
                              version = c("v4", "v3"),
                              auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                              headers = list()) {
  .vgi_company_directory("publishers", search, limit, min_games, vgi_ids, slugs, cursor,
                         all_pages, match.arg(version), auth_token, headers)
}

.vgi_company_directory <- function(kind, search, limit, min_games, vgi_ids, slugs, cursor,
                                   all_pages, version, auth_token, headers) {
  get_auth_token(auth_token)
  if (!is.null(min_games)) .vgi_deprecate(sprintf("vgi_%s_list(min_games=)", sub("s$", "", kind)), "client-side filtering")
  if (!is.null(limit) && limit > 1000) {
    warning("limit capped at 1000 (API maximum).")
    limit <- 1000
  }
  if (!is.null(limit)) validate_numeric(limit, "limit", min_val = 1, max_val = 1000)

  next_cursor <- NULL
  if (version == "v3") {
    rows <- make_api_request(
      endpoint = sprintf("%s/%s-list", kind, sub("s$", "", kind)),
      auth_token = auth_token, method = "GET", headers = headers, version = "v3"
    )
    df <- if (is.data.frame(rows) && nrow(rows) > 0) {
      tibble::tibble(id = as.integer(rows$id), name = as.character(rows$name))
    } else {
      tibble::tibble(id = integer(), name = character())
    }
  } else {
    qp <- .vgi_v4_query(vgi_ids = vgi_ids, slugs = slugs, limit = limit, cursor = cursor)
    page <- .vgi_fetch_v4_pages(sprintf("companies/%s/list", kind), qp, auth_token = auth_token,
                                headers = headers, all_pages = all_pages)
    rows <- page$results
    next_cursor <- page$next_cursor
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
  }
  df <- df[!is.na(df$id), , drop = FALSE]
  if (!is.null(search) && nzchar(search)) {
    nm <- df$name
    nm[is.na(nm)] <- ""
    df <- df[grepl(search, nm, ignore.case = TRUE), , drop = FALSE]
  }
  out <- .vgi_clean_names(df[order(df$name), , drop = FALSE])
  attr(out, "next_cursor") <- next_cursor
  out
}
