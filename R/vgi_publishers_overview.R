#' Get v4 Publisher Overview Data
#'
#' Fetches publisher overview rows from the v4 `companies/publishers` endpoint.
#'
#' @param vgi_ids Optional numeric/integer vector of VGI company IDs.
#' @param slugs Optional character vector of publisher slugs.
#' @param cursor Optional cursor for pagination.
#' @param limit Optional page size (1-1000).
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to `VGI_AUTH_TOKEN`.
#' @param headers List. Optional custom headers.
#'
#' @return A data frame of publisher overview records.
#' @export
vgi_publishers_overview <- function(vgi_ids = NULL,
                                    slugs = NULL,
                                    cursor = NULL,
                                    limit = 100,
                                    auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                                    headers = list()) {
  if (!is.null(limit)) validate_numeric(limit, "limit", min_val = 1, max_val = 1000)
  if (!is.null(cursor)) validate_numeric(cursor, "cursor", min_val = 0)

  query_params <- list()
  if (!is.null(limit)) query_params$limit <- as.integer(limit)
  if (!is.null(cursor)) query_params$cursor <- as.integer(cursor)
  if (!is.null(vgi_ids) && length(vgi_ids) > 0) query_params$vgiIds <- .vgi_to_csv_ids(as.integer(vgi_ids))
  if (!is.null(slugs) && length(slugs) > 0) query_params$slugs <- paste(unique(slugs[!is.na(slugs)]), collapse = ",")

  result <- make_api_request(
    endpoint = "companies/publishers",
    query_params = query_params,
    auth_token = auth_token,
    method = "GET",
    headers = headers
  )

  rows <- .vgi_unwrap_results(result)
  if (is.data.frame(rows)) return(.vgi_clean_names(rows))
  .vgi_clean_names(tibble::tibble())
}
