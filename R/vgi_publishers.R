#' Get Publisher Information (v3)
#'
#' Retrieve a publisher's summary metrics from the v3 `/publishers/{companyId}`
#' endpoint. For the multi-platform v4 overview use [vgi_publishers_overview()].
#'
#' @param company_id Integer. The VGI company ID of the publisher.
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A one-row [tibble][tibble::tibble] with columns `company_id`,
#'   `name`, `classification`, `games_published`, `games_in_development`,
#'   `revenue_total`, `revenue_avg_per_game`, `revenue_median_per_game`.
#'   Empty when the publisher is unknown.
#'
#' @export
#' @examples
#' \dontrun{
#' vgi_publisher_info(28663)
#' }
vgi_publisher_info <- function(company_id,
                              auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                              headers = list()) {
  validate_numeric(company_id, "company_id")
  result <- make_api_request(
    endpoint = sprintf("publishers/%s", as.integer(company_id)),
    auth_token = auth_token, method = "GET", headers = headers, version = "v3"
  )
  .vgi_company_rows(result, "gamesPublished")
}

#' Get a Publisher's Steam Games (v3)
#'
#' Retrieve the Steam App IDs published by a company from the v3
#' `/publishers/{companyId}/game-ids` endpoint.
#'
#' @inheritParams vgi_publisher_info
#' @return An integer vector of Steam App IDs (empty when none).
#' @export
#' @examples
#' \dontrun{
#' vgi_publisher_games(28663)
#' }
vgi_publisher_games <- function(company_id,
                               auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                               headers = list()) {
  validate_numeric(company_id, "company_id")
  result <- make_api_request(
    endpoint = sprintf("publishers/%s/game-ids", as.integer(company_id)),
    auth_token = auth_token, method = "GET", headers = headers, version = "v3"
  )
  as.integer(unlist(result$steamAppIds))
}

#' List Publishers with Summary Metrics (v3)
#'
#' Page through the v3 `/publishers` catalogue. For the multi-platform v4
#' overview (with per-platform genre breakdowns) use
#' [vgi_publishers_overview()].
#'
#' @param offset Integer. Records to skip.
#' @param limit Integer. Records to return (API default 5, maximum 1000).
#' @param all_pages Logical. Page through the whole catalogue.
#' @inheritParams vgi_publisher_info
#' @return A [tibble][tibble::tibble] with the columns of [vgi_publisher_info()].
#' @export
#' @examples
#' \dontrun{
#' vgi_publishers(limit = 100)
#' }
vgi_publishers <- function(offset = NULL, limit = NULL, all_pages = FALSE,
                           auth_token = Sys.getenv("VGI_AUTH_TOKEN"), headers = list()) {
  .vgi_company_catalogue("publishers", "gamesPublished", offset, limit, all_pages, auth_token, headers)
}

# --- shared company helpers ---

.vgi_company_rows <- function(rows, games_col) {
  if (is.list(rows) && !is.data.frame(rows)) {
    if (length(rows) == 0 || is.null(rows$companyId)) rows <- NULL
    else rows <- as.data.frame(lapply(rows, function(x) if (is.null(x)) NA else x), stringsAsFactors = FALSE)
  }
  if (!is.data.frame(rows) || nrow(rows) == 0) {
    out <- tibble::tibble(companyId = integer(), name = character(), classification = character())
    out[[games_col]] <- integer()
    out$gamesInDevelopment <- integer()
    out$revenueTotal <- numeric(); out$revenueAvgPerGame <- numeric(); out$revenueMedianPerGame <- numeric()
    return(.vgi_clean_names(out))
  }
  out <- tibble::tibble(
    companyId = as.integer(rows$companyId),
    name = as.character(.vgi_col(rows, "name", NA_character_)),
    classification = as.character(.vgi_col(rows, "classification", NA_character_))
  )
  out[[games_col]] <- as.integer(.vgi_col(rows, games_col))
  out$gamesInDevelopment <- as.integer(.vgi_col(rows, "gamesInDevelopment"))
  out$revenueTotal <- as.numeric(.vgi_col(rows, "revenueTotal"))
  out$revenueAvgPerGame <- as.numeric(.vgi_col(rows, "revenueAvgPerGame"))
  out$revenueMedianPerGame <- as.numeric(.vgi_col(rows, "revenueMedianPerGame"))
  .vgi_clean_names(out)
}

.vgi_company_catalogue <- function(endpoint, games_col, offset, limit, all_pages, auth_token, headers) {
  if (!is.null(offset)) validate_numeric(offset, "offset", min_val = 0)
  if (!is.null(limit)) validate_numeric(limit, "limit", min_val = 1, max_val = 1000)
  qp <- list()
  if (!is.null(offset)) qp$offset <- as.integer(offset)
  if (!is.null(limit)) qp$limit <- as.integer(limit)
  rows <- .vgi_fetch_v3_pages(endpoint, qp, auth_token = auth_token, headers = headers,
                              all_pages = all_pages, page_size = as.integer(limit %||% 1000))
  .vgi_company_rows(rows, games_col)
}
