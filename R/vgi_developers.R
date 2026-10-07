#' Get Developer Information (v3)
#'
#' Retrieve a developer's summary metrics from the v3 `/developers/{companyId}`
#' endpoint. For the multi-platform v4 overview use [vgi_developers_overview()].
#'
#' @param company_id Integer. The VGI company ID of the developer.
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A one-row [tibble][tibble::tibble] with columns `company_id`,
#'   `name`, `classification`, `games_developed`, `games_in_development`,
#'   `revenue_total`, `revenue_avg_per_game`, `revenue_median_per_game`.
#'   Empty when the developer is unknown.
#'
#' @export
#' @examples
#' \dontrun{
#' vgi_developer_info(28663)
#' }
vgi_developer_info <- function(company_id,
                              auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                              headers = list()) {
  validate_numeric(company_id, "company_id")
  result <- make_api_request(
    endpoint = sprintf("developers/%s", as.integer(company_id)),
    auth_token = auth_token, method = "GET", headers = headers, version = "v3"
  )
  .vgi_company_rows(result, "gamesDeveloped")
}

#' Get a Developer's Steam Games (v3)
#'
#' Retrieve the Steam App IDs developed by a company from the v3
#' `/developers/{companyId}/game-ids` endpoint.
#'
#' @inheritParams vgi_developer_info
#' @return An integer vector of Steam App IDs (empty when none).
#' @export
#' @examples
#' \dontrun{
#' vgi_developer_games(28663)
#' }
vgi_developer_games <- function(company_id,
                               auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                               headers = list()) {
  validate_numeric(company_id, "company_id")
  result <- make_api_request(
    endpoint = sprintf("developers/%s/game-ids", as.integer(company_id)),
    auth_token = auth_token, method = "GET", headers = headers, version = "v3"
  )
  as.integer(unlist(result$steamAppIds))
}

#' List Developers with Summary Metrics (v3)
#'
#' Page through the v3 `/developers` catalogue. For the multi-platform v4
#' overview use [vgi_developers_overview()].
#'
#' @inheritParams vgi_publishers
#' @return A [tibble][tibble::tibble] with the columns of [vgi_developer_info()].
#' @export
#' @examples
#' \dontrun{
#' vgi_developers(limit = 100)
#' }
vgi_developers <- function(offset = NULL, limit = NULL, all_pages = FALSE,
                           auth_token = Sys.getenv("VGI_AUTH_TOKEN"), headers = list()) {
  .vgi_company_catalogue("developers", "gamesDeveloped", offset, limit, all_pages, auth_token, headers)
}
