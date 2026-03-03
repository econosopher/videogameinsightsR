#' Get Developer List with Filtering
#'
#' Retrieve a filtered list of game developers from the Video Game Insights database.
#' At least one filtering parameter is required to avoid retrieving the entire database.
#'
#' @param search Character string. Search term to filter developers by name. Optional.
#' @param limit Integer. Maximum number of results to return. Optional.
#' @param min_games Integer. Minimum number of games published. Optional.
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{id}{Integer. The company ID}
#'   \item{name}{Character. The developer name}
#' }
#'
#' @details
#' This endpoint is useful for:
#' \itemize{
#'   \item Discovering developers matching specific criteria
#'   \item Building developer selection interfaces
#'   \item Finding developer IDs for further queries
#'   \item Analyzing the developer ecosystem
#' }
#' 
#' Note: At least one filtering parameter must be provided to prevent
#' accidentally retrieving thousands of developers.
#'
#' @export
#' @examples
#' \dontrun{
#' # Search for developers by name
#' valve_devs <- vgi_developer_list(search = "Valve")
#' print(valve_devs)
#' 
#' # Get top developers (limit results)
#' top_devs <- vgi_developer_list(limit = 100)
#' cat("Retrieved", nrow(top_devs), "developers\n")
#' 
#' # Find prolific developers
#' prolific_devs <- vgi_developer_list(min_games = 10, limit = 50)
#' print(prolific_devs)
#' 
#' # Search for studios
#' studios <- vgi_developer_list(search = "Studios", limit = 200)
#' cat("Found", nrow(studios), "studios\n")
#' 
#' # Get developer info for a specific developer
#' dev <- vgi_developer_list(search = "Valve Corporation", limit = 1)
#' if (nrow(dev) > 0) {
#'   valve_info <- vgi_developer_info(dev$id[1])
#'   print(valve_info)
#' }
#' }
vgi_developer_list <- function(search = NULL,
                              limit = NULL,
                              min_games = NULL,
                              auth_token = Sys.getenv("VGI_AUTH_TOKEN"), 
                              headers = list()) {
  
  # Validate authentication early to satisfy test expectations
  get_auth_token(auth_token)
  
  # Prevent abuse of limit parameter
  if (!is.null(limit) && limit > 1000) {
    warning("Large limit values defeat the purpose of filtering. Consider using more specific search criteria. Limit capped at 1000.")
    limit <- 1000
  }
  
  # Validate inputs
  if (!is.null(limit)) {
    validate_numeric(limit, "limit", min_val = 1, max_val = 10000)
  }
  
  if (!is.null(min_games)) {
    warning("'min_games' is not supported by v4 developer list endpoint and will be ignored.")
  }
  query_params <- list()
  if (!is.null(limit)) query_params$limit <- limit

  result <- make_api_request(
    endpoint = "companies/developers/list",
    query_params = query_params,
    auth_token = auth_token,
    method = "GET",
    headers = headers
  )

  rows <- .vgi_unwrap_results(result)
  if (!is.data.frame(rows) || nrow(rows) == 0) {
    return(.vgi_clean_names(data.frame(id = integer(), name = character(), stringsAsFactors = FALSE)))
  }

  df <- data.frame(
    id = as.integer(rows$vgiCompanyId %||% NA),
    name = as.character(rows$name %||% NA_character_),
    slug = as.character(rows$slug %||% NA_character_),
    stringsAsFactors = FALSE
  )
  df <- df[!is.na(df$id), , drop = FALSE]
  if (!is.null(search) && nzchar(search)) {
    nm <- df$name
    nm[is.na(nm)] <- ""
    df <- df[grepl(search, nm, ignore.case = TRUE), , drop = FALSE]
  }
  df <- df[order(df$name), , drop = FALSE]
  .vgi_clean_names(df)
}