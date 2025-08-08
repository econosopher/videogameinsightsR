#' Get Publisher List with Filtering
#'
#' Retrieve a filtered list of game publishers from the Video Game Insights database.
#' At least one filtering parameter is required to avoid retrieving the entire database.
#'
#' @param search Character string. Search term to filter publishers by name. Optional.
#' @param limit Integer. Maximum number of results to return. Optional.
#' @param min_games Integer. Minimum number of games published. Optional.
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{id}{Integer. The company ID}
#'   \item{name}{Character. The publisher name}
#' }
#'
#' @details
#' This endpoint is useful for:
#' \itemize{
#'   \item Discovering publishers matching specific criteria
#'   \item Building publisher selection interfaces
#'   \item Finding publisher IDs for further queries
#'   \item Analyzing the publishing landscape
#' }
#' 
#' Note: At least one filtering parameter must be provided to prevent
#' accidentally retrieving thousands of publishers.
#'
#' @export
#' @examples
#' \dontrun{
#' # Search for major publishers
#' ea_pubs <- vgi_publisher_list(search = "Electronic Arts")
#' print(ea_pubs)
#' 
#' # Get top publishers (limit results)
#' top_pubs <- vgi_publisher_list(limit = 100)
#' cat("Retrieved", nrow(top_pubs), "publishers\n")
#' 
#' # Find major publishers with many games
#' major_pubs <- vgi_publisher_list(min_games = 20, limit = 50)
#' print(major_pubs)
#' 
#' # Search for specific publisher
#' ubi <- vgi_publisher_list(search = "Ubisoft", limit = 1)
#' if (nrow(ubi) > 0) {
#'   # Get more info about this publisher
#'   ubi_info <- vgi_publisher_info(ubi$id[1])
#'   print(ubi_info)
#' }
#' 
#' # Find publishers with "Games" in name
#' games_pubs <- vgi_publisher_list(search = "Games", limit = 200)
#' cat("Publishers with 'Games' in name:", nrow(games_pubs), "\n")
#' 
#' # Export for analysis
#' top_100 <- vgi_publisher_list(limit = 100)
#' write.csv(top_100, "vgi_top_publishers.csv", row.names = FALSE)
#' }
vgi_publisher_list <- function(search = NULL,
                              limit = NULL,
                              min_games = NULL,
                              auth_token = Sys.getenv("VGI_AUTH_TOKEN"), 
                              headers = list()) {
  
  # Validate authentication early to satisfy test expectations
  get_auth_token(auth_token)

  # Require the search parameter as the primary criterion
  if (is.null(search)) {
    stop("The 'search' parameter is required. You must specify a publisher name or search term to find publishers. The 'min_games' and 'limit' parameters are optional filters.")
  }
  
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
    validate_numeric(min_games, "min_games", min_val = 1)
  }
  
  # Build query parameters
  query_params <- list()
  if (!is.null(search)) {
    query_params$search <- search
  }
  if (!is.null(limit)) {
    query_params$limit <- limit
  }
  if (!is.null(min_games)) {
    query_params$min_games <- min_games
  }
  
  # Make API request
  result <- make_api_request(
    endpoint = "publishers/publisher-list",
    query_params = query_params,
    auth_token = auth_token,
    method = "GET",
    headers = headers
  )
  
  # Convert to data frame
  if (is.list(result) && length(result) > 0) {
    df <- do.call(rbind, lapply(result, function(x) {
      data.frame(
        id = as.integer(x$id),
        name = as.character(x$name),
        stringsAsFactors = FALSE
      )
    }))
    
    # Sort by name for easier browsing
    df <- df[order(df$name), ]
    
    return(df)
  } else {
    # Return empty data frame with correct structure
    return(data.frame(
      id = integer(),
      name = character(),
      stringsAsFactors = FALSE
    ))
  }
}