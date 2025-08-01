#' Search Games in Video Game Insights
#'
#' Search for games by title using the Video Game Insights database.
#'
#' @param query Character string. The search query (game title).
#' @param limit Integer. Maximum number of results to return. Defaults to 10.
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A [tibble][tibble::tibble] containing search results with game
#'   information including steamAppId and name.
#'
#' @examples
#' \dontrun{
#' # Ensure the VGI_AUTH_TOKEN environment variable is set
#' # Sys.setenv(VGI_AUTH_TOKEN = "your_auth_token_here")
#'
#' # Search for games with "valve" in the title
#' valve_games <- vgi_search_games("valve")
#' print(valve_games)
#'
#' # Search with more results
#' rpg_games <- vgi_search_games("rpg", limit = 50)
#' }
#'
#' @export
vgi_search_games <- function(query,
                            limit = 10,
                            auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                            headers = list()) {
  
  # Validate inputs
  if (is.null(query) || query == "") {
    stop("query parameter is required")
  }
  
  # Validate limit
  if (!is.numeric(limit) || limit < 1 || limit > 1000) {
    stop("limit must be between 1 and 1000")
  }
  
  # Make API request to games endpoint with search parameter
  result <- make_api_request(
    endpoint = "games/game-list",
    query_params = list(
      search = query,
      limit = limit
    ),
    auth_token = auth_token,
    headers = headers
  )
  
  # Convert to data frame if needed
  if (!is.null(result) && length(result) > 0) {
    if (is.list(result) && !is.data.frame(result)) {
      # Handle list of games
      df <- do.call(rbind, lapply(result, function(x) {
        data.frame(
          steamAppId = as.integer(x$id %||% x$steamAppId %||% NA),
          name = x$name %||% NA_character_,
          stringsAsFactors = FALSE
        )
      }))
      return(df)
    }
  }
  
  return(result)
}