#' Search Games in Video Game Insights
#'
#' Search for games by title using the Video Game Insights database.
#' Since the new API doesn't have a dedicated search endpoint, this function
#' fetches the game list and performs client-side filtering.
#'
#' @param query Character string. The search query (game title).
#' @param limit Integer. Maximum number of results to return. Defaults to 10.
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A [tibble][tibble::tibble] containing search results with game
#'   information including Steam App ID and name.
#'
#' @details
#' This function now uses the `/games/game-list` endpoint and filters results
#' locally. For better performance with large datasets, consider caching the
#' game list.
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
  validate_numeric(limit, "limit", min_val = 1, max_val = 1000)
  
  # Get the full game list
  response <- make_api_request(
    endpoint = "games/game-list",
    auth_token = auth_token,
    headers = headers
  )
  
  # Process response
  game_list <- process_api_response(response)
  
  # Check if we got a valid response
  if (is.null(game_list) || nrow(game_list) == 0) {
    return(tibble::tibble())
  }
  
  # Ensure we have the expected columns
  if (!all(c("id", "name") %in% names(game_list))) {
    stop("Unexpected response format from game-list endpoint")
  }
  
  # Rename id to steam_app_id for consistency
  game_list <- dplyr::rename(game_list, steam_app_id = "id")
  
  # Perform case-insensitive search
  query_lower <- tolower(query)
  filtered_games <- game_list[grepl(query_lower, tolower(game_list$name), fixed = FALSE), ]
  
  # Limit results
  if (nrow(filtered_games) > limit) {
    filtered_games <- utils::head(filtered_games, limit)
  }
  
  # Sort by relevance (games that start with the query come first)
  starts_with <- grepl(paste0("^", query_lower), tolower(filtered_games$name))
  filtered_games <- filtered_games[order(!starts_with), ]
  
  return(filtered_games)
}