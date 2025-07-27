#' Get Game Metadata from Video Game Insights
#'
#' Retrieves detailed metadata for a specific game using its Steam App ID.
#'
#' @param steam_app_id Character or numeric. The Steam App ID of the game.
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A [tibble][tibble::tibble] containing game metadata including
#'   name, release date, price, genres, categories, developers, publishers,
#'   and more.
#'
#' @examples
#' \dontrun{
#' # Ensure the VGI_AUTH_TOKEN environment variable is set
#' # Sys.setenv(VGI_AUTH_TOKEN = "your_auth_token_here")
#'
#' # Get metadata for Valheim (Steam App ID: 892970)
#' valheim_data <- vgi_game_metadata(892970)
#' print(valheim_data)
#' }
#'
#' @export
vgi_game_metadata <- function(steam_app_id, 
                             auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                             headers = list()) {
  
  # Validate inputs
  if (is.null(steam_app_id) || steam_app_id == "") {
    stop("steam_app_id is required")
  }
  
  # Convert to character if numeric
  steam_app_id <- as.character(steam_app_id)
  
  # Make API request using the new endpoint structure
  response <- make_api_request(
    endpoint = paste0("games/", steam_app_id, "/metadata"),
    auth_token = auth_token,
    headers = headers
  )
  
  # Process response
  result <- process_api_response(response)
  
  return(result)
}