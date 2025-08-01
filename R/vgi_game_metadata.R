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
  
  # Handle the response directly since it contains nested structures
  if (is.null(response) || length(response) == 0) {
    return(data.frame())
  }
  
  # Convert to data frame, handling nested lists
  result <- data.frame(
    steamAppId = as.integer(response$steamAppId %||% steam_app_id),
    name = response$name %||% NA_character_,
    price = as.numeric(response$price %||% NA),
    releaseDate = response$releaseDate %||% NA_character_,
    fullReleaseDate = response$fullReleaseDate %||% NA_character_,
    genres = response$genres %||% NA_character_,
    subgenres = response$subgenres %||% NA_character_,
    languages = response$languages %||% NA_character_,
    publisherClassification = response$publisherClassification %||% NA_character_,
    vgiUrl = response$vgiUrl %||% NA_character_,
    steamUrl = response$steamUrl %||% NA_character_,
    publishingType = response$publishingType %||% NA_character_,
    stringsAsFactors = FALSE
  )
  
  # Add publisher info if available
  if (!is.null(response$publishers)) {
    if (is.data.frame(response$publishers) && nrow(response$publishers) > 0) {
      result$publisherId <- response$publishers$companyId[1] %||% NA_integer_
      result$publisherName <- response$publishers$companyName[1] %||% NA_character_
    } else if (is.list(response$publishers) && length(response$publishers) > 0) {
      pub <- response$publishers[[1]]
      result$publisherId <- pub$companyId %||% NA_integer_
      result$publisherName <- pub$companyName %||% NA_character_
    }
  }
  
  # Add developer info if available
  if (!is.null(response$developers)) {
    if (is.data.frame(response$developers) && nrow(response$developers) > 0) {
      result$developerId <- response$developers$companyId[1] %||% NA_integer_
      result$developerName <- response$developers$companyName[1] %||% NA_character_
    } else if (is.list(response$developers) && length(response$developers) > 0) {
      dev <- response$developers[[1]]
      result$developerId <- dev$companyId %||% NA_integer_
      result$developerName <- dev$companyName %||% NA_character_
    }
  }
  
  return(result)
}