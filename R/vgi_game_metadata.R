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
  
  # v4 metadata endpoint takes IDs as query params.
  response <- make_api_request(
    endpoint = "games/metadata",
    query_params = list(steamAppIds = steam_app_id, limit = 1),
    auth_token = auth_token,
    headers = headers
  )
  rows <- .vgi_unwrap_results(response)

  if (!is.data.frame(rows) || nrow(rows) == 0) {
    return(.vgi_clean_names(tibble::tibble()))
  }

  row <- rows[1, , drop = FALSE]
  parsed_steam_id <- .vgi_parse_steam_app_id(row$storeUrl.steam)
  out_steam_id <- if (!is.na(parsed_steam_id)) parsed_steam_id else as.integer(steam_app_id)

  # Normalize nested vectors/lists into compact scalar fields for compatibility.
  genres <- if ("genre" %in% names(row)) paste(unlist(row$genre[[1]]), collapse = ", ") else NA_character_
  subgenres <- if ("subgenre" %in% names(row)) paste(unlist(row$subgenre[[1]]), collapse = ", ") else NA_character_
  languages <- if ("languages" %in% names(row)) paste(unlist(row$languages[[1]]), collapse = ", ") else NA_character_

  result <- tibble::tibble(
    steamAppId = as.integer(out_steam_id),
    id = as.integer(out_steam_id),
    name = as.character(row$name %||% NA_character_),
    price = as.numeric(row$price.steam %||% NA),
    releaseDate = as.character(row$releaseDate.steam %||% row$steamFullReleaseDate %||% NA_character_),
    fullReleaseDate = as.character(row$steamFullReleaseDate %||% NA_character_),
    genres = genres,
    subgenres = subgenres,
    languages = languages,
    publisherClassification = as.character(row$publisherClassification %||% NA_character_),
    vgiUrl = as.character(row$vgiUrl %||% NA_character_),
    steamUrl = as.character(row$storeUrl.steam %||% NA_character_),
    publishingType = as.character(row$publishingType %||% NA_character_),

  )
  
  # Add publisher info if available
  if ("publishers" %in% names(row)) {
    pubs <- row$publishers[[1]]
    if (is.data.frame(pubs) && nrow(pubs) > 0) {
      result$publisherId <- as.integer(pubs$companyId[1] %||% NA_integer_)
      result$publisherName <- as.character(pubs$companyName[1] %||% NA_character_)
    }
  }
  
  # Add developer info if available
  if ("developers" %in% names(row)) {
    devs <- row$developers[[1]]
    if (is.data.frame(devs) && nrow(devs) > 0) {
      result$developerId <- as.integer(devs$companyId[1] %||% NA_integer_)
      result$developerName <- as.character(devs$companyName[1] %||% NA_character_)
    }
  }
  
  .vgi_clean_names(result)
}
