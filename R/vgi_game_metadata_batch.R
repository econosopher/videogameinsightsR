#' Get Batch Game Metadata from Video Game Insights
#'
#' Retrieves metadata for multiple games using their Steam App IDs.
#' Since the new API doesn't have a batch endpoint, this function makes
#' multiple individual requests and combines the results.
#'
#' @param steam_app_ids Numeric vector. The Steam App IDs of the games.
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A [tibble][tibble::tibble] containing metadata for all requested games.
#'   Each row represents one game with columns for name, release date, price,
#'   genres, categories, developers, publishers, and more.
#'
#' @examples
#' \dontrun{
#' # Ensure the VGI_AUTH_TOKEN environment variable is set
#' # Sys.setenv(VGI_AUTH_TOKEN = "your_auth_token_here")
#'
#' # Get metadata for multiple games
#' game_ids <- c(892970, 1245620, 105600) # Valheim, Elden Ring, Terraria
#' games_data <- vgi_game_metadata_batch(game_ids)
#' print(games_data)
#' }
#'
#' @export
vgi_game_metadata_batch <- function(steam_app_ids, 
                                   auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                                   headers = list()) {
  
  # Validate inputs
  if (is.null(steam_app_ids) || length(steam_app_ids) == 0) {
    stop("steam_app_ids must be a non-empty vector")
  }
  
  # Convert to numeric if character
  steam_app_ids <- as.numeric(steam_app_ids)
  
  # Check for NA values
  if (any(is.na(steam_app_ids))) {
    stop("steam_app_ids contains invalid values")
  }
  
  # Make individual requests for each game using the normalized single-game function
  results <- lapply(steam_app_ids, function(app_id) {
    tryCatch({
      row <- vgi_game_metadata(app_id, auth_token = auth_token, headers = headers)
      # Ensure id column for backward compatibility
      if (!"id" %in% names(row) && "steamAppId" %in% names(row)) row$id <- row$steamAppId
      tibble::as_tibble(row)
    }, error = function(e) {
      warning(sprintf("Failed to fetch metadata for game %s: %s", app_id, e$message))
      NULL
    })
  })
  
  # Remove NULL results (failed requests)
  results <- results[!sapply(results, is.null)]
  
  # If no successful results, return empty tibble
  if (length(results) == 0) {
    return(tibble::tibble())
  }
  
  # Combine results into a single tibble
  result <- suppressWarnings(dplyr::bind_rows(results))
  
  return(result)
}