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
  
  # Make individual requests for each game
  results <- lapply(steam_app_ids, function(app_id) {
    tryCatch({
      response <- make_api_request(
        endpoint = paste0("games/", app_id, "/metadata"),
        auth_token = auth_token,
        headers = headers
      )
      res <- process_api_response(response)
      # Ensure steamAppId exists
      if (!"steamAppId" %in% names(res)) {
        sid <- NA_integer_
        if (is.list(response) && !is.null(response$steamAppId)) sid <- as.integer(response$steamAppId)
        res$steamAppId <- sid %||% as.integer(app_id)
      }
      # Backward-compatible id column
      if (!"id" %in% names(res)) res$id <- res$steamAppId
      tibble::as_tibble(res)
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