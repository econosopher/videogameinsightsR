#' Get Complete Game List
#'
#' Retrieve the complete list of games from the Video Game Insights database.
#' Note: This endpoint returns ALL games and does not support filtering parameters.
#'
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{steamAppId}{Integer. The Steam App ID}
#'   \item{name}{Character. The game name}
#' }
#'
#' @details
#' This endpoint returns the complete game list from the API. According to the
#' API specification, no filtering parameters are supported. The API will return
#' all games in its database.
#' 
#' WARNING: This may return a very large dataset (potentially 100,000+ games).
#' Consider caching the results locally to avoid repeated API calls.
#' 
#' For filtering games, you'll need to:
#' 1. Fetch the complete list with this function
#' 2. Filter the results locally in R
#' 3. Cache the filtered results for future use
#'
#' @export
#' @examples
#' \dontrun{
#' # Get all games (WARNING: Large dataset)
#' all_games <- vgi_game_list()
#' cat("Total games in database:", nrow(all_games), "\n")
#' 
#' # Filter locally for shooter games
#' # (Requires fetching metadata for each game to get genre)
#' 
#' # Search for games by name locally
#' cs_games <- all_games[grepl("Counter-Strike", all_games$name, ignore.case = TRUE), ]
#' print(cs_games)
#' 
#' # Cache the complete list for future use
#' saveRDS(all_games, "vgi_all_games_cache.rds")
#' 
#' # Later, load from cache instead of API
#' if (file.exists("vgi_all_games_cache.rds")) {
#'   all_games <- readRDS("vgi_all_games_cache.rds")
#' } else {
#'   all_games <- vgi_game_list()
#'   saveRDS(all_games, "vgi_all_games_cache.rds")
#' }
#' 
#' # Get a sample of games
#' sample_games <- all_games[sample(nrow(all_games), 10), ]
#' print(sample_games)
#' }
vgi_game_list <- function(auth_token = Sys.getenv("VGI_AUTH_TOKEN"), 
                         headers = list()) {
  
  # Warn about potentially large response
  message("Note: This endpoint returns ALL games and may take some time. Consider caching the results.")
  
  # Make API request - no query parameters supported
  result <- make_api_request(
    endpoint = "games/game-list",
    query_params = list(),
    auth_token = auth_token,
    method = "GET",
    headers = headers
  )
  
  # Result is already a data frame from jsonlite
  if (is.data.frame(result) && nrow(result) > 0) {
    # Ensure column types are correct and standardize column names
    df <- result
    if ("id" %in% names(df) && !"steamAppId" %in% names(df)) {
      df$steamAppId <- as.integer(df$id)
      df$id <- NULL
    } else if ("steamAppId" %in% names(df)) {
      df$steamAppId <- as.integer(df$steamAppId)
    }
    if ("name" %in% names(df)) {
      df$name <- as.character(df$name)
    }

    # Sort by steamAppId for consistency
    sort_col <- if ("steamAppId" %in% names(df)) "steamAppId" else NULL
    if (!is.null(sort_col)) {
      df <- df[order(df[[sort_col]]), ]
      warn_if_stale_ids(df[[sort_col]])
    }

    # Backward-compatibility: keep an 'id' column mirroring steamAppId
    if ("steamAppId" %in% names(df) && !"id" %in% names(df)) {
      df$id <- df$steamAppId
    }

    # Return tibble for consistency across the package
    return(tibble::as_tibble(df))
  } else {
    # Return empty data frame with correct structure
    return(tibble::tibble(
      steamAppId = integer(),
      name = character()
    ))
  }
}