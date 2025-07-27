#' Get Complete Game List
#'
#' Retrieve a list of all games available in the Video Game Insights database.
#' This is a lightweight endpoint that returns game IDs and names only.
#'
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{id}{Integer. The Steam App ID}
#'   \item{name}{Character. The game name}
#' }
#'
#' @details
#' This endpoint is useful for:
#' \itemize{
#'   \item Getting a complete inventory of tracked games
#'   \item Building game selection interfaces
#'   \item Caching game names for ID lookups
#'   \item Validating game IDs before making other API calls
#' }
#' 
#' Note: This endpoint may return thousands of games. Consider caching
#' the results locally to avoid repeated API calls.
#'
#' @export
#' @examples
#' \dontrun{
#' # Get all games
#' all_games <- vgi_game_list()
#' cat("Total games in database:", nrow(all_games), "\n")
#' 
#' # Search for games by name
#' cs_games <- all_games[grep("Counter-Strike", all_games$name, ignore.case = TRUE), ]
#' print(cs_games)
#' 
#' # Find game ID by exact name
#' game_id <- all_games$id[all_games$name == "Counter-Strike 2"]
#' if (length(game_id) > 0) {
#'   cat("Counter-Strike 2 ID:", game_id, "\n")
#' }
#' 
#' # Get random sample of games for analysis
#' sample_games <- all_games[sample(nrow(all_games), 10), ]
#' print(sample_games)
#' 
#' # Cache results for future use
#' saveRDS(all_games, "vgi_game_list_cache.rds")
#' # Later: all_games <- readRDS("vgi_game_list_cache.rds")
#' }
vgi_game_list <- function(auth_token = Sys.getenv("VGI_AUTH_TOKEN"), headers = list()) {
  
  # Make API request
  result <- make_api_request(
    endpoint = "games/game-list",
    auth_token = auth_token,
    method = "GET",
    headers = headers
  )
  
  # Result is already a data frame from jsonlite
  if (is.data.frame(result) && nrow(result) > 0) {
    # Ensure column types are correct
    df <- result
    df$id <- as.integer(df$id)
    df$name <- as.character(df$name)
    
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