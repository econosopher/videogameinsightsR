#' Get All Developer Game IDs
#'
#' Retrieve a comprehensive mapping of all developers to their game IDs,
#' useful for bulk analysis of developer portfolios.
#'
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{developerId}{Integer. The developer's company ID}
#'   \item{gameIds}{List. A list of Steam App IDs for games by this developer}
#'   \item{gameCount}{Integer. Number of games by this developer}
#' }
#'
#' @details
#' This endpoint provides a complete developer-to-games mapping, enabling:
#' \itemize{
#'   \item Portfolio size analysis across all developers
#'   \item Developer productivity metrics
#'   \item Market concentration studies
#'   \item Genre specialization analysis
#'   \item Developer ranking by output
#' }
#' 
#' Note: The gameIds column contains lists, which may need special handling
#' for certain analyses.
#'
#' @export
#' @examples
#' \dontrun{
#' # Get all developer game mappings
#' dev_games <- vgi_all_developer_games()
#' 
#' # Find most prolific developers
#' top_devs <- head(dev_games[order(-dev_games$gameCount), ], 20)
#' cat("Top 20 most prolific developers:\n")
#' print(top_devs[, c("developerId", "gameCount")])
#' 
#' # Get developer names for context
#' dev_list <- vgi_developer_list()
#' top_devs_named <- merge(top_devs, dev_list, 
#'                        by.x = "developerId", by.y = "id")
#' print(top_devs_named[, c("name", "gameCount")])
#' 
#' # Analyze developer portfolio sizes
#' hist(dev_games$gameCount[dev_games$gameCount <= 50],
#'      breaks = 50,
#'      main = "Distribution of Developer Portfolio Sizes",
#'      xlab = "Number of Games",
#'      col = "lightblue")
#' 
#' # Find single-game developers
#' single_game_devs <- dev_games[dev_games$gameCount == 1, ]
#' cat("Developers with only one game:", nrow(single_game_devs), "\n")
#' cat("Percentage of single-game developers:", 
#'     round(nrow(single_game_devs) / nrow(dev_games) * 100, 1), "%\n")
#' 
#' # Analyze specific developer's portfolio
#' valve_id <- 8  # Example: Valve's ID
#' valve_games <- dev_games$gameIds[dev_games$developerId == valve_id][[1]]
#' if (length(valve_games) > 0) {
#'   cat("Valve has", length(valve_games), "games\n")
#'   
#'   # Get metadata for all Valve games
#'   valve_metadata <- vgi_game_metadata_batch(valve_games)
#'   print(valve_metadata[, c("name", "releaseDate")])
#' }
#' 
#' # Find developers with similar portfolio sizes
#' target_size <- 10
#' similar_devs <- dev_games[dev_games$gameCount >= target_size - 2 & 
#'                          dev_games$gameCount <= target_size + 2, ]
#' cat("Developers with 8-12 games:", nrow(similar_devs), "\n")
#' 
#' # Calculate total games in database
#' total_games <- sum(dev_games$gameCount)
#' unique_games <- length(unique(unlist(dev_games$gameIds)))
#' cat("Total developer-game relationships:", total_games, "\n")
#' cat("Unique games:", unique_games, "\n")
#' cat("Average developers per game:", round(total_games / unique_games, 2), "\n")
#' }
vgi_all_developer_games <- function(auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                                   headers = list()) {
  
  # Make API request
  result <- make_api_request(
    endpoint = "developers/game-ids",
    auth_token = auth_token,
    method = "GET",
    headers = headers
  )
  
  # Convert to data frame
  if (is.list(result) && length(result) > 0) {
    df <- do.call(rbind, lapply(names(result), function(dev_id) {
      game_ids <- result[[dev_id]]
      data.frame(
        developerId = as.integer(dev_id),
        gameIds = I(list(as.integer(unlist(game_ids)))),
        gameCount = length(game_ids),
        stringsAsFactors = FALSE
      )
    }))
    
    # Sort by game count descending
    df <- df[order(-df$gameCount), ]
    
    return(df)
  } else {
    # Return empty data frame with correct structure
    return(data.frame(
      developerId = integer(),
      gameIds = I(list()),
      gameCount = integer(),
      stringsAsFactors = FALSE
    ))
  }
}