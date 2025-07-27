#' Get All Publisher Game IDs
#'
#' Retrieve a comprehensive mapping of all publishers to their game IDs,
#' useful for analyzing publishing portfolios and market dynamics.
#'
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{publisherId}{Integer. The publisher's company ID}
#'   \item{gameIds}{List. A list of Steam App IDs for games by this publisher}
#'   \item{gameCount}{Integer. Number of games by this publisher}
#' }
#'
#' @details
#' This endpoint provides a complete publisher-to-games mapping, enabling:
#' \itemize{
#'   \item Publishing portfolio analysis
#'   \item Market share calculations
#'   \item Publisher strategy assessment
#'   \item Competitive landscape mapping
#'   \item Publisher ranking by catalog size
#' }
#' 
#' Publishers often have larger portfolios than developers as they
#' may publish games from multiple development studios.
#'
#' @export
#' @examples
#' \dontrun{
#' # Get all publisher game mappings
#' pub_games <- vgi_all_publisher_games()
#' 
#' # Find largest publishers by catalog size
#' top_pubs <- head(pub_games[order(-pub_games$gameCount), ], 20)
#' cat("Top 20 largest publishers:\n")
#' print(top_pubs[, c("publisherId", "gameCount")])
#' 
#' # Get publisher names for context
#' pub_list <- vgi_publisher_list()
#' top_pubs_named <- merge(top_pubs, pub_list, 
#'                        by.x = "publisherId", by.y = "id")
#' print(top_pubs_named[, c("name", "gameCount")])
#' 
#' # Compare with developer portfolios
#' dev_games <- vgi_all_developer_games()
#' cat("Average games per developer:", round(mean(dev_games$gameCount), 1), "\n")
#' cat("Average games per publisher:", round(mean(pub_games$gameCount), 1), "\n")
#' 
#' # Find mega-publishers (>100 games)
#' mega_pubs <- pub_games[pub_games$gameCount > 100, ]
#' cat("Publishers with >100 games:", nrow(mega_pubs), "\n")
#' 
#' # Analyze publisher concentration
#' total_pub_games <- sum(pub_games$gameCount)
#' top_10_pub_games <- sum(head(pub_games$gameCount, 10))
#' concentration <- (top_10_pub_games / total_pub_games) * 100
#' cat(sprintf("Top 10 publishers control %.1f%% of published games\n", 
#'             concentration))
#' 
#' # Find publishers that also develop
#' dev_list <- vgi_developer_list()
#' pub_dev_overlap <- intersect(pub_list$name, dev_list$name)
#' cat("Companies that both publish and develop:", 
#'     length(pub_dev_overlap), "\n")
#' 
#' # Analyze specific publisher's portfolio
#' ea_id <- 1  # Example: Electronic Arts
#' ea_games <- pub_games$gameIds[pub_games$publisherId == ea_id][[1]]
#' if (length(ea_games) > 0) {
#'   cat("EA has published", length(ea_games), "games\n")
#'   
#'   # Get recent EA releases
#'   ea_metadata <- vgi_game_metadata_batch(ea_games)
#'   recent_ea <- ea_metadata[as.Date(ea_metadata$releaseDate) > 
#'                           as.Date("2023-01-01"), ]
#'   cat("EA games released since 2023:", nrow(recent_ea), "\n")
#' }
#' 
#' # Distribution of publisher sizes
#' pub_size_dist <- table(cut(pub_games$gameCount,
#'                           breaks = c(1, 2, 5, 10, 20, 50, 100, Inf),
#'                           labels = c("1", "2-4", "5-9", "10-19", 
#'                                     "20-49", "50-99", "100+"),
#'                           right = FALSE))
#' 
#' barplot(pub_size_dist,
#'         main = "Distribution of Publishers by Portfolio Size",
#'         xlab = "Number of Games Published",
#'         ylab = "Number of Publishers",
#'         col = "purple")
#' }
vgi_all_publisher_games <- function(auth_token = Sys.getenv("VGI_AUTH_TOKEN"), headers = list()) {
  
  # Make API request
  result <- make_api_request(
    endpoint = "publishers/game-ids",
    auth_token = auth_token,
    method = "GET",
    headers = headers
  )
  
  # Convert to data frame
  if (is.list(result) && length(result) > 0) {
    df <- do.call(rbind, lapply(names(result), function(pub_id) {
      game_ids <- result[[pub_id]]
      data.frame(
        publisherId = as.integer(pub_id),
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
      publisherId = integer(),
      gameIds = I(list()),
      gameCount = integer(),
      stringsAsFactors = FALSE
    ))
  }
}