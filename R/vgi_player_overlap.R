#' Get Player Overlap Data
#'
#' Retrieve player overlap data showing which other games are played by players
#' of a specific game. This helps identify similar games and player preferences.
#'
#' @param steam_app_id Integer. The Steam App ID of the main game.
#' @param limit Integer. Maximum number of overlapping games to return (default 10).
#' @param offset Integer. Number of records to skip for pagination (default 0).
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A list containing:
#' \describe{
#'   \item{steamAppId}{Integer. The Steam App ID of the main game}
#'   \item{playerOverlaps}{Data frame with columns for each overlapping game:
#'     \itemize{
#'       \item steamAppId: ID of the overlapping game
#'       \item medianPlaytime: Median hours played of main game by overlap players
#'       \item unitsSoldOverlap: Number of players who own both games
#'       \item unitsSoldOverlapPercentage: Percent of main game owners who own this game
#'       \item unitsSoldOverlapIndex: How much more likely to own vs average Steam user
#'       \item mauOverlap: Monthly active users who play both games
#'       \item mauOverlapPercentage: Percent of main game MAU who play this game
#'       \item mauOverlapIndex: How much more likely to play vs average Steam user
#'       \item wishlistOverlap: Number who wishlist both games
#'       \item wishlistOverlapPercentage: Percent of main game wishlisters who wishlist this
#'       \item wishlistOverlapIndex: How much more likely to wishlist vs average
#'     }
#'   }
#' }
#'
#' @details
#' Player overlap data is valuable for:
#' \itemize{
#'   \item Competitive analysis - Identify direct competitors
#'   \item Marketing - Find games with similar audiences for cross-promotion
#'   \item Game design - Understand what other games your players enjoy
#'   \item Platform strategy - Identify bundle opportunities
#' }
#' 
#' The overlap index is particularly useful:
#' \itemize{
#'   \item Index > 2.0: Strong overlap, very similar audience
#'   \item Index 1.5-2.0: Moderate overlap, some audience similarity
#'   \item Index 1.0-1.5: Slight overlap, minimal similarity
#'   \item Index < 1.0: Below average overlap
#' }
#'
#' @export
#' @examples
#' \dontrun{
#' # Get player overlap for a game
#' overlap <- vgi_player_overlap(steam_app_id = 892970, limit = 20)
#' 
#' # Find games with strongest overlap
#' strong_overlap <- overlap$playerOverlaps[
#'   overlap$playerOverlaps$unitsSoldOverlapIndex > 2.0,
#' ]
#' print(paste("Games with strong overlap:", nrow(strong_overlap)))
#' 
#' # Analyze competitor landscape
#' competitors <- head(overlap$playerOverlaps[
#'   order(-overlap$playerOverlaps$unitsSoldOverlapPercentage),
#' ], 5)
#' print("Top 5 competitors by player overlap:")
#' print(competitors[, c("steamAppId", "unitsSoldOverlapPercentage", 
#'                       "unitsSoldOverlapIndex")])
#' 
#' # Find games where overlap players are highly engaged
#' engaged_overlap <- overlap$playerOverlaps[
#'   overlap$playerOverlaps$medianPlaytime > 50,
#' ]
#' print(paste("Games where overlap players spend 50+ hours:", 
#'             nrow(engaged_overlap)))
#' 
#' # Calculate total addressable market from overlap
#' total_overlap_players <- sum(overlap$playerOverlaps$unitsSoldOverlap)
#' print(paste("Total unique players across all overlaps:", 
#'             format(total_overlap_players, big.mark = ",")))
#' }
vgi_player_overlap <- function(steam_app_id,
                             limit = 10,
                             offset = 0,
                             auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                             headers = list()) {
  
  # Validate inputs
  validate_numeric(steam_app_id, "steam_app_id")
  validate_numeric(limit, "limit", min_val = 1)
  validate_numeric(offset, "offset", min_val = 0)
  
  # Build query parameters
  query_params <- list(
    limit = limit,
    offset = offset
  )
  
  # Make API request
  result <- make_api_request(
    endpoint = paste0("player-insights/games/", steam_app_id, "/player-overlap"),
    query_params = query_params,
    auth_token = auth_token,
    method = "GET",
    headers = headers
  )
  
  # Process the playerOverlaps array if it exists
  if (!is.null(result$playerOverlaps) && length(result$playerOverlaps) > 0) {
    # Convert to data frame
    overlaps_df <- do.call(rbind, lapply(result$playerOverlaps, function(x) {
      data.frame(
        steamAppId = as.integer(x$steamAppId),
        medianPlaytime = as.numeric(x$medianPlaytime %||% NA),
        unitsSoldOverlap = as.numeric(x$unitsSoldOverlap %||% NA),
        unitsSoldOverlapPercentage = as.numeric(x$unitsSoldOverlapPercentage %||% NA),
        unitsSoldOverlapIndex = as.numeric(x$unitsSoldOverlapIndex %||% NA),
        mauOverlap = as.numeric(x$mauOverlap %||% NA),
        mauOverlapPercentage = as.numeric(x$mauOverlapPercentage %||% NA),
        mauOverlapIndex = as.numeric(x$mauOverlapIndex %||% NA),
        wishlistOverlap = as.numeric(x$wishlistOverlap %||% NA),
        wishlistOverlapPercentage = as.numeric(x$wishlistOverlapPercentage %||% NA),
        wishlistOverlapIndex = as.numeric(x$wishlistOverlapIndex %||% NA),
        stringsAsFactors = FALSE
      )
    }))
    
    # Sort by units sold overlap percentage (most relevant first)
    overlaps_df <- overlaps_df[order(-overlaps_df$unitsSoldOverlapPercentage), ]
    
    result$playerOverlaps <- overlaps_df
  } else {
    # Return empty data frame with correct structure
    result$playerOverlaps <- data.frame(
      steamAppId = integer(),
      medianPlaytime = numeric(),
      unitsSoldOverlap = numeric(),
      unitsSoldOverlapPercentage = numeric(),
      unitsSoldOverlapIndex = numeric(),
      mauOverlap = numeric(),
      mauOverlapPercentage = numeric(),
      mauOverlapIndex = numeric(),
      wishlistOverlap = numeric(),
      wishlistOverlapPercentage = numeric(),
      wishlistOverlapIndex = numeric(),
      stringsAsFactors = FALSE
    )
  }
  
  return(result)
}