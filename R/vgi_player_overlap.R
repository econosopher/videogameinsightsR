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
  
  query_params <- list(
    limit = 1,
    steamAppIds = as.character(steam_app_id)
  )
  if (!is.null(offset) && offset > 0) query_params$cursor <- as.integer(offset)
  
  # Make API request
  result <- make_api_request(
    endpoint = "player-overlap",
    query_params = query_params,
    auth_token = auth_token,
    method = "GET",
    headers = headers
  )

  rows <- .vgi_unwrap_results(result)
  empty_overlaps <- data.frame(
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

  if (!is.data.frame(rows) || nrow(rows) == 0 || !"playerOverlaps" %in% names(rows)) {
    return(list(steamAppId = as.integer(steam_app_id), playerOverlaps = empty_overlaps))
  }

  row <- .vgi_steam_row(rows, steam_app_id)
  if (is.null(row) || !"playerOverlaps" %in% names(row)) {
    return(list(steamAppId = as.integer(steam_app_id), playerOverlaps = empty_overlaps))
  }
  overlaps <- row$playerOverlaps[[1]]
  if (!is.data.frame(overlaps) || nrow(overlaps) == 0) {
    return(list(steamAppId = as.integer(steam_app_id), playerOverlaps = empty_overlaps))
  }

  overlaps_df <- data.frame(
    steamAppId = as.integer(overlaps$externalId %||% NA),
    medianPlaytime = as.numeric(overlaps$medianPlaytime %||% NA),
    unitsSoldOverlap = as.numeric(overlaps$unitsSoldOverlap %||% NA),
    unitsSoldOverlapPercentage = as.numeric(overlaps$unitsSoldOverlapPercentage %||% NA),
    unitsSoldOverlapIndex = as.numeric(overlaps$unitsSoldOverlapIndex %||% NA),
    mauOverlap = as.numeric(overlaps$mauOverlap %||% NA),
    mauOverlapPercentage = as.numeric(overlaps$mauOverlapPercentage %||% NA),
    mauOverlapIndex = as.numeric(overlaps$mauOverlapIndex %||% NA),
    wishlistOverlap = as.numeric(overlaps$wishlistOverlap %||% NA),
    wishlistOverlapPercentage = as.numeric(overlaps$wishlistOverlapPercentage %||% NA),
    wishlistOverlapIndex = as.numeric(overlaps$wishlistOverlapIndex %||% NA),
    stringsAsFactors = FALSE
  )
  overlaps_df <- overlaps_df[order(-overlaps_df$unitsSoldOverlapPercentage), , drop = FALSE]
  
  if (!is.null(limit) && nrow(overlaps_df) > limit) {
    overlaps_df <- overlaps_df[seq_len(limit), , drop = FALSE]
  }

  list(
    steamAppId = as.integer(steam_app_id),
    playerOverlaps = overlaps_df
  )
}