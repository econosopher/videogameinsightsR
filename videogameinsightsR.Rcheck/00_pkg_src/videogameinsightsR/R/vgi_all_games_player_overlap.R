#' Get Player Overlap Data for All Games
#'
#' Retrieve player overlap statistics for all games, showing which games
#' share the most players and identifying gaming ecosystem connections.
#'
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{steamAppId}{Integer. The Steam App ID}
#'   \item{topOverlaps}{List. Top overlapping games with overlap metrics}
#'   \item{overlapCount}{Integer. Number of games with significant overlap}
#'   \item{topOverlapGame}{Integer. Steam App ID of most overlapping game}
#'   \item{topOverlapPct}{Numeric. Percentage overlap with top game}
#' }
#'
#' @details
#' Player overlap data reveals:
#' \itemize{
#'   \item Direct competitors (high overlap = similar audience)
#'   \item Complementary games (moderate overlap = cross-promotion opportunities)
#'   \item Genre clusters and gaming ecosystems
#'   \item Sequel/franchise connections
#'   \item Platform-specific communities
#' }
#' 
#' The topOverlaps list column contains detailed overlap data
#' that can be analyzed for deeper insights.
#'
#' @export
#' @examples
#' \dontrun{
#' # Get player overlap data for all games
#' overlap_data <- vgi_all_games_player_overlap()
#' 
#' # Find games with highest overlap percentages
#' high_overlap <- overlap_data[overlap_data$topOverlapPct > 50, ]
#' cat("Games with >50% overlap with another game:", nrow(high_overlap), "\n")
#' 
#' # These are likely sequels, expansions, or very similar games
#' print(head(high_overlap[, c("steamAppId", "topOverlapGame", "topOverlapPct")]))
#' 
#' # Analyze overlap patterns
#' hist(overlap_data$topOverlapPct,
#'      breaks = 50,
#'      main = "Distribution of Maximum Player Overlap",
#'      xlab = "Top Overlap Percentage",
#'      col = "lightcoral")
#' 
#' # Find gaming clusters (mutual high overlap)
#' # Check if game A's top overlap is game B, and vice versa
#' mutual_overlaps <- overlap_data[
#'   mapply(function(id, top_id) {
#'     if (is.na(top_id)) return(FALSE)
#'     overlap_data$topOverlapGame[overlap_data$steamAppId == top_id] == id
#'   }, overlap_data$steamAppId, overlap_data$topOverlapGame),
#' ]
#' cat("Games with mutual top overlap:", nrow(mutual_overlaps), "\n")
#' 
#' # Extract detailed overlap data for analysis
#' # Find games that overlap with a specific game
#' target_game <- 730  # Example: Counter-Strike 2
#' games_overlapping_target <- overlap_data[
#'   sapply(overlap_data$topOverlaps, function(overlaps) {
#'     if (is.null(overlaps)) return(FALSE)
#'     target_game %in% overlaps$steamAppId
#'   }),
#' ]
#' cat("Games with significant overlap with game", target_game, ":", 
#'     nrow(games_overlapping_target), "\n")
#' 
#' # Build a gaming ecosystem map
#' # Extract all overlap relationships
#' all_overlaps <- do.call(rbind, lapply(seq_len(nrow(overlap_data)), function(i) {
#'   game_id <- overlap_data$steamAppId[i]
#'   overlaps <- overlap_data$topOverlaps[[i]]
#'   if (is.null(overlaps) || nrow(overlaps) == 0) return(NULL)
#'   
#'   data.frame(
#'     from = game_id,
#'     to = overlaps$steamAppId[1:min(5, nrow(overlaps))],
#'     overlap_pct = overlaps$overlapPercentage[1:min(5, nrow(overlaps))]
#'   )
#' }))
#' 
#' # Find most connected games (hubs in the network)
#' connection_counts <- table(c(all_overlaps$from, all_overlaps$to))
#' hubs <- head(sort(connection_counts, decreasing = TRUE), 20)
#' cat("Most connected games (appear in many overlaps):\n")
#' print(hubs)
#' 
#' # Genre affinity analysis (would need genre data)
#' # Games with high overlap likely share genres
#' # Could cluster games based on overlap patterns
#' 
#' # Find isolated games (low overlap with any other game)
#' isolated_games <- overlap_data[overlap_data$topOverlapPct < 5 | 
#'                                is.na(overlap_data$topOverlapPct), ]
#' cat("Games with <5% overlap (unique/niche):", nrow(isolated_games), "\n")
#' }
vgi_all_games_player_overlap <- function(auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                                        headers = list()) {
  
  # Make API request
  result <- make_api_request(
    endpoint = "player-overlap",
    auth_token = auth_token,
    method = "GET",
    headers = headers
  )

  rows <- .vgi_unwrap_results(result)
  if (!is.data.frame(rows) || nrow(rows) == 0) {
    return(.vgi_clean_names(data.frame(
      steamAppId = integer(),
      topOverlaps = I(list()),
      overlapCount = integer(),
      topOverlapGame = integer(),
      topOverlapPct = numeric(),
      stringsAsFactors = FALSE
    )))
  }

  df <- do.call(rbind, lapply(seq_len(nrow(rows)), function(i) {
    overlaps <- if ("playerOverlaps" %in% names(rows)) rows$playerOverlaps[[i]] else NULL
    if (is.data.frame(overlaps) && nrow(overlaps) > 0) {
      overlap_df <- data.frame(
        steamAppId = as.integer(overlaps$externalId %||% NA),
        overlapPercentage = as.numeric(overlaps$unitsSoldOverlapPercentage %||% NA),
        overlapIndex = as.numeric(overlaps$unitsSoldOverlapIndex %||% NA),
        stringsAsFactors = FALSE
      )
      overlap_count <- nrow(overlap_df)
      top_overlap_game <- overlap_df$steamAppId[1]
      top_overlap_pct <- overlap_df$overlapPercentage[1]
    } else {
      overlap_df <- NULL
      overlap_count <- 0
      top_overlap_game <- NA_integer_
      top_overlap_pct <- NA_real_
    }

    data.frame(
      steamAppId = as.integer(rows$externalId[i] %||% NA),
      topOverlaps = I(list(overlap_df)),
      overlapCount = as.integer(overlap_count),
      topOverlapGame = as.integer(top_overlap_game),
      topOverlapPct = as.numeric(top_overlap_pct),
      stringsAsFactors = FALSE
    )
  }))

  df <- df[!is.na(df$steamAppId), , drop = FALSE]
  df <- df[order(-df$topOverlapPct, na.last = TRUE), , drop = FALSE]
  .vgi_clean_names(df)
}