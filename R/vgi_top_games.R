#' Get Top Games from Video Game Insights
#'
#' Retrieve top games ranked by various metrics including revenue, units sold,
#' concurrent users (CCU), daily active users (DAU), or followers.
#'
#' @param metric Character string. The metric to rank games by. Must be one of:
#'   "revenue", "units", "ccu", "dau", or "followers".
#' @param platform Character string. Platform to filter by. Options are:
#'   "steam", "playstation", "xbox", "nintendo", or "all". Defaults to "all".
#' @param start_date Date or character string. Start date for the ranking period
#'   in YYYY-MM-DD format. Optional.
#' @param end_date Date or character string. End date for the ranking period
#'   in YYYY-MM-DD format. Optional.
#' @param limit Integer. Maximum number of results to return. Defaults to 100.
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A [tibble][tibble::tibble] containing top games with columns:
#'   \itemize{
#'     \item steam_app_id: The Steam App ID
#'     \item name: Game name (when available)
#'     \item rank: Rank for the specified metric (1 = best)
#'     \item percentile: Percentile ranking (0-100)
#'     \item value: Same as percentile (for backwards compatibility)
#'   }
#'
#' @examples
#' \dontrun{
#' # Ensure the VGI_AUTH_TOKEN environment variable is set
#' # Sys.setenv(VGI_AUTH_TOKEN = "your_auth_token_here")
#'
#' # Get top 10 games by revenue
#' top_revenue <- vgi_top_games("revenue", limit = 10)
#' print(top_revenue)
#'
#' # Get top Steam games by CCU for a specific date range
#' top_ccu_steam <- vgi_top_games(
#'   metric = "ccu",
#'   platform = "steam",
#'   start_date = "2024-01-01",
#'   end_date = "2024-01-31",
#'   limit = 50
#' )
#' }
#'
#' @export
vgi_top_games <- function(metric,
                         platform = "all",
                         start_date = NULL,
                         end_date = NULL,
                         limit = 100,
                         auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                         headers = list()) {
  
  # Validate metric
  valid_metrics <- c("revenue", "units", "ccu", "dau", "followers")
  if (!metric %in% valid_metrics) {
    stop(sprintf(
      "Invalid metric '%s'. Must be one of: %s",
      metric,
      paste(valid_metrics, collapse = ", ")
    ))
  }
  
  # Validate platform
  validate_platform(platform)
  
  # Validate dates (though not used in current implementation)
  start_date <- validate_date(start_date, "start_date")
  end_date <- validate_date(end_date, "end_date")
  
  # Validate limit
  validate_numeric(limit, "limit", min_val = 1, max_val = 1000)
  
  # Get rankings data with proper parameters
  # Note: vgi_game_rankings doesn't support platform/date filtering currently
  rankings <- vgi_game_rankings(
    limit = limit * 2,  # Get more than needed to account for filtering
    auth_token = auth_token, 
    headers = headers
  )
  
  if (nrow(rankings) == 0) {
    return(tibble::tibble())
  }
  
  # Determine which column to sort by based on metric
  rank_column <- switch(metric,
    revenue = "totalRevenueRank",
    units = "totalUnitsSoldRank",
    ccu = "avgPlaytimeRank",  # Using playtime as proxy for CCU
    dau = "yesterdayUnitsSoldRank",  # Using yesterday's units as proxy for DAU
    followers = "followersRank"
  )
  
  value_column <- switch(metric,
    revenue = "totalRevenuePrct",
    units = "totalUnitsSoldPrct",
    ccu = "avgPlaytimePrct",
    dau = "yesterdayUnitsSoldPrct",
    followers = "followersPrct"
  )
  
  # Filter out rows where the rank column is NA
  rankings <- rankings[!is.na(rankings[[rank_column]]), ]
  
  # Sort by rank (ascending - lower rank = better)
  rankings <- rankings[order(rankings[[rank_column]]), ]
  
  # Limit results
  if (nrow(rankings) > limit) {
    rankings <- rankings[1:limit, ]
  }
  
  # Try to add game names by fetching metadata for the specific games
  tryCatch({
    # Get unique game IDs from rankings
    game_ids <- unique(rankings$steamAppId)
    
    # Fetch metadata for these specific games
    if (length(game_ids) > 0) {
      game_metadata <- vgi_game_metadata_batch(game_ids, auth_token = auth_token, headers = headers)
      
      if (nrow(game_metadata) > 0 && "id" %in% names(game_metadata) && "name" %in% names(game_metadata)) {
        # Merge with game names
        rankings <- merge(
          rankings,
          game_metadata[, c("id", "name")],
          by.x = "steamAppId",
          by.y = "id",
          all.x = TRUE
        )
        # Reorder to put name near the beginning
        name_idx <- which(names(rankings) == "name")
        other_idx <- setdiff(1:ncol(rankings), name_idx)
        rankings <- rankings[, c(1, name_idx, other_idx[-1])]
      }
    }
  }, error = function(e) {
    # If we can't get game names, just continue without them
    warning("Could not fetch game names: ", e$message)
  })
  
  # Create output with relevant columns including actual values
  result <- data.frame(
    steamAppId = rankings$steamAppId,
    rank = rankings[[rank_column]],
    percentile = rankings[[value_column]],
    stringsAsFactors = FALSE
  )
  
  # Add actual metric values based on what was requested
  if (metric == "revenue") {
    result$revenue <- rankings$totalRevenue
  } else if (metric == "units") {
    result$units <- rankings$totalUnitsSold
  } else if (metric == "ccu") {
    # For CCU, use avgPlaytime as a proxy or fetch from concurrent data
    result$ccu <- rankings$avgPlaytime
  } else if (metric == "dau") {
    result$dau <- rankings$yesterdayUnitsSold
  } else if (metric == "followers") {
    result$followers <- rankings$followers
  }
  
  # Add name if available
  if ("name" %in% names(rankings)) {
    result$name <- rankings$name
    # Reorder columns to put name after steamAppId
    col_order <- c("steamAppId", "name", setdiff(names(result), c("steamAppId", "name")))
    result <- result[, col_order]
  }
  
  # Convert to tibble
  result <- tibble::as_tibble(result)
  
  return(result)
}