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
#' @details
#' Note: The API does not currently provide server-side filtering by platform
#' or direct CCU/DAU top lists. This function constructs top lists from the
#' rankings endpoint, which may not perfectly reflect CCU/DAU. Treat these as
#' approximate until the API supports direct metrics.
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
  
  # Get rankings data (no platform/date filtering available in the API)
  rankings <- vgi_game_rankings(
    limit = limit * 2,  # Get more than needed to account for filtering
    auth_token = auth_token, 
    headers = headers
  )
  
  if (nrow(rankings) == 0) {
    return(.vgi_clean_names(tibble::tibble()))
  }
  
  # Determine which column to sort by based on metric
  rank_column <- switch(metric,
    revenue = "total_revenue_rank",
    units = "total_units_sold_rank",
    ccu = "avg_playtime_rank",  # proxy until API provides CCU ranking
    dau = "yesterday_units_sold_rank",  # proxy until API provides DAU ranking
    followers = "followers_rank"
  )
  value_column <- sub("_rank$", "_prct", rank_column)

  # Filter out rows where the rank column is NA, sort ascending (1 = best)
  rankings <- rankings[!is.na(rankings[[rank_column]]), , drop = FALSE]
  rankings <- rankings[order(rankings[[rank_column]]), , drop = FALSE]
  if (nrow(rankings) > limit) rankings <- rankings[seq_len(limit), , drop = FALSE]

  result <- tibble::tibble(
    steam_app_id = rankings$steam_app_id,
    rank = rankings[[rank_column]],
    percentile = rankings[[value_column]]
  )
  result$value <- result$percentile

  # Try to add game names by fetching metadata for the specific games
  if (nrow(result) > 0) {
    names_df <- tryCatch({
      meta <- vgi_game_metadata_batch(result$steam_app_id, auth_token = auth_token, headers = headers)
      if (is.data.frame(meta) && nrow(meta) > 0 && all(c("steam_app_id", "name") %in% names(meta))) {
        meta[, c("steam_app_id", "name")]
      } else {
        NULL
      }
    }, error = function(e) {
      warning("Could not fetch game names: ", e$message)
      NULL
    })
    if (!is.null(names_df)) {
      result$name <- names_df$name[match(result$steam_app_id, names_df$steam_app_id)]
      result <- result[, c("steam_app_id", "name", "rank", "percentile", "value")]
    }
  }

  .vgi_clean_names(result)
}
