#' Smart Fetch Functions for VGI API
#' 
#' These functions handle pagination and ID-based fetching intelligently

#' Fetch All Games with Pagination
#'
#' Fetches all games from the API by handling pagination automatically.
#' Results are cached to avoid repeated API calls.
#'
#' @param cache_results Logical. Cache results to disk. Default TRUE.
#' @param verbose Logical. Print progress messages. Default TRUE.
#' @param auth_token Character string. Your VGI API authentication token.
#'
#' @return Data frame with all games
#' @export
vgi_fetch_all_games <- function(cache_results = TRUE, 
                               verbose = TRUE,
                               auth_token = Sys.getenv("VGI_AUTH_TOKEN")) {
  
  if (cache_results) {
    cache_file <- file.path(tempdir(), "vgi_all_games_temp.rds")
    if (file.exists(cache_file)) {
      cache_age <- difftime(Sys.time(), file.info(cache_file)$mtime, units = "hours")
      if (cache_age < 24) {
        if (verbose) message("Using cached game list (less than 24 hours old)")
        return(readRDS(cache_file))
      }
    }
  }
  
  all_games <- list()
  offset <- 0
  limit <- 1000  # Maximum allowed by API
  total_fetched <- 0
  
  if (verbose) message("Fetching all games from API...")
  
  repeat {
    # Fetch batch
    games_batch <- vgi_game_rankings(offset = offset, limit = limit)
    
    if (nrow(games_batch) == 0) break
    
    all_games[[length(all_games) + 1]] <- games_batch
    total_fetched <- total_fetched + nrow(games_batch)
    
    if (verbose) {
      message(sprintf("Fetched %d games (total: %d)", nrow(games_batch), total_fetched))
    }
    
    # Check if we got less than limit (indicates last page)
    if (nrow(games_batch) < limit) break
    
    offset <- offset + limit
    
    # Be nice to the API
    Sys.sleep(0.1)
  }
  
  # Combine all results
  all_games_df <- do.call(rbind, all_games)
  
  if (cache_results && nrow(all_games_df) > 0) {
    saveRDS(all_games_df, cache_file)
    if (verbose) message(sprintf("Cached %d games to temp file", nrow(all_games_df)))
  }
  
  return(all_games_df)
}

#' Fetch Games by ID List
#'
#' Efficiently fetches game data for a list of Steam App IDs.
#' Uses caching and batching to minimize API calls.
#'
#' @param steam_app_ids Integer vector. List of Steam App IDs to fetch.
#' @param data_type Character. Type of data to fetch: "metadata", "rankings", "active_players"
#' @param date Character or Date. Required for "active_players" data type.
#' @param use_cache Logical. Use local cache if available. Default TRUE.
#' @param auth_token Character string. Your VGI API authentication token.
#'
#' @return Data frame with requested game data
#' @export
#' @examples
#' \dontrun{
#' # Get metadata for specific games
#' games <- vgi_fetch_by_ids(c(730, 578080, 1172470), "metadata")
#' 
#' # Get active players for specific games
#' active <- vgi_fetch_by_ids(c(730, 578080), "active_players", date = "2025-07-26")
#' }
vgi_fetch_by_ids <- function(steam_app_ids, 
                            data_type = c("metadata", "rankings", "active_players"),
                            date = NULL,
                            use_cache = TRUE,
                            auth_token = Sys.getenv("VGI_AUTH_TOKEN")) {
  
  data_type <- match.arg(data_type)
  
  # Remove duplicates
  steam_app_ids <- unique(steam_app_ids)
  
  if (data_type == "rankings") {
    # For rankings, we need to fetch all and filter
    all_rankings <- vgi_fetch_all_games(cache_results = use_cache)
    return(all_rankings[all_rankings$steamAppId %in% steam_app_ids, ])
  }
  
  if (data_type == "active_players") {
    if (is.null(date)) stop("Date is required for active_players data type")
    
    # Fetch active players for date
    all_active <- vgi_active_players_by_date(date, limit = 1000)
    
    # Filter for our IDs
    return(all_active[all_active$steamAppId %in% steam_app_ids, ])
  }
  
  if (data_type == "metadata") {
    # Try batch fetch first
    result <- tryCatch({
      vgi_game_metadata_batch(steam_app_ids)
    }, error = function(e) {
      # Fall back to individual fetches
      message("Batch metadata fetch failed, trying individual fetches...")
      
      metadata_list <- list()
      success_count <- 0
      
      for (id in steam_app_ids) {
        tryCatch({
          meta <- vgi_game_metadata(id)
          metadata_list[[as.character(id)]] <- data.frame(
            steamAppId = id,
            name = meta$name,
            genres = paste(meta$genres, collapse = ", "),
            releaseDate = meta$releaseDate,
            developer = meta$developer,
            publisher = meta$publisher,
            stringsAsFactors = FALSE
          )
          success_count <- success_count + 1
        }, error = function(e) {
          # Skip failed games
        })
        
        # Progress indicator
        if (length(steam_app_ids) > 10 && which(steam_app_ids == id) %% 10 == 0) {
          message(sprintf("Progress: %d/%d games fetched", 
                         which(steam_app_ids == id), length(steam_app_ids)))
        }
      }
      
      if (length(metadata_list) > 0) {
        do.call(rbind, metadata_list)
      } else {
        data.frame()
      }
    })
    
    return(result)
  }
}

#' Smart Game Search with Filtering
#'
#' Searches for games using local cache and smart filtering to minimize API calls.
#'
#' @param genre Character. Genre to filter by (optional).
#' @param developer Character. Developer to filter by (optional).
#' @param publisher Character. Publisher to filter by (optional).
#' @param min_rank Integer. Minimum rank threshold (optional).
#' @param metric Character. Ranking metric to use: "revenue", "units", "followers"
#' @param limit Integer. Maximum number of results to return.
#'
#' @return Data frame with filtered games
#' @export
#' @examples
#' \dontrun{
#' # Get top 20 shooters by revenue
#' shooters <- vgi_smart_search(genre = "Shooter", metric = "revenue", limit = 20)
#' 
#' # Get all games by a specific developer
#' valve_games <- vgi_smart_search(developer = "Valve")
#' }
vgi_smart_search <- function(genre = NULL,
                            developer = NULL, 
                            publisher = NULL,
                            min_rank = NULL,
                            metric = c("revenue", "units", "followers"),
                            limit = 100) {
  
  metric <- match.arg(metric)
  
  # Get all games with rankings
  all_games <- vgi_fetch_all_games(verbose = FALSE)
  
  # Determine which rank column to use
  rank_col <- switch(metric,
    revenue = "totalRevenueRank",
    units = "totalUnitsSoldRank",
    followers = "followersRank"
  )
  
  # Apply rank filter if specified
  if (!is.null(min_rank)) {
    all_games <- all_games[!is.na(all_games[[rank_col]]) & 
                          all_games[[rank_col]] <= min_rank, ]
  }
  
  # Sort by rank
  all_games <- all_games[order(all_games[[rank_col]]), ]
  
  # If no other filters, return top N
  if (is.null(genre) && is.null(developer) && is.null(publisher)) {
    return(head(all_games, limit))
  }
  
  # We need metadata for filtering
  # Get IDs of top candidates (fetch more than limit to account for filtering)
  candidate_ids <- head(all_games$steamAppId, limit * 3)
  
  # Fetch metadata for candidates
  metadata <- vgi_fetch_by_ids(candidate_ids, "metadata")
  
  # Merge with rankings
  results <- merge(all_games, metadata, by = "steamAppId")
  
  # Apply filters
  if (!is.null(genre)) {
    results <- results[grepl(genre, results$genres, ignore.case = TRUE), ]
  }
  
  if (!is.null(developer)) {
    results <- results[grepl(developer, results$developer, ignore.case = TRUE), ]
  }
  
  if (!is.null(publisher)) {
    results <- results[grepl(publisher, results$publisher, ignore.case = TRUE), ]
  }
  
  # Return top N after filtering
  return(head(results, limit))
}

#' Get Top Games with Active Player Data
#'
#' Combines rankings with active player data for a specific date.
#'
#' @param date Character or Date. Date for active player data.
#' @param metric Character. Ranking metric to sort by.
#' @param limit Integer. Number of games to return.
#' @param genre Character. Optional genre filter.
#'
#' @return Data frame with games, rankings, and active player data
#' @export
vgi_top_games_with_activity <- function(date,
                                       metric = c("revenue", "units", "followers"),
                                       limit = 50,
                                       genre = NULL) {
  
  metric <- match.arg(metric)
  
  # Get top games by metric
  if (!is.null(genre)) {
    top_games <- vgi_smart_search(genre = genre, metric = metric, limit = limit * 2)
  } else {
    all_games <- vgi_fetch_all_games(verbose = FALSE)
    rank_col <- switch(metric,
      revenue = "totalRevenueRank",
      units = "totalUnitsSoldRank", 
      followers = "followersRank"
    )
    top_games <- head(all_games[order(all_games[[rank_col]]), ], limit * 2)
  }
  
  # Get active player data
  active_data <- vgi_active_players_by_date(date, limit = 1000)
  
  # Merge the data
  results <- merge(top_games, active_data, by = "steamAppId", all.x = TRUE)
  
  # Sort by DAU (if available) or by original metric
  if (sum(!is.na(results$dau)) > 0) {
    results <- results[order(-results$dau, results[[rank_col]]), ]
  }
  
  return(head(results, limit))
}