#' Video Game Insights Smart Cache System
#' 
#' Provides intelligent caching for Steam App IDs and game metadata to minimize API calls
#' while enabling local filtering by genre, platform, and other attributes.

# Cache directory setup
.vgi_cache_dir <- function() {
  cache_dir <- file.path(rappdirs::user_cache_dir("videogameinsightsR"), "vgi_cache")
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }
  return(cache_dir)
}

#' Build Complete Game Cache
#'
#' Fetches all games from the API and caches them locally with metadata.
#' This is a one-time operation that should be run periodically to update the cache.
#'
#' @param force_refresh Logical. Force refresh even if cache exists. Default FALSE.
#' @param batch_size Integer. Number of games to fetch metadata for at once. Default 100.
#' @param auth_token Character string. Your VGI API authentication token.
#'
#' @return Invisible TRUE on success
#' @export
#' @examples
#' \dontrun{
#' # Build initial cache
#' vgi_build_cache()
#' 
#' # Force refresh of cache
#' vgi_build_cache(force_refresh = TRUE)
#' }
vgi_build_cache <- function(force_refresh = FALSE, 
                           batch_size = 100,
                           auth_token = Sys.getenv("VGI_AUTH_TOKEN")) {
  
  cache_file <- file.path(.vgi_cache_dir(), "game_cache.rds")
  
  # Check if cache exists and is recent
  if (!force_refresh && file.exists(cache_file)) {
    cache_age <- difftime(Sys.time(), file.info(cache_file)$mtime, units = "days")
    if (cache_age < 7) {
      message("Cache is less than 7 days old. Use force_refresh = TRUE to update.")
      return(invisible(TRUE))
    }
  }
  
  message("Building game cache. This may take several minutes...")
  
  # Step 1: Get all games (paginated)
  all_games <- list()
  offset <- 0
  limit <- 1000  # Max allowed
  
  repeat {
    message(sprintf("Fetching games %d-%d...", offset + 1, offset + limit))
    
    games_batch <- vgi_game_list()
    
    if (nrow(games_batch) == 0) break
    
    all_games[[length(all_games) + 1]] <- games_batch
    
    if (nrow(games_batch) < limit) break
    offset <- offset + limit
  }
  
  # Combine all games
  games_df <- do.call(rbind, all_games)
  message(sprintf("Found %d games total", nrow(games_df)))
  
  # Step 2: Get rankings to have additional data
  message("Fetching game rankings...")
  rankings <- list()
  offset <- 0
  
  repeat {
    rankings_batch <- vgi_game_rankings(offset = offset, limit = 1000)
    if (nrow(rankings_batch) == 0) break
    rankings[[length(rankings) + 1]] <- rankings_batch
    if (nrow(rankings_batch) < 1000) break
    offset <- offset + 1000
  }
  
  rankings_df <- do.call(rbind, rankings)
  
  # Merge games with rankings
  cache_df <- merge(games_df, rankings_df, by.x = "id", by.y = "steamAppId", all.x = TRUE)
  names(cache_df)[names(cache_df) == "id"] <- "steamAppId"
  
  # Step 3: Fetch metadata for games in batches
  message("Fetching metadata for all games...")
  
  # Split into batches
  game_ids <- unique(cache_df$steamAppId)
  batches <- split(game_ids, ceiling(seq_along(game_ids) / batch_size))
  
  metadata_list <- list()
  
  for (i in seq_along(batches)) {
    batch_ids <- batches[[i]]
    message(sprintf("Processing batch %d/%d (%d games)...", i, length(batches), length(batch_ids)))
    
    # Use batch metadata function
    tryCatch({
      batch_metadata <- vgi_game_metadata_batch(batch_ids)
      metadata_list[[i]] <- batch_metadata
    }, error = function(e) {
      # If batch fails, try individual
      message("Batch failed, trying individual fetches...")
      individual_meta <- list()
      
      for (game_id in batch_ids) {
        tryCatch({
          meta <- vgi_game_metadata(game_id)
          individual_meta[[as.character(game_id)]] <- meta
        }, error = function(e) {
          # Skip failed games
        })
      }
      
      if (length(individual_meta) > 0) {
        metadata_list[[i]] <- do.call(rbind, individual_meta)
      }
    })
    
    # Be nice to the API
    Sys.sleep(0.5)
  }
  
  # Combine all metadata
  if (length(metadata_list) > 0) {
    metadata_df <- do.call(rbind, metadata_list)
    
    # Merge with cache
    cache_df <- merge(cache_df, metadata_df, 
                      by = "steamAppId", 
                      all.x = TRUE, 
                      suffixes = c("", "_meta"))
  }
  
  # Add cache metadata
  attr(cache_df, "cache_time") <- Sys.time()
  attr(cache_df, "cache_version") <- "1.0"
  
  # Save cache
  saveRDS(cache_df, cache_file)
  message(sprintf("Cache saved to %s", cache_file))
  
  return(invisible(TRUE))
}

#' Load Game Cache
#'
#' Loads the cached game database.
#'
#' @param auto_build Logical. Automatically build cache if it doesn't exist. Default TRUE.
#'
#' @return Data frame with all cached games and metadata
#' @export
vgi_load_cache <- function(auto_build = TRUE) {
  cache_file <- file.path(.vgi_cache_dir(), "game_cache.rds")
  
  if (!file.exists(cache_file)) {
    if (auto_build) {
      message("Cache not found. Building initial cache...")
      vgi_build_cache()
    } else {
      stop("Cache not found. Run vgi_build_cache() first.")
    }
  }
  
  cache_df <- readRDS(cache_file)
  cache_time <- attr(cache_df, "cache_time")
  
  if (!is.null(cache_time)) {
    cache_age <- difftime(Sys.time(), cache_time, units = "days")
    if (cache_age > 7) {
      message(sprintf("Cache is %.1f days old. Consider running vgi_build_cache(force_refresh = TRUE)", 
                      as.numeric(cache_age)))
    }
  }
  
  return(cache_df)
}

#' Filter Games by Genre
#'
#' Filter cached games by genre without hitting the API.
#'
#' @param genre Character string. Genre to filter by (e.g., "Shooter", "RPG")
#' @param cache_df Optional pre-loaded cache. If NULL, will load automatically.
#'
#' @return Data frame of games matching the genre
#' @export
#' @examples
#' \dontrun{
#' # Get all shooters
#' shooters <- vgi_filter_genre("Shooter")
#' 
#' # Get top 10 shooters by revenue rank
#' top_shooters <- shooters %>%
#'   arrange(totalRevenueRank) %>%
#'   head(10)
#' }
vgi_filter_genre <- function(genre, cache_df = NULL) {
  if (is.null(cache_df)) {
    cache_df <- vgi_load_cache()
  }
  
  # Handle genre column (might be a list or character vector)
  if ("genres" %in% names(cache_df)) {
    # Filter by genre
    matches <- sapply(cache_df$genres, function(g) {
      if (is.character(g)) {
        grepl(genre, g, ignore.case = TRUE)
      } else if (is.list(g)) {
        any(grepl(genre, unlist(g), ignore.case = TRUE))
      } else {
        FALSE
      }
    })
    
    return(cache_df[matches, ])
  } else {
    warning("No genre information in cache. Run vgi_build_cache(force_refresh = TRUE)")
    return(cache_df[0, ])
  }
}

#' Get Games by Steam App IDs
#'
#' Efficiently retrieve game information for specific Steam App IDs using cache.
#'
#' @param steam_app_ids Integer vector. Steam App IDs to retrieve.
#' @param cache_df Optional pre-loaded cache. If NULL, will load automatically.
#' @param fetch_missing Logical. Fetch from API if not in cache. Default TRUE.
#'
#' @return Data frame with game information
#' @export
#' @examples
#' \dontrun{
#' # Get specific games
#' games <- vgi_get_games_by_id(c(730, 578080, 1172470))
#' 
#' # Get games with rankings
#' popular_ids <- c(730, 570, 440, 1245620)
#' popular_games <- vgi_get_games_by_id(popular_ids)
#' }
vgi_get_games_by_id <- function(steam_app_ids, cache_df = NULL, fetch_missing = TRUE) {
  if (is.null(cache_df)) {
    cache_df <- vgi_load_cache()
  }
  
  # Get games from cache
  cached_games <- cache_df[cache_df$steamAppId %in% steam_app_ids, ]
  
  # Check for missing games
  missing_ids <- setdiff(steam_app_ids, cached_games$steamAppId)
  
  if (length(missing_ids) > 0 && fetch_missing) {
    message(sprintf("Fetching %d games not in cache...", length(missing_ids)))
    
    # Fetch missing games
    missing_games <- list()
    for (id in missing_ids) {
      tryCatch({
        meta <- vgi_game_metadata(id)
        # Convert to data frame row
        missing_games[[as.character(id)]] <- data.frame(
          steamAppId = id,
          name = meta$name %||% NA,
          genres = paste(meta$genres, collapse = ", "),
          stringsAsFactors = FALSE
        )
      }, error = function(e) {
        # Skip failed games
      })
    }
    
    if (length(missing_games) > 0) {
      missing_df <- do.call(rbind, missing_games)
      # Combine with cached results
      cached_games <- rbind(cached_games, missing_df, fill = TRUE)
    }
  }
  
  return(cached_games)
}

#' Get Cache Statistics
#'
#' Display information about the current cache.
#'
#' @return Invisible list with cache statistics
#' @export
vgi_cache_stats <- function() {
  cache_file <- file.path(.vgi_cache_dir(), "game_cache.rds")
  
  if (!file.exists(cache_file)) {
    cat("No cache found.\n")
    return(invisible(NULL))
  }
  
  cache_df <- readRDS(cache_file)
  cache_time <- attr(cache_df, "cache_time")
  
  stats <- list(
    total_games = nrow(cache_df),
    cache_time = cache_time,
    cache_age_days = as.numeric(difftime(Sys.time(), cache_time, units = "days")),
    cache_size_mb = file.info(cache_file)$size / 1024^2,
    has_rankings = sum(!is.na(cache_df$totalRevenueRank)),
    has_genres = "genres" %in% names(cache_df),
    unique_genres = if ("genres" %in% names(cache_df)) {
      unique(unlist(strsplit(paste(cache_df$genres, collapse = ", "), ", ")))
    } else {
      character(0)
    }
  )
  
  cat("Video Game Insights Cache Statistics\n")
  cat("====================================\n")
  cat(sprintf("Total games: %s\n", format(stats$total_games, big.mark = ",")))
  cat(sprintf("Cache created: %s\n", format(stats$cache_time, "%Y-%m-%d %H:%M:%S")))
  cat(sprintf("Cache age: %.1f days\n", stats$cache_age_days))
  cat(sprintf("Cache size: %.1f MB\n", stats$cache_size_mb))
  cat(sprintf("Games with rankings: %s\n", format(stats$has_rankings, big.mark = ",")))
  if (stats$has_genres && length(stats$unique_genres) > 0) {
    cat(sprintf("Unique genres: %d\n", length(stats$unique_genres)))
    cat("Top genres:", paste(head(sort(table(stats$unique_genres), decreasing = TRUE), 10), collapse = ", "), "\n")
  }
  
  return(invisible(stats))
}

#' Clear VGI Cache
#'
#' Remove the cached game database.
#'
#' @param confirm Logical. Require confirmation. Default TRUE.
#'
#' @return Invisible TRUE
#' @export
vgi_clear_cache <- function(confirm = TRUE) {
  cache_file <- file.path(.vgi_cache_dir(), "game_cache.rds")
  
  if (!file.exists(cache_file)) {
    message("No cache to clear.")
    return(invisible(TRUE))
  }
  
  if (confirm) {
    response <- readline("Are you sure you want to clear the cache? (yes/no): ")
    if (tolower(response) != "yes") {
      message("Cache clearing cancelled.")
      return(invisible(FALSE))
    }
  }
  
  unlink(cache_file)
  message("Cache cleared.")
  return(invisible(TRUE))
}