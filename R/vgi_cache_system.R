#' Video Game Insights Smart Cache System
#' 
#' Provides intelligent caching for Steam App IDs and game metadata to minimize API calls
#' while enabling local filtering by genre, platform, and other attributes.

# Cache directory setup
.vgi_cache_dir <- function() {
  configured_cache <- getOption("videogameinsightsR.cache_dir", Sys.getenv("VGI_CACHE_DIR", ""))
  base_dir <- if (!is.null(configured_cache) && nzchar(configured_cache)) {
    configured_cache
  } else {
    tempdir()
  }
  cache_dir <- file.path(base_dir, "vgi_cache")
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }
  return(cache_dir)
}

#' Build Complete Game Cache
#'
#' Fetches all games from the API and caches them locally with metadata.
#' This is a one-time operation that should be run periodically to update the cache.
#'
#' @param force_refresh Logical. Force refresh even if cache exists. Default FALSE.
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

  message("Building game cache from /games/metadata. This may take several minutes...")
  cache_df <- vgi_games_metadata(limit = 1000, all_pages = TRUE, auth_token = auth_token)
  attr(cache_df, "next_cursor") <- NULL
  message(sprintf("Found %d games", nrow(cache_df)))

  attr(cache_df, "cache_time") <- Sys.time()
  attr(cache_df, "cache_version") <- "2.0"

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
  
  return(.vgi_clean_names(cache_df))
}

#' Filter Games by Genre
#'
#' Filter cached games by genre without hitting the API. VGI assigns each game
#' one `genre` (for example "Shooter"); Steam's own genres are in
#' `steam_genres` and `steam_subgenres`. `source` picks which to match.
#'
#' @param genre Character string. Regular expression matched case-insensitively
#'   (e.g. "Shooter", "RPG").
#' @param cache_df Optional pre-loaded cache. If NULL, will load automatically.
#' @param source `"vgi"` (default) matches VGI's `genre` and `subgenre`;
#'   `"steam"` matches `steam_genres` and `steam_subgenres`.
#'
#' @return A [tibble][tibble::tibble] of the matching games.
#' @export
#' @examples
#' \dontrun{
#' shooters <- vgi_filter_genre("Shooter")
#' steam_shooters <- vgi_filter_genre("Shooter", source = "steam")
#' }
vgi_filter_genre <- function(genre, cache_df = NULL, source = c("vgi", "steam")) {
  source <- match.arg(source)
  if (is.null(cache_df)) {
    cache_df <- vgi_load_cache()
  }
  cols <- if (source == "vgi") c("genre", "subgenre") else c("steam_genres", "steam_subgenres")
  cols <- intersect(cols, names(cache_df))
  if (length(cols) == 0) {
    warning("No genre information in cache. Run vgi_build_cache(force_refresh = TRUE)")
    return(.vgi_clean_names(cache_df[0, ]))
  }
  text <- do.call(paste, c(lapply(cols, function(cl) ifelse(is.na(cache_df[[cl]]), "", cache_df[[cl]])), sep = " | "))
  .vgi_clean_names(cache_df[grepl(genre, text, ignore.case = TRUE), , drop = FALSE])
}

#' Get Games by Steam App IDs
#'
#' Retrieve game information for specific Steam App IDs from the cache,
#' fetching any missing games from `/games/metadata`.
#'
#' @param steam_app_ids Integer vector. Steam App IDs to retrieve.
#' @param cache_df Optional pre-loaded cache. If NULL, will load automatically.
#' @param fetch_missing Logical. Fetch from API if not in cache. Default TRUE.
#'
#' @return A [tibble][tibble::tibble] with one row per game found.
#' @export
#' @examples
#' \dontrun{
#' games <- vgi_get_games_by_id(c(730, 578080, 1172470))
#' }
vgi_get_games_by_id <- function(steam_app_ids, cache_df = NULL, fetch_missing = TRUE) {
  if (is.null(cache_df)) {
    cache_df <- vgi_load_cache()
  }
  cached_games <- cache_df[!is.na(cache_df$steam_app_id) & cache_df$steam_app_id %in% steam_app_ids, , drop = FALSE]
  missing_ids <- setdiff(steam_app_ids, cached_games$steam_app_id)
  if (length(missing_ids) > 0 && fetch_missing) {
    message(sprintf("Fetching %d games not in cache...", length(missing_ids)))
    missing_df <- tryCatch(suppressWarnings(vgi_game_metadata_batch(missing_ids)), error = function(e) NULL)
    if (is.data.frame(missing_df) && nrow(missing_df) > 0) {
      cached_games <- dplyr::bind_rows(cached_games, missing_df)
    }
  }
  .vgi_clean_names(cached_games)
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
    message("No cache found.")
    return(invisible(NULL))
  }

  cache_df <- readRDS(cache_file)
  cache_time <- attr(cache_df, "cache_time")

  stats <- list(
    total_games = nrow(cache_df),
    cache_time = cache_time,
    cache_age_days = as.numeric(difftime(Sys.time(), cache_time, units = "days")),
    cache_size_mb = file.info(cache_file)$size / 1024^2,
    steam_games = if ("steam_app_id" %in% names(cache_df)) sum(!is.na(cache_df$steam_app_id)) else NA_integer_,
    genres = if ("genre" %in% names(cache_df)) sort(table(cache_df$genre), decreasing = TRUE) else integer(0)
  )

  message("Video Game Insights Cache Statistics")
  message("====================================")
  message(sprintf("Total games: %s", format(stats$total_games, big.mark = ",")))
  message(sprintf("Games on Steam: %s", format(stats$steam_games, big.mark = ",")))
  message(sprintf("Cache created: %s", format(stats$cache_time, "%Y-%m-%d %H:%M:%S")))
  message(sprintf("Cache age: %.1f days", stats$cache_age_days))
  message(sprintf("Cache size: %.1f MB", stats$cache_size_mb))
  if (length(stats$genres) > 0) {
    top <- head(stats$genres, 10)
    message(sprintf("Top genres: %s", paste(sprintf("%s (%d)", names(top), as.integer(top)), collapse = ", ")))
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
