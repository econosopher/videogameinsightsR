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
  configured_cache <- getOption("videogameinsightsR.cache_dir", Sys.getenv("VGI_CACHE_DIR", ""))
  cache_base_dir <- if (!is.null(configured_cache) && nzchar(configured_cache)) {
    configured_cache
  } else {
    tempdir()
  }
  cache_file <- file.path(cache_base_dir, "vgi_all_games_cache.rds")
  
  if (cache_results) {
    if (file.exists(cache_file)) {
      cache_age <- difftime(Sys.time(), file.info(cache_file)$mtime, units = "hours")
      if (cache_age < 24) {
        if (verbose) message("Using cached game list (less than 24 hours old)")
        return(.vgi_clean_names(readRDS(cache_file)))
      }
    }
  }
  
  if (verbose) message("Fetching all games from API...")
  all_games_df <- vgi_game_list()
  if (verbose) message(sprintf("Fetched %d games", nrow(all_games_df)))
  
  if (cache_results && nrow(all_games_df) > 0) {
    if (!dir.exists(dirname(cache_file))) {
      dir.create(dirname(cache_file), recursive = TRUE, showWarnings = FALSE)
    }
    saveRDS(all_games_df, cache_file)
    if (verbose) message(sprintf("Cached %d games to %s", nrow(all_games_df), cache_file))
  }
  
  return(.vgi_clean_names(all_games_df))
}

#' Fetch Games by ID List
#'
#' Fetch game data for a list of Steam App IDs: metadata from
#' `/games/metadata`, or one day's active players from `/historical-data`.
#'
#' @param steam_app_ids Integer vector. List of Steam App IDs to fetch.
#' @param data_type Character. Type of data to fetch: "metadata" or
#'   "active_players".
#' @param date Character or Date. Required for "active_players".
#' @param auth_token Character string. Your VGI API authentication token.
#'
#' @return A [tibble][tibble::tibble] with the requested game data.
#' @export
#' @examples
#' \dontrun{
#' games <- vgi_fetch_by_ids(c(730, 578080, 1172470), "metadata")
#' active <- vgi_fetch_by_ids(c(730, 578080), "active_players", date = "2025-07-26")
#' }
vgi_fetch_by_ids <- function(steam_app_ids,
                            data_type = c("metadata", "active_players"),
                            date = NULL,
                            auth_token = Sys.getenv("VGI_AUTH_TOKEN")) {
  data_type <- match.arg(data_type)
  steam_app_ids <- unique(steam_app_ids)
  if (data_type == "active_players") {
    if (is.null(date)) stop("Date is required for active_players data type")
    return(vgi_active_players_by_date(date, steam_app_ids = steam_app_ids, auth_token = auth_token))
  }
  vgi_game_metadata_batch(steam_app_ids, auth_token = auth_token)
}
