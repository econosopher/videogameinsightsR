#' Smart Game Search using Publisher/Developer Relationships
#'
#' This function performs intelligent game searches by finding one game that matches
#' your query, then retrieving ALL games from the same publisher or developer.
#' This is the RIGHT way to search for games - not by searching through a massive cached list.
#'
#' @param query Character string. The search query (partial game name).
#' @param search_type Character. Either "publisher" or "developer" to determine
#'   which relationship to follow. Defaults to "publisher".
#' @param limit Integer. Maximum number of results to return. Defaults to 50.
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A tibble containing games from the same publisher/developer with columns:
#' \describe{
#'   \item{steamAppId}{Integer. The Steam App ID}
#'   \item{name}{Character. The game name}
#'   \item{publisher}{Character. Publisher name (if search_type="publisher")}
#'   \item{developer}{Character. Developer name (if search_type="developer")}
#'   \item{releaseDate}{Date. Game release date}
#' }
#'
#' @details
#' This function implements the CORRECT way to search for games:
#' 1. First, it finds ONE game that matches your query (from cache or API)
#' 2. Then it gets the publisher or developer ID from that game's metadata
#' 3. Finally, it retrieves ALL games from that publisher/developer
#' 
#' This is much smarter than searching through thousands of cached games
#' and actually uses the API's relationship structure properly.
#' 
#' Example: Search for "Battlefield" -> finds one Battlefield game -> 
#' gets EA's publisher ID -> returns ALL EA games including all Battlefields.
#'
#' @export
#' @examples
#' \dontrun{
#' # Find all games from Battlefield's publisher (EA)
#' ea_games <- vgi_smart_game_search("battlefield", search_type = "publisher")
#' print(ea_games)
#' 
#' # Find all games from the developer of Gothic
#' gothic_dev_games <- vgi_smart_game_search("gothic", search_type = "developer")
#' print(gothic_dev_games)
#' }
vgi_smart_game_search <- function(query,
                                  search_type = c("publisher", "developer"),
                                  limit = 50,
                                  auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                                  headers = list()) {
  
  search_type <- match.arg(search_type)
  
  # Validate inputs
  if (!is.character(query) || length(query) != 1 || nchar(query) == 0) {
    stop("Query must be a non-empty character string")
  }
  
  message(sprintf("Smart search: Finding games related to '%s' by %s", query, search_type))
  
  # Step 1: Find ONE game that matches the query
  # Try cache first for speed
  package_cache <- system.file("extdata", "game_cache.rds", package = "videogameinsightsR")
  
  seed_game <- NULL
  
  if (file.exists(package_cache)) {
    games_cache <- .vgi_clean_names(readRDS(package_cache))
    matches <- grepl(query, games_cache$name, ignore.case = TRUE)
    if (any(matches)) {
      seed_game <- games_cache[which(matches)[1], ]
      message(sprintf("  Found seed game from cache: %s (ID: %d)", 
                      seed_game$name, seed_game$steam_app_id))
    }
  }
  
  # If not in cache, try the API game list (last resort)
  if (is.null(seed_game)) {
    message("  No match in cache, fetching from API...")
    tryCatch({
      all_games <- vgi_game_list(auth_token = auth_token, headers = headers)
      matches <- grepl(query, all_games$name, ignore.case = TRUE)
      if (any(matches)) {
        seed_game <- all_games[which(matches)[1], ]
        message(sprintf("  Found seed game from API: %s (ID: %d)", 
                        seed_game$name, seed_game$steam_app_id))
      }
    }, error = function(e) {
      stop("Could not find any game matching '", query, "'")
    })
  }
  
  if (is.null(seed_game)) {
    stop("No games found matching '", query, "'")
  }
  
  # Step 2: Get the game's metadata to find publisher/developer
  message("  Fetching metadata for seed game...")
  metadata <- vgi_game_metadata(seed_game$steam_app_id, auth_token = auth_token, headers = headers)
  company_id <- metadata[[paste0(search_type, "_id")]]
  company_name <- metadata[[paste0(search_type, "_name")]]
  if (length(company_id) == 0 || is.na(company_id[1])) {
    stop("No ", search_type, " information found for ", seed_game$name)
  }
  company_id <- company_id[1]
  company_name <- company_name[1]
  message(sprintf("  %s: %s (ID: %s)", tools::toTitleCase(search_type), company_name, company_id))

  # Step 3: Get the company's Steam games
  message(sprintf("  Fetching all games from this %s...", search_type))
  game_ids <- if (search_type == "publisher") {
    vgi_publisher_games(company_id, auth_token = auth_token, headers = headers)
  } else {
    vgi_developer_games(company_id, auth_token = auth_token, headers = headers)
  }
  if (length(game_ids) == 0) stop("No games found for ", search_type, " ID ", company_id)
  message(sprintf("  Found %d games from %s", length(game_ids), company_name))
  if (length(game_ids) > limit) {
    game_ids <- game_ids[seq_len(limit)]
    message(sprintf("  Limiting to first %d games", limit))
  }

  meta <- suppressWarnings(vgi_game_metadata_batch(game_ids, auth_token = auth_token, headers = headers))
  games_data <- tibble::tibble(
    steamAppId = as.integer(game_ids),
    name = meta$name[match(game_ids, meta$steam_app_id)],
    releaseDate = as.Date(meta$release_date_steam[match(game_ids, meta$steam_app_id)])
  )
  games_data[[search_type]] <- company_name
  games_data <- games_data[order(games_data$releaseDate, decreasing = TRUE), , drop = FALSE]

  message(sprintf("\nFound %d games from the same %s", nrow(games_data), search_type))

  return(.vgi_clean_names(games_data))
}
