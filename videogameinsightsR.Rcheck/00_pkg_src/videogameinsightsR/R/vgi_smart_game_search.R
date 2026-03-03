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
  
  if (search_type == "publisher") {
    publisher_id <- metadata$publisher_id %||% NULL
    publisher_name <- metadata$publisher_name %||% NULL
    
    # If not in direct fields, check publishers field
    if (is.null(publisher_id) || is.null(publisher_name)) {
      if (!is.null(metadata$publishers) && length(metadata$publishers) > 0) {
        if (is.list(metadata$publishers)) {
          publisher_id <- metadata$publishers[[1]]$id
          publisher_name <- metadata$publishers[[1]]$name
        } else {
          publisher_name <- metadata$publishers[1]
          # Need to look up publisher by name
          pub_list <- vgi_publisher_list(auth_token = auth_token, headers = headers)
          pub_match <- pub_list[pub_list$name == publisher_name, ]
          if (nrow(pub_match) > 0) {
            publisher_id <- pub_match$id[1]
          }
        }
      }
    }
    
    if (is.null(publisher_id) || is.null(publisher_name)) {
      stop("No publisher information found for ", seed_game$name)
    }
    
    message(sprintf("  Publisher: %s (ID: %s)", publisher_name, publisher_id))
    
    # Step 3: Get ALL games from this publisher
    message("  Fetching all games from this publisher...")
    
    # Get publisher's game IDs
    pub_games <- vgi_all_publisher_games(auth_token = auth_token, headers = headers)
    pub_row <- pub_games[pub_games$publisher_id == publisher_id, ]
    
    if (nrow(pub_row) == 0) {
      stop("No games found for publisher ID ", publisher_id)
    }
    
    game_ids <- unlist(pub_row$game_ids)
    message(sprintf("  Found %d games from %s", length(game_ids), publisher_name))
    
    if (length(game_ids) > limit) {
      game_ids <- game_ids[1:limit]
      message(sprintf("  Limiting to first %d games", limit))
    }
    
    games_data <- purrr::map_dfr(game_ids, function(id) {
      tryCatch({
        meta <- vgi_game_metadata(id, auth_token = auth_token, headers = headers)
        tibble::tibble(
          steamAppId = as.integer(id),
          name = as.character(meta$name %||% paste0("Game_", id)),
          publisher = publisher_name,
          releaseDate = as.Date(meta$release_date %||% NA)
        )
      }, error = function(e) {
        tibble::tibble(
          steamAppId = as.integer(id),
          name = paste0("Game_", id),
          publisher = publisher_name,
          releaseDate = as.Date(NA)
        )
      })
    })
    
  } else {  # search_type == "developer"
    # Get developer ID and name - check multiple possible field names
    developer_id <- metadata$developer_id %||% NULL
    developer_name <- metadata$developer_name %||% NULL
    
    # If not in direct fields, check developers field
    if (is.null(developer_id) || is.null(developer_name)) {
      if (!is.null(metadata$developers) && length(metadata$developers) > 0) {
        if (is.list(metadata$developers)) {
          developer_id <- metadata$developers[[1]]$id
          developer_name <- metadata$developers[[1]]$name
        } else {
          developer_name <- metadata$developers[1]
          # Need to look up developer by name
          dev_list <- vgi_developer_list(auth_token = auth_token, headers = headers)
          dev_match <- dev_list[dev_list$name == developer_name, ]
          if (nrow(dev_match) > 0) {
            developer_id <- dev_match$id[1]
          }
        }
      }
    }
    
    if (is.null(developer_id) || is.null(developer_name)) {
      stop("No developer information found for ", seed_game$name)
    }
    
    message(sprintf("  Developer: %s (ID: %s)", developer_name, developer_id))
    
    # Step 3: Get ALL games from this developer
    message("  Fetching all games from this developer...")
    
    # Get developer's game IDs
    dev_games <- vgi_all_developer_games(auth_token = auth_token, headers = headers)
    dev_row <- dev_games[dev_games$developer_id == developer_id, ]
    
    if (nrow(dev_row) == 0) {
      stop("No games found for developer ID ", developer_id)
    }
    
    game_ids <- unlist(dev_row$game_ids)
    message(sprintf("  Found %d games from %s", length(game_ids), developer_name))
    
    if (length(game_ids) > limit) {
      game_ids <- game_ids[1:limit]
      message(sprintf("  Limiting to first %d games", limit))
    }
    
    games_data <- purrr::map_dfr(game_ids, function(id) {
      tryCatch({
        meta <- vgi_game_metadata(id, auth_token = auth_token, headers = headers)
        tibble::tibble(
          steamAppId = as.integer(id),
          name = as.character(meta$name %||% paste0("Game_", id)),
          developer = developer_name,
          releaseDate = as.Date(meta$release_date %||% NA)
        )
      }, error = function(e) {
        tibble::tibble(
          steamAppId = as.integer(id),
          name = paste0("Game_", id),
          developer = developer_name,
          releaseDate = as.Date(NA)
        )
      })
    })
  }
  
  # Sort by release date (newest first)
  games_data <- games_data[order(games_data$release_date, decreasing = TRUE), ]
  
  message(sprintf("\nFound %d games from the same %s", nrow(games_data), search_type))
  
  return(.vgi_clean_names(games_data))
}