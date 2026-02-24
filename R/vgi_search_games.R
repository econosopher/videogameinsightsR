#' Search Games in Video Game Insights
#'
#' Search for games by title using the Video Game Insights database.
#'
#' @param query Character string. The search query (game title).
#' @param limit Integer. Maximum number of results to return. Defaults to 10.
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#' @param allow_api_fallback Logical. If `TRUE`, falls back to the expensive
#'   `games/game-list` API endpoint when no local cache is available. Defaults
#'   to `TRUE` (set `options(vgi.search_allow_api_fallback = FALSE)` to disable).
#'
#' @return A [tibble][tibble::tibble] containing search results with game
#'   information including steamAppId and name.
#'
#' @examples
#' \dontrun{
#' # Ensure the VGI_AUTH_TOKEN environment variable is set
#' # Sys.setenv(VGI_AUTH_TOKEN = "your_auth_token_here")
#'
#' # Search for games with "valve" in the title
#' valve_games <- vgi_search_games("valve")
#' print(valve_games)
#'
#' # Search with more results
#' rpg_games <- vgi_search_games("rpg", limit = 50)
#' }
#'
#' @export
vgi_search_games <- function(query,
                            limit = 10,
                            auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                            headers = list(),
                            allow_api_fallback = getOption("vgi.search_allow_api_fallback", TRUE)) {
  
  # Validate inputs
  if (is.null(query) || query == "") {
    stop("query parameter is required")
  }
  
  # Validate limit
  if (!is.numeric(limit) || limit < 1 || limit > 1000) {
    if (is.numeric(limit) && limit < 1) stop("limit must be at least 1")
    if (is.numeric(limit) && limit > 1000) stop("limit must be at most 1000")
    stop("limit must be between 1 and 1000")
  }
  
  # Prefer local caches to avoid expensive full-list API calls.
  # The API fallback can be enabled explicitly with:
  # options(vgi.search_allow_api_fallback = TRUE)
  candidate_cache_paths <- c(
    system.file("extdata", "game_cache.rds", package = "videogameinsightsR"),
    file.path("inst", "extdata", "game_cache.rds"),
    file.path(rappdirs::user_cache_dir("videogameinsightsR"), "vgi_all_games_cache.rds")
  )
  candidate_cache_paths <- unique(candidate_cache_paths[nzchar(candidate_cache_paths)])

  all_games <- getOption("vgi.search_all_games_cache", NULL)
  if (!is.data.frame(all_games) || nrow(all_games) == 0) all_games <- NULL
  for (cache_path in candidate_cache_paths) {
    if (!is.null(all_games)) break
    if (!file.exists(cache_path)) next
    all_games <- tryCatch(
      readRDS(cache_path),
      error = function(e) NULL
    )
    if (is.data.frame(all_games) && nrow(all_games) > 0) break
  }

  if (is.null(all_games) || !is.data.frame(all_games) || nrow(all_games) == 0) {
    if (isTRUE(allow_api_fallback)) {
      all_games <- tryCatch(
        vgi_game_list(auth_token = auth_token, headers = headers),
        error = function(e) tibble::tibble(steamAppId = integer(), name = character(), id = integer())
      )
    } else {
      all_games <- tibble::tibble(steamAppId = integer(), name = character(), id = integer())
    }
  }

  if (!is.data.frame(all_games) || nrow(all_games) == 0) {
    return(tibble::tibble(steamAppId = integer(), name = character(), id = integer()))
  }

  # Ensure columns present
  if (!"name" %in% names(all_games)) all_games$name <- NA_character_
  if (!"steamAppId" %in% names(all_games) && "id" %in% names(all_games)) all_games$steamAppId <- as.integer(all_games$id)
  if (!"id" %in% names(all_games) && "steamAppId" %in% names(all_games)) all_games$id <- all_games$steamAppId

  # Local filter and limit
  name_col <- all_games$name
  name_col[is.na(name_col)] <- ""
  matches <- grepl(query, name_col, ignore.case = TRUE)
  filtered <- all_games[matches, c("steamAppId", "name", "id")]

  # v4 fallback: try metadata lookup via slug (cheap and targeted).
  if (nrow(filtered) == 0) {
    query_slug <- tolower(query)
    query_slug <- gsub("[^a-z0-9]+", "-", query_slug)
    query_slug <- gsub("(^-+|-+$)", "", query_slug)

    if (nzchar(query_slug)) {
      meta_resp <- tryCatch(
        make_api_request(
          endpoint = "games/metadata",
          query_params = list(slugs = query_slug, limit = max(10, as.integer(limit))),
          auth_token = auth_token,
          method = "GET",
          headers = headers
        ),
        error = function(e) NULL
      )
      meta_rows <- .vgi_unwrap_results(meta_resp)

      if (is.data.frame(meta_rows) && nrow(meta_rows) > 0) {
        steam_urls <- if ("storeUrl.steam" %in% names(meta_rows)) {
          meta_rows$storeUrl.steam
        } else {
          rep(NA_character_, nrow(meta_rows))
        }
        steam_ids <- vapply(steam_urls, .vgi_parse_steam_app_id, integer(1))
        meta_names <- if ("name" %in% names(meta_rows)) {
          as.character(meta_rows$name)
        } else {
          rep(NA_character_, nrow(meta_rows))
        }
        fallback <- tibble::tibble(
          steamAppId = as.integer(steam_ids),
          name = meta_names
        )
        fallback <- fallback[!is.na(fallback$steamAppId), , drop = FALSE]
        if (nrow(fallback) > 0) {
          fallback$id <- fallback$steamAppId
          # Keep exact/contains matches first.
          contains <- grepl(query, fallback$name, ignore.case = TRUE)
          if (any(contains)) fallback <- fallback[contains, , drop = FALSE]
          filtered <- fallback
        }
      }
    }
  }

  # Optional final fallback to full game list if explicitly allowed.
  if (nrow(filtered) == 0 && isTRUE(allow_api_fallback)) {
    all_games_api <- tryCatch(
      vgi_game_list(auth_token = auth_token, headers = headers),
      error = function(e) tibble::tibble(steamAppId = integer(), name = character(), id = integer())
    )
    if (is.data.frame(all_games_api) && nrow(all_games_api) > 0) {
      options(vgi.search_all_games_cache = all_games_api)
      if (!"id" %in% names(all_games_api) && "steamAppId" %in% names(all_games_api)) {
        all_games_api$id <- all_games_api$steamAppId
      }
      nm <- all_games_api$name
      nm[is.na(nm)] <- ""
      filtered <- all_games_api[grepl(query, nm, ignore.case = TRUE), c("steamAppId", "name", "id")]
    }
  }

  if (nrow(filtered) > limit) filtered <- filtered[seq_len(limit), ]
  tibble::as_tibble(filtered)
}