#' Search Games in Video Game Insights
#'
#' Search for games by title using the Video Game Insights database.
#'
#' @param query Character string. The search query (game title).
#' @param limit Integer. Maximum number of results to return. Defaults to 10.
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
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
                            headers = list()) {
  
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
  
  # NOTE: The API does not reliably filter on the server. Fetch full list and filter locally.
  all_games <- tryCatch(
    vgi_game_list(auth_token = auth_token, headers = headers),
    error = function(e) tibble::tibble(steamAppId = integer(), name = character(), id = integer())
  )

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
  if (nrow(filtered) > limit) filtered <- filtered[seq_len(limit), ]
  tibble::as_tibble(filtered)
}