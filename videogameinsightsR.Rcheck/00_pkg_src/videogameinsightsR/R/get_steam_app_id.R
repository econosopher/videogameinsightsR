#' Get Steam App ID from Steam Store Search
#'
#' Searches the Steam store and returns the app ID of the first result.
#' Useful for finding Steam IDs when you only know the game name.
#'
#' @param game_name Character string. The game name to search for.
#' @return Integer. The Steam App ID of the first search result, or NULL if not found.
#' @export
#' @examples
#' \dontrun{
#' # Find Steam ID for a game
#' bf_id <- get_steam_app_id("Battlefield 2042 Open Beta")
#' print(bf_id)
#' }
get_steam_app_id <- function(game_name) {
  requireNamespace("httr2", quietly = TRUE)
  requireNamespace("rvest", quietly = TRUE)
  
  # URL encode the search term
  search_term <- URLencode(game_name)
  
  # Steam store search URL
  search_url <- paste0("https://store.steampowered.com/search/?term=", search_term)
  
  tryCatch({
    # Make request
    resp <- request(search_url) |>
      req_user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36") |>
      req_perform()
    
    # Parse HTML
    html_content <- resp_body_string(resp)
    page <- read_html(html_content)
    
    # Find first search result link
    # Steam search results have links like: /app/1517290/Battlefield_2042/
    first_result <- page |>
      html_node("a.search_result_row") |>
      html_attr("data-ds-appid")
    
    if (!is.na(first_result)) {
      app_id <- as.integer(first_result)
      message(sprintf("Found Steam App ID %d for '%s'", app_id, game_name))
      return(app_id)
    } else {
      # Try alternative parsing
      first_link <- page |>
        html_node("a.search_result_row") |>
        html_attr("href")
      
      if (!is.na(first_link)) {
        # Extract app ID from URL
        matches <- regmatches(first_link, regexec("/app/([0-9]+)/", first_link))
        if (length(matches[[1]]) > 1) {
          app_id <- as.integer(matches[[1]][2])
          message(sprintf("Found Steam App ID %d for '%s'", app_id, game_name))
          return(app_id)
        }
      }
    }
    
    message(sprintf("No Steam App ID found for '%s'", game_name))
    return(NULL)
    
  }, error = function(e) {
    message(sprintf("Error searching for '%s': %s", game_name, e$message))
    return(NULL)
  })
}