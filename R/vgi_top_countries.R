#' Get Top Countries by Player Count
#'
#' Retrieve the top countries by player count for a specific game, showing
#' where the game's player base is concentrated geographically.
#'
#' @param steam_app_id Integer. The Steam App ID of the game.
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{country}{Character. Two-letter country code (ISO 3166-1 alpha-2)}
#'   \item{countryName}{Character. Full country name}
#'   \item{playerCount}{Integer. Number of players from this country}
#'   \item{percentage}{Numeric. Percentage of total player base}
#'   \item{rank}{Integer. Country rank by player count}
#' }
#'
#' @details
#' This endpoint provides insights into:
#' \itemize{
#'   \item Geographic distribution of your player base
#'   \item Key markets for localization efforts
#'   \item Regional marketing opportunities
#'   \item Server location planning
#' }
#' 
#' Countries are ranked by total player count, typically showing
#' the top 20-50 countries depending on the game's distribution.
#'
#' @export
#' @examples
#' \dontrun{
#' # Get top countries for a game
#' top_countries <- vgi_top_countries(steam_app_id = 730)
#' 
#' # Display top 10 countries
#' head(top_countries, 10)
#' 
#' # Calculate cumulative percentage
#' top_countries$cumulative_pct <- cumsum(top_countries$percentage)
#' 
#' # Find how many countries make up 80% of players
#' countries_80pct <- which(top_countries$cumulative_pct >= 80)[1]
#' cat("80% of players come from top", countries_80pct, "countries\n")
#' 
#' # Create a bar chart of top 10 countries
#' top10 <- head(top_countries, 10)
#' barplot(top10$percentage, 
#'         names.arg = top10$countryName,
#'         las = 2,
#'         main = "Top 10 Countries by Player %",
#'         ylab = "Percentage of Players",
#'         col = "steelblue")
#' 
#' # Check for specific regions
#' eu_countries <- c("DE", "FR", "GB", "IT", "ES", "PL", "NL", "SE", "BE", "AT")
#' eu_players <- sum(top_countries$percentage[top_countries$country %in% eu_countries])
#' cat("EU player percentage:", round(eu_players, 1), "%\n")
#' 
#' # Identify emerging markets
#' emerging <- top_countries[top_countries$rank > 10 & top_countries$percentage > 1, ]
#' cat("Emerging markets (rank >10 but >1%):", nrow(emerging), "\n")
#' print(emerging)
#' }
vgi_top_countries <- function(steam_app_id,
                             auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                             headers = list()) {
  
  # Validate inputs
  validate_numeric(steam_app_id, "steam_app_id")
  
  # Make API request
  result <- make_api_request(
    endpoint = paste0("player-insights/games/", steam_app_id, "/top-countries"),
    auth_token = auth_token,
    method = "GET",
    headers = headers
  )
  
  # Convert to data frame
  if (is.list(result) && length(result) > 0) {
    df <- do.call(rbind, lapply(seq_along(result), function(i) {
      x <- result[[i]]
      data.frame(
        country = as.character(x$country %||% NA),
        countryName = as.character(x$countryName %||% NA),
        playerCount = as.integer(x$playerCount %||% NA),
        percentage = as.numeric(x$percentage %||% NA),
        rank = i,  # Rank by order in response
        stringsAsFactors = FALSE
      )
    }))
    
    return(df)
  } else {
    # Return empty data frame with correct structure
    return(data.frame(
      country = character(),
      countryName = character(),
      playerCount = integer(),
      percentage = numeric(),
      rank = integer(),
      stringsAsFactors = FALSE
    ))
  }
}