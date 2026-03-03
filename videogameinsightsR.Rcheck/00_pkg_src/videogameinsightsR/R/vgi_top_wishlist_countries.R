#' Get Top Countries by Wishlist Count
#'
#' Retrieve the top countries by wishlist count for a specific game, showing
#' where potential future players are concentrated.
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
#'   \item{wishlistCount}{Integer. Number of wishlists from this country}
#'   \item{percentage}{Numeric. Percentage of total wishlists}
#'   \item{rank}{Integer. Country rank by wishlist count}
#' }
#'
#' @details
#' Wishlist geographic data is valuable for:
#' \itemize{
#'   \item Pre-launch marketing focus
#'   \item Identifying high-interest regions
#'   \item Planning regional promotions
#'   \item Localization priorities for upcoming content
#'   \item Predicting launch day geographic distribution
#' }
#' 
#' Compare wishlist distribution with actual player distribution
#' to identify untapped markets or conversion opportunities.
#'
#' @export
#' @examples
#' \dontrun{
#' # Get top wishlist countries for a game
#' wishlist_countries <- vgi_top_wishlist_countries(steam_app_id = 892970)
#' 
#' # Display top 10 countries
#' head(wishlist_countries, 10)
#' 
#' # Compare with actual player distribution
#' player_countries <- vgi_top_countries(steam_app_id = 892970)
#' 
#' # Merge to compare wishlist vs player percentages
#' comparison <- merge(wishlist_countries, player_countries, 
#'                    by = "country", suffixes = c("_wishlist", "_player"))
#' 
#' # Calculate conversion potential
#' comparison$conversion_rate <- comparison$percentage_player / comparison$percentage_wishlist
#' comparison <- comparison[order(comparison$conversion_rate), ]
#' 
#' # Find underperforming countries (high wishlist, low players)
#' underperforming <- comparison[comparison$conversion_rate < 0.5, ]
#' cat("Countries with low wishlist conversion:\n")
#' print(underperforming[, c("countryName_wishlist", "percentage_wishlist", 
#'                          "percentage_player", "conversion_rate")])
#' 
#' # Calculate regional interest
#' asia_countries <- c("CN", "JP", "KR", "TW", "HK", "SG", "TH", "ID", "MY", "PH")
#' asia_wishlist_pct <- sum(wishlist_countries$percentage[
#'   wishlist_countries$country %in% asia_countries])
#' cat("Asia wishlist percentage:", round(asia_wishlist_pct, 1), "%\n")
#' 
#' # Visualize top 10 wishlist countries
#' top10 <- head(wishlist_countries, 10)
#' barplot(top10$percentage, 
#'         names.arg = top10$countryName,
#'         las = 2,
#'         main = "Top 10 Countries by Wishlist %",
#'         ylab = "Percentage of Wishlists",
#'         col = "darkgreen")
#' }
vgi_top_wishlist_countries <- function(steam_app_id,
                                      auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                                      headers = list()) {
  
  # Validate inputs
  validate_numeric(steam_app_id, "steam_app_id")
  
  # Make API request
  result <- make_api_request(
    endpoint = "player-insights/games/top-wishlist-countries",
    query_params = list(steamAppIds = as.character(steam_app_id), limit = 1),
    auth_token = auth_token,
    method = "GET",
    headers = headers
  )

  rows <- .vgi_unwrap_results(result)
  if (!is.data.frame(rows) || nrow(rows) == 0 || !"wishlists" %in% names(rows)) {
    return(.vgi_clean_names(data.frame(
      country = character(),
      countryName = character(),
      wishlistCount = integer(),
      percentage = numeric(),
      rank = integer(),
      stringsAsFactors = FALSE
    )))
  }

  wishlists <- rows$wishlists[[1]]
  if (!is.data.frame(wishlists) || nrow(wishlists) == 0) {
    return(.vgi_clean_names(data.frame(
      country = character(),
      countryName = character(),
      wishlistCount = integer(),
      percentage = numeric(),
      rank = integer(),
      stringsAsFactors = FALSE
    )))
  }

  .vgi_clean_names(data.frame(
    country = as.character(wishlists$countryCode %||% NA_character_),
    countryName = as.character(wishlists$countryName %||% NA_character_),
    wishlistCount = as.integer(NA),
    percentage = as.numeric(wishlists$percentage %||% NA),
    rank = as.integer(wishlists$rank %||% seq_len(nrow(wishlists))),
    stringsAsFactors = FALSE
  ))
}