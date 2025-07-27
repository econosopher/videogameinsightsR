#' Get Top Wishlist Countries Data for All Games
#'
#' Retrieve top countries by wishlist count for all games, providing insights
#' into global interest patterns and pre-release market potential.
#'
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{steamAppId}{Integer. The Steam App ID}
#'   \item{topWishlistCountries}{List. Top countries with wishlist percentages}
#'   \item{wishlistCountryCount}{Integer. Number of countries with wishlists}
#'   \item{topWishlistCountry}{Character. The #1 country by wishlist count}
#'   \item{topWishlistCountryPct}{Numeric. Percentage of wishlists from top country}
#' }
#'
#' @details
#' Wishlist geographic data reveals:
#' \itemize{
#'   \item Pre-launch interest by region
#'   \item Marketing effectiveness across countries
#'   \item Localization priorities
#'   \item Launch strategy optimization
#'   \item Conversion potential by region
#' }
#' 
#' Comparing wishlist distribution with actual player distribution
#' can reveal untapped markets or conversion challenges.
#'
#' @export
#' @examples
#' \dontrun{
#' # Get wishlist country data for all games
#' wishlist_countries <- vgi_all_games_wishlist_countries()
#' 
#' # Compare with player country data
#' player_countries <- vgi_all_games_top_countries()
#' 
#' # Merge to analyze wishlist vs player patterns
#' geo_comparison <- merge(wishlist_countries, player_countries,
#'                        by = "steamAppId",
#'                        suffixes = c("_wishlist", "_player"))
#' 
#' # Find games where wishlist and player geography differ
#' geo_comparison$country_match <- 
#'   geo_comparison$topWishlistCountry == geo_comparison$topCountry
#' 
#' mismatched <- geo_comparison[!geo_comparison$country_match, ]
#' cat("Games where top wishlist country != top player country:", 
#'     nrow(mismatched), "\n")
#' 
#' # Analyze wishlist concentration
#' hist(wishlist_countries$topWishlistCountryPct,
#'      breaks = 30,
#'      main = "Wishlist Geographic Concentration",
#'      xlab = "Top Country Wishlist %",
#'      col = "darkgreen")
#' 
#' # Find games with global wishlist appeal
#' global_wishlist <- wishlist_countries[
#'   wishlist_countries$topWishlistCountryPct < 25 & 
#'   wishlist_countries$wishlistCountryCount > 50, ]
#' cat("Games with global wishlist distribution:", nrow(global_wishlist), "\n")
#' 
#' # Regional wishlist patterns
#' wishlist_top_countries <- table(wishlist_countries$topWishlistCountry)
#' player_top_countries <- table(player_countries$topCountry)
#' 
#' # Compare which countries dominate wishlists vs players
#' country_comparison <- merge(
#'   data.frame(country = names(wishlist_top_countries),
#'              wishlist_games = as.numeric(wishlist_top_countries)),
#'   data.frame(country = names(player_top_countries),
#'              player_games = as.numeric(player_top_countries)),
#'   by = "country", all = TRUE
#' )
#' country_comparison[is.na(country_comparison)] <- 0
#' country_comparison$wishlist_player_ratio <- 
#'   country_comparison$wishlist_games / (country_comparison$player_games + 1)
#' 
#' # Countries that wishlist more than they play
#' high_wishlist_countries <- country_comparison[
#'   country_comparison$wishlist_player_ratio > 1.5 & 
#'   country_comparison$wishlist_games > 10, ]
#' cat("Countries that wishlist disproportionately:\n")
#' print(high_wishlist_countries[order(-high_wishlist_countries$wishlist_player_ratio), ])
#' 
#' # Extract detailed data for emerging markets
#' emerging_markets <- c("BR", "IN", "MX", "TR", "PL")
#' 
#' emerging_wishlist_share <- sapply(wishlist_countries$topWishlistCountries,
#'   function(countries) {
#'     if (is.null(countries)) return(0)
#'     sum(countries$percentage[countries$country %in% emerging_markets])
#'   })
#' 
#' high_emerging <- wishlist_countries[emerging_wishlist_share > 30, ]
#' cat("Games with >30% wishlists from emerging markets:", nrow(high_emerging), "\n")
#' 
#' # Wishlist velocity by region (would need time series data)
#' # Could identify which regions are gaining momentum
#' }
vgi_all_games_wishlist_countries <- function(auth_token = Sys.getenv("VGI_AUTH_TOKEN"), headers = list()) {
  
  # Make API request
  result <- make_api_request(
    endpoint = "player-insights/games/top-wishlist-countries",
    auth_token = auth_token,
    method = "GET",
    headers = headers
  )
  
  # Convert to data frame
  if (is.list(result) && length(result) > 0) {
    df <- do.call(rbind, lapply(result, function(x) {
      # Process top wishlist countries
      top_wishlist_countries <- NULL
      top_wishlist_country <- NA
      top_wishlist_country_pct <- NA
      wishlist_country_count <- 0
      
      if (!is.null(x$topWishlistCountries) && length(x$topWishlistCountries) > 0) {
        wc_df <- do.call(rbind, lapply(x$topWishlistCountries, function(wc) {
          data.frame(
            country = as.character(wc$country %||% NA),
            countryName = as.character(wc$countryName %||% NA),
            percentage = as.numeric(wc$percentage %||% 0),
            stringsAsFactors = FALSE
          )
        }))
        
        top_wishlist_countries <- wc_df
        wishlist_country_count <- nrow(wc_df)
        if (wishlist_country_count > 0) {
          top_wishlist_country <- wc_df$country[1]
          top_wishlist_country_pct <- wc_df$percentage[1]
        }
      }
      
      data.frame(
        steamAppId = as.integer(x$steamAppId),
        topWishlistCountries = I(list(top_wishlist_countries)),
        wishlistCountryCount = wishlist_country_count,
        topWishlistCountry = top_wishlist_country,
        topWishlistCountryPct = top_wishlist_country_pct,
        stringsAsFactors = FALSE
      )
    }))
    
    # Sort by top wishlist country percentage descending
    df <- df[order(-df$topWishlistCountryPct, na.last = TRUE), ]
    
    return(df)
  } else {
    # Return empty data frame with correct structure
    return(data.frame(
      steamAppId = integer(),
      topWishlistCountries = I(list()),
      wishlistCountryCount = integer(),
      topWishlistCountry = character(),
      topWishlistCountryPct = numeric(),
      stringsAsFactors = FALSE
    ))
  }
}