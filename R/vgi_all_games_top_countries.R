#' Get Top Countries Data for All Games
#'
#' Retrieve top countries by player count for all games, providing insights
#' into global gaming preferences and regional market dynamics.
#'
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{steamAppId}{Integer. The Steam App ID}
#'   \item{topCountries}{List. Top countries with their player percentages}
#'   \item{countryCount}{Integer. Number of countries in the data}
#'   \item{topCountry}{Character. The #1 country by player count}
#'   \item{topCountryPct}{Numeric. Percentage of players from top country}
#' }
#'
#' @details
#' This endpoint helps identify:
#' \itemize{
#'   \item Games with global vs regional appeal
#'   \item Regional gaming preferences
#'   \item Localization opportunities
#'   \item Market penetration patterns
#'   \item Cultural gaming trends
#' }
#' 
#' The topCountries list column contains detailed country breakdowns
#' that can be expanded for deeper analysis.
#'
#' @export
#' @examples
#' \dontrun{
#' # Get top countries for all games
#' countries_data <- vgi_all_games_top_countries()
#' 
#' # Find games dominated by specific countries
#' us_dominated <- countries_data[countries_data$topCountry == "US" & 
#'                               countries_data$topCountryPct > 50, ]
#' cat("Games where >50% of players are from US:", nrow(us_dominated), "\n")
#' 
#' # Find globally diverse games
#' global_games <- countries_data[countries_data$topCountryPct < 20 & 
#'                               countries_data$countryCount > 50, ]
#' cat("Globally diverse games (<20% from any country):", nrow(global_games), "\n")
#' 
#' # Analyze regional preferences
#' top_countries_summary <- table(countries_data$topCountry)
#' top_10_countries <- head(sort(top_countries_summary, decreasing = TRUE), 10)
#' 
#' barplot(top_10_countries,
#'         main = "Countries Most Often #1 in Games",
#'         xlab = "Country",
#'         ylab = "Number of Games Where #1",
#'         las = 2,
#'         col = "steelblue")
#' 
#' # Extract detailed country data for a specific game
#' game_id <- 730  # Example game
#' game_countries <- countries_data$topCountries[
#'   countries_data$steamAppId == game_id][[1]]
#' if (!is.null(game_countries)) {
#'   print(head(game_countries, 10))
#' }
#' 
#' # Find games popular in specific regions
#' # Extract games where China is in top 3
#' china_popular <- countries_data[sapply(countries_data$topCountries, 
#'   function(tc) {
#'     if (is.null(tc) || nrow(tc) < 3) return(FALSE)
#'     "CN" %in% tc$country[1:3]
#'   }), ]
#' cat("Games where China is in top 3 countries:", nrow(china_popular), "\n")
#' 
#' # Calculate market concentration
#' countries_data$top3_concentration <- sapply(countries_data$topCountries,
#'   function(tc) {
#'     if (is.null(tc) || nrow(tc) < 3) return(NA)
#'     sum(tc$percentage[1:3])
#'   })
#' 
#' # Games with highest geographic concentration
#' concentrated <- countries_data[!is.na(countries_data$top3_concentration) & 
#'                               countries_data$top3_concentration > 70, ]
#' cat("Games where top 3 countries >70% of players:", nrow(concentrated), "\n")
#' 
#' # Regional gaming hours analysis
#' # Games popular in Asia vs Americas vs Europe
#' asia_countries <- c("CN", "JP", "KR", "TW", "HK", "SG", "TH", "ID")
#' americas_countries <- c("US", "CA", "BR", "MX", "AR", "CL", "CO")
#' europe_countries <- c("DE", "FR", "GB", "IT", "ES", "PL", "NL", "SE")
#' 
#' countries_data$asia_pct <- sapply(countries_data$topCountries,
#'   function(tc) {
#'     if (is.null(tc)) return(0)
#'     sum(tc$percentage[tc$country %in% asia_countries])
#'   })
#' 
#' asia_focused <- countries_data[countries_data$asia_pct > 50, ]
#' cat("Games with >50% Asian players:", nrow(asia_focused), "\n")
#' }
vgi_all_games_top_countries <- function(auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                                       headers = list()) {
  
  # Make API request
  result <- make_api_request(
    endpoint = "player-insights/games/top-countries",
    auth_token = auth_token,
    method = "GET",
    headers = headers
  )
  
  # Convert to data frame
  if (is.list(result) && length(result) > 0) {
    df <- do.call(rbind, lapply(result, function(x) {
      # Process top countries list
      top_countries <- NULL
      top_country <- NA
      top_country_pct <- NA
      country_count <- 0
      
      if (!is.null(x$topCountries) && length(x$topCountries) > 0) {
        tc_df <- do.call(rbind, lapply(x$topCountries, function(tc) {
          data.frame(
            country = as.character(tc$country %||% NA),
            countryName = as.character(tc$countryName %||% NA),
            percentage = as.numeric(tc$percentage %||% 0),
            stringsAsFactors = FALSE
          )
        }))
        
        top_countries <- tc_df
        country_count <- nrow(tc_df)
        if (country_count > 0) {
          top_country <- tc_df$country[1]
          top_country_pct <- tc_df$percentage[1]
        }
      }
      
      data.frame(
        steamAppId = as.integer(x$steamAppId),
        topCountries = I(list(top_countries)),
        countryCount = country_count,
        topCountry = top_country,
        topCountryPct = top_country_pct,
        stringsAsFactors = FALSE
      )
    }))
    
    # Sort by top country percentage descending
    df <- df[order(-df$topCountryPct), ]
    
    return(df)
  } else {
    # Return empty data frame with correct structure
    return(data.frame(
      steamAppId = integer(),
      topCountries = I(list()),
      countryCount = integer(),
      topCountry = character(),
      topCountryPct = numeric(),
      stringsAsFactors = FALSE
    ))
  }
}