#' Get Steam Market Data Analytics
#'
#' Retrieve Steam market analytics including global statistics and trends.
#'
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A list containing Steam market analytics data with components:
#' \describe{
#'   \item{totalGames}{Integer. Total number of games on Steam}
#'   \item{totalRevenue}{Numeric. Total revenue across all games}
#'   \item{totalUnitsSold}{Numeric. Total units sold across all games}
#'   \item{averagePrice}{Numeric. Average game price}
#'   \item{medianPrice}{Numeric. Median game price}
#'   \item{totalDevelopers}{Integer. Total number of developers}
#'   \item{totalPublishers}{Integer. Total number of publishers}
#'   \item{gamesReleasedLast30Days}{Integer. Number of games released in last 30 days}
#'   \item{gamesReleasedLast365Days}{Integer. Number of games released in last year}
#'   \item{topGenres}{Data frame with genre statistics}
#'   \item{topTags}{Data frame with tag statistics}
#'   \item{priceDistribution}{Data frame with price range distributions}
#' }
#'
#' @details
#' This endpoint provides high-level market analytics for the entire Steam platform.
#' Use this data to:
#' \itemize{
#'   \item Understand overall market size and trends
#'   \item Analyze price distributions for competitive positioning
#'   \item Track new release velocity
#'   \item Identify popular genres and tags
#'   \item Benchmark your game against market averages
#' }
#'
#' @export
#' @examples
#' \dontrun{
#' # Get Steam market analytics
#' market_data <- vgi_steam_market_data()
#' 
#' # Display key market metrics
#' cat("Total Steam Games:", format(market_data$totalGames, big.mark = ","), "\n")
#' cat("Total Revenue: $", format(market_data$totalRevenue, big.mark = ","), "\n")
#' cat("Average Game Price: $", round(market_data$averagePrice, 2), "\n")
#' cat("Games Released Last 30 Days:", market_data$gamesReleasedLast30Days, "\n")
#' 
#' # Analyze top genres
#' if (!is.null(market_data$topGenres)) {
#'   print("Top 5 Genres by Game Count:")
#'   print(head(market_data$topGenres, 5))
#' }
#' 
#' # Examine price distribution
#' if (!is.null(market_data$priceDistribution)) {
#'   barplot(market_data$priceDistribution$count,
#'           names.arg = market_data$priceDistribution$priceRange,
#'           main = "Steam Games Price Distribution",
#'           xlab = "Price Range",
#'           ylab = "Number of Games",
#'           col = "steelblue")
#' }
#' 
#' # Calculate market concentration
#' top_10_percent_games <- market_data$totalGames * 0.1
#' cat("The top 10% of games (", round(top_10_percent_games), 
#'     " games) likely generate 80%+ of revenue\n", sep = "")
#' }
vgi_steam_market_data <- function(auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                                  headers = list()) {
  
  # Make API request
  result <- make_api_request(
    endpoint = "analytics/steam-market-data",
    auth_token = auth_token,
    method = "GET",
    headers = headers
  )
  
  # Process nested data structures if they exist
  if (!is.null(result$topGenres) && length(result$topGenres) > 0) {
    # Convert to data frame
    result$topGenres <- do.call(rbind, lapply(result$topGenres, function(x) {
      data.frame(
        genre = as.character(x$genre %||% NA),
        gameCount = as.integer(x$gameCount %||% NA),
        totalRevenue = as.numeric(x$totalRevenue %||% NA),
        averagePrice = as.numeric(x$averagePrice %||% NA),
        stringsAsFactors = FALSE
      )
    }))
  }
  
  if (!is.null(result$topTags) && length(result$topTags) > 0) {
    # Convert to data frame
    result$topTags <- do.call(rbind, lapply(result$topTags, function(x) {
      data.frame(
        tag = as.character(x$tag %||% NA),
        gameCount = as.integer(x$gameCount %||% NA),
        totalRevenue = as.numeric(x$totalRevenue %||% NA),
        averageRating = as.numeric(x$averageRating %||% NA),
        stringsAsFactors = FALSE
      )
    }))
  }
  
  if (!is.null(result$priceDistribution) && length(result$priceDistribution) > 0) {
    # Convert to data frame
    result$priceDistribution <- do.call(rbind, lapply(result$priceDistribution, function(x) {
      data.frame(
        priceRange = as.character(x$priceRange %||% NA),
        count = as.integer(x$count %||% NA),
        percentage = as.numeric(x$percentage %||% NA),
        stringsAsFactors = FALSE
      )
    }))
  }
  
  return(result)
}