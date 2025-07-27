#' Get Historical Game Data
#'
#' Retrieve comprehensive historical data for a specific game including all
#' available metrics over time.
#'
#' @param steam_app_id Integer. The Steam App ID of the game.
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A list containing historical data with components:
#' \describe{
#'   \item{steamAppId}{Integer. The Steam App ID}
#'   \item{revenue}{Data frame with date and revenue columns}
#'   \item{unitsSold}{Data frame with date and units sold columns}
#'   \item{concurrentPlayers}{Data frame with date and concurrent players columns}
#'   \item{activePlayers}{Data frame with date, DAU, and MAU columns}
#'   \item{reviews}{Data frame with date, positive, and negative review counts}
#'   \item{wishlists}{Data frame with date and wishlist count columns}
#'   \item{followers}{Data frame with date and follower count columns}
#'   \item{priceHistory}{Data frame with date, currency, and price columns}
#' }
#'
#' @details
#' This endpoint provides a comprehensive historical view of a game's performance
#' across all tracked metrics. This is useful for:
#' \itemize{
#'   \item Creating detailed performance dashboards
#'   \item Analyzing long-term trends
#'   \item Correlating different metrics (e.g., price changes vs. sales)
#'   \item Building predictive models
#'   \item Generating comprehensive reports
#' }
#' 
#' The data typically spans from the game's release date to the present,
#' with different metrics having different update frequencies.
#'
#' @export
#' @examples
#' \dontrun{
#' # Get all historical data for a game
#' historical <- vgi_historical_data(steam_app_id = 730)
#' 
#' # Plot revenue over time
#' if (!is.null(historical$revenue)) {
#'   plot(as.Date(historical$revenue$date), historical$revenue$revenue,
#'        type = "l", col = "green", lwd = 2,
#'        xlab = "Date", ylab = "Revenue ($)",
#'        main = "Revenue Over Time")
#' }
#' 
#' # Analyze review sentiment over time
#' if (!is.null(historical$reviews)) {
#'   historical$reviews$positiveRatio <- historical$reviews$positive / 
#'     (historical$reviews$positive + historical$reviews$negative)
#'   
#'   plot(as.Date(historical$reviews$date), historical$reviews$positiveRatio,
#'        type = "l", col = "blue", lwd = 2,
#'        xlab = "Date", ylab = "Positive Review Ratio",
#'        main = "Review Sentiment Over Time")
#'   abline(h = 0.7, col = "green", lty = 2)
#'   abline(h = 0.5, col = "orange", lty = 2)
#' }
#' 
#' # Correlate price changes with sales
#' if (!is.null(historical$priceHistory) && !is.null(historical$unitsSold)) {
#'   # Find USD prices
#'   usd_prices <- historical$priceHistory[historical$priceHistory$currency == "USD", ]
#'   
#'   # Match dates between price and units sold
#'   matched_data <- merge(usd_prices, historical$unitsSold, 
#'                        by = "date", all = FALSE)
#'   
#'   if (nrow(matched_data) > 0) {
#'     plot(matched_data$price, matched_data$unitsSold,
#'          pch = 19, col = "darkblue",
#'          xlab = "Price (USD)", ylab = "Units Sold",
#'          main = "Price vs. Sales Correlation")
#'   }
#' }
#' 
#' # Calculate growth metrics
#' if (!is.null(historical$followers)) {
#'   n <- nrow(historical$followers)
#'   if (n > 30) {
#'     growth_30d <- (historical$followers$followers[n] - 
#'                    historical$followers$followers[n-30]) / 
#'                   historical$followers$followers[n-30] * 100
#'     cat("30-day follower growth:", round(growth_30d, 1), "%\n")
#'   }
#' }
#' }
vgi_historical_data <- function(steam_app_id,
                               auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                               headers = list()) {
  
  # Validate inputs
  validate_numeric(steam_app_id, "steam_app_id")
  
  # Make API request
  result <- make_api_request(
    endpoint = paste0("historical-data/games/", steam_app_id),
    auth_token = auth_token,
    method = "GET",
    headers = headers
  )
  
  # Process each data type into data frames
  
  # Revenue data
  if (!is.null(result$revenue) && length(result$revenue) > 0) {
    result$revenue <- do.call(rbind, lapply(result$revenue, function(x) {
      data.frame(
        date = as.character(x$date %||% NA),
        revenue = as.numeric(x$revenue %||% NA),
        stringsAsFactors = FALSE
      )
    }))
  }
  
  # Units sold data
  if (!is.null(result$unitsSold) && length(result$unitsSold) > 0) {
    result$unitsSold <- do.call(rbind, lapply(result$unitsSold, function(x) {
      data.frame(
        date = as.character(x$date %||% NA),
        unitsSold = as.numeric(x$unitsSold %||% NA),
        stringsAsFactors = FALSE
      )
    }))
  }
  
  # Concurrent players data
  if (!is.null(result$concurrentPlayers) && length(result$concurrentPlayers) > 0) {
    result$concurrentPlayers <- do.call(rbind, lapply(result$concurrentPlayers, function(x) {
      data.frame(
        date = as.character(x$date %||% NA),
        concurrentPlayers = as.numeric(x$concurrentPlayers %||% NA),
        stringsAsFactors = FALSE
      )
    }))
  }
  
  # Active players (DAU/MAU) data
  if (!is.null(result$activePlayers) && length(result$activePlayers) > 0) {
    result$activePlayers <- do.call(rbind, lapply(result$activePlayers, function(x) {
      data.frame(
        date = as.character(x$date %||% NA),
        dau = as.numeric(x$dau %||% NA),
        mau = as.numeric(x$mau %||% NA),
        stringsAsFactors = FALSE
      )
    }))
  }
  
  # Reviews data
  if (!is.null(result$reviews) && length(result$reviews) > 0) {
    result$reviews <- do.call(rbind, lapply(result$reviews, function(x) {
      data.frame(
        date = as.character(x$date %||% NA),
        positive = as.numeric(x$positive %||% NA),
        negative = as.numeric(x$negative %||% NA),
        stringsAsFactors = FALSE
      )
    }))
  }
  
  # Wishlists data
  if (!is.null(result$wishlists) && length(result$wishlists) > 0) {
    result$wishlists <- do.call(rbind, lapply(result$wishlists, function(x) {
      data.frame(
        date = as.character(x$date %||% NA),
        wishlists = as.numeric(x$wishlists %||% NA),
        stringsAsFactors = FALSE
      )
    }))
  }
  
  # Followers data
  if (!is.null(result$followers) && length(result$followers) > 0) {
    result$followers <- do.call(rbind, lapply(result$followers, function(x) {
      data.frame(
        date = as.character(x$date %||% NA),
        followers = as.numeric(x$followers %||% NA),
        stringsAsFactors = FALSE
      )
    }))
  }
  
  # Price history data
  if (!is.null(result$priceHistory) && length(result$priceHistory) > 0) {
    result$priceHistory <- do.call(rbind, lapply(result$priceHistory, function(x) {
      data.frame(
        date = as.character(x$date %||% NA),
        currency = as.character(x$currency %||% NA),
        price = as.numeric(x$price %||% NA),
        stringsAsFactors = FALSE
      )
    }))
  }
  
  return(result)
}