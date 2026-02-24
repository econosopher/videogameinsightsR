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
  
  validate_numeric(steam_app_id, "steam_app_id")
  
  all_rows <- data.frame()
  cursor <- NULL
  
  repeat {
    qp <- list(steamAppIds = as.character(steam_app_id), limit = 1000)
    if (!is.null(cursor)) qp$cursor <- cursor
    
    raw <- make_api_request(
      endpoint = "historical-data",
      query_params = qp,
      auth_token = auth_token,
      method = "GET",
      headers = headers
    )
    
    batch <- .vgi_unwrap_results(raw)
    if (!is.data.frame(batch) || nrow(batch) == 0) break
    
    all_rows <- rbind(all_rows, batch)
    
    next_cur <- raw$nextCursor
    if (is.null(next_cur) || identical(next_cur, cursor)) break
    cursor <- next_cur
  }
  
  if (nrow(all_rows) == 0) {
    return(list(
      steamAppId = as.integer(steam_app_id),
      revenue = NULL, unitsSold = NULL, concurrentPlayers = NULL,
      activePlayers = NULL, reviews = NULL, wishlists = NULL,
      followers = NULL, priceHistory = NULL
    ))
  }
  
  # Filter to steam platform rows matching the requested ID
  if ("platform" %in% names(all_rows)) {
    all_rows <- all_rows[all_rows$platform == "steam", , drop = FALSE]
  }
  if ("externalId" %in% names(all_rows)) {
    all_rows <- all_rows[as.integer(all_rows$externalId) == as.integer(steam_app_id), , drop = FALSE]
  }
  
  safe_col <- function(df, col) {
    if (col %in% names(df)) df[[col]] else rep(NA, nrow(df))
  }
  
  make_ts <- function(cols) {
    out <- data.frame(date = as.character(all_rows$date), stringsAsFactors = FALSE)
    for (nm in names(cols)) {
      out[[nm]] <- as.numeric(safe_col(all_rows, cols[[nm]]))
    }
    out <- out[!is.na(out$date), , drop = FALSE]
    out[order(out$date), , drop = FALSE]
  }
  
  revenue_df <- make_ts(list(revenue = "revenueTotal", dailyRevenue = "revenueChange"))
  units_df <- make_ts(list(unitsSold = "unitsSoldTotal", dailyUnits = "unitsSoldChange"))
  ccu_df <- make_ts(list(ccuAvg = "ccuAvg", ccuMedian = "ccuMedian",
                          ccuMax = "ccuMax", ccuMin = "ccuMin"))
  active_df <- make_ts(list(dau = "dau", mau = "mau"))
  reviews_df <- make_ts(list(positive = "positiveReviewsTotal", negative = "negativeReviewsTotal"))
  wishlists_df <- make_ts(list(wishlists = "wishlistsTotal"))
  followers_df <- make_ts(list(followers = "followersTotal"))
  price_df <- make_ts(list(priceInitial = "priceInitial", priceFinal = "priceFinal"))
  
  null_if_empty <- function(df) if (nrow(df) == 0 || all(is.na(df[, -1, drop = FALSE]))) NULL else df
  
  list(
    steamAppId = as.integer(steam_app_id),
    revenue = null_if_empty(revenue_df),
    unitsSold = null_if_empty(units_df),
    concurrentPlayers = null_if_empty(ccu_df),
    activePlayers = null_if_empty(active_df),
    reviews = null_if_empty(reviews_df),
    wishlists = null_if_empty(wishlists_df),
    followers = null_if_empty(followers_df),
    priceHistory = null_if_empty(price_df)
  )
}