#' Get Price History Data for a Game
#'
#' Retrieve historical pricing data for a specific game across different currencies.
#'
#' @param steam_app_id Integer. The Steam App ID of the game.
#' @param currency Character. Optional. Currency code (e.g., "USD", "EUR", "GBP").
#'   If not specified, returns price history for all currencies.
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return If currency is specified, returns a list containing:
#' \describe{
#'   \item{steamAppId}{Integer. The Steam App ID}
#'   \item{currency}{Character. The currency code}
#'   \item{priceChanges}{Data frame with columns:
#'     \itemize{
#'       \item priceInitial: Full price without discount
#'       \item priceFinal: Price that the game is sold at
#'       \item firstDate: First date when this price was recorded
#'       \item lastDate: Last date when this price was active (NULL if current)
#'     }
#'   }
#' }
#' 
#' If currency is not specified, returns a list containing:
#' \describe{
#'   \item{steamAppId}{Integer. The Steam App ID}
#'   \item{price}{List of price histories for each currency}
#' }
#'
#' @export
#' @examples
#' \dontrun{
#' # Get price history for a game in USD
#' usd_history <- vgi_insights_price_history(
#'   steam_app_id = 730,
#'   currency = "USD"
#' )
#' 
#' # Calculate discount percentage for each price period
#' if (nrow(usd_history$priceChanges) > 0) {
#'   usd_history$priceChanges$discount_pct <- 
#'     round((1 - usd_history$priceChanges$priceFinal / 
#'            usd_history$priceChanges$priceInitial) * 100, 1)
#' }
#' 
#' # Get price history for all currencies
#' all_prices <- vgi_insights_price_history(steam_app_id = 730)
#' 
#' # Find all currencies where the game is available
#' currencies <- sapply(all_prices$price, function(x) x$currency)
#' print(paste("Available in", length(currencies), "currencies"))
#' 
#' # Identify sales periods (where priceFinal < priceInitial)
#' sales <- usd_history$priceChanges[
#'   usd_history$priceChanges$priceFinal < usd_history$priceChanges$priceInitial, 
#' ]
#' print(paste("Number of sale periods:", nrow(sales)))
#' }
vgi_insights_price_history <- function(steam_app_id, 
                                     currency = NULL,
                                     auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                                     headers = list()) {
  
  validate_numeric(steam_app_id, "steam_app_id")
  if (!is.null(currency) && (!is.character(currency) || nchar(currency) == 0)) {
    stop("currency must be a non-empty character string")
  }
  
  hist <- vgi_historical_data(steam_app_id, auth_token = auth_token, headers = headers)
  
  empty_changes <- data.frame(
    priceInitial = numeric(), priceFinal = numeric(),
    firstDate = as.Date(character()), lastDate = as.Date(character()),
    stringsAsFactors = FALSE
  )
  
  price_ts <- hist$priceHistory
  if (is.null(price_ts) || nrow(price_ts) == 0) {
    return(.vgi_clean_list(list(
      steamAppId = as.integer(steam_app_id),
      currency = currency %||% "ALL",
      priceChanges = empty_changes
    )))
  }
  
  # Build price-change periods from daily snapshots
  build_changes <- function(df) {
    df <- df[order(df$date), , drop = FALSE]
    df <- df[!is.na(df$priceInitial) | !is.na(df$priceFinal), , drop = FALSE]
    if (nrow(df) == 0) return(empty_changes)
    
    changes <- list()
    cur_init <- df$priceInitial[1]
    cur_final <- df$priceFinal[1]
    first_date <- df$date[1]
    
    for (i in seq_len(nrow(df))) {
      pi <- df$priceInitial[i]
      pf <- df$priceFinal[i]
      if (!identical(pi, cur_init) || !identical(pf, cur_final)) {
        changes[[length(changes) + 1]] <- data.frame(
          priceInitial = cur_init, priceFinal = cur_final,
          firstDate = as.Date(first_date),
          lastDate = as.Date(df$date[i - 1]),
          stringsAsFactors = FALSE
        )
        cur_init <- pi
        cur_final <- pf
        first_date <- df$date[i]
      }
    }
    changes[[length(changes) + 1]] <- data.frame(
      priceInitial = cur_init, priceFinal = cur_final,
      firstDate = as.Date(first_date), lastDate = as.Date(NA),
      stringsAsFactors = FALSE
    )
    
    result <- do.call(rbind, changes)
    result[order(result$firstDate, decreasing = TRUE), , drop = FALSE]
  }
  
  price_changes <- build_changes(price_ts)
  
  .vgi_clean_list(list(
    steamAppId = as.integer(steam_app_id),
    currency = currency %||% "USD",
    priceChanges = price_changes
  ))
}