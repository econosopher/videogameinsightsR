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
  
  # Validate inputs
  validate_numeric(steam_app_id, "steam_app_id")
  
  # Build endpoint based on whether currency is specified
  if (!is.null(currency)) {
    if (!is.character(currency) || nchar(currency) == 0) {
      stop("currency must be a non-empty character string")
    }
    endpoint <- paste0("commercial-performance/price-history/games/", 
                       steam_app_id, "/", currency)
  } else {
    endpoint <- paste0("commercial-performance/price-history/games/", 
                       steam_app_id)
  }
  
  # Make API request
  result <- make_api_request(
    endpoint = endpoint,
    auth_token = auth_token,
    method = "GET",
    headers = headers
  )
  
  # Process the response based on the endpoint type
  if (!is.null(currency)) {
    # Single currency response - process priceChanges array
    if (!is.null(result$priceChanges) && length(result$priceChanges) > 0) {
      # Convert to data frame
      price_df <- do.call(rbind, lapply(result$priceChanges, function(x) {
        data.frame(
          priceInitial = as.numeric(x$priceInitial),
          priceFinal = as.numeric(x$priceFinal),
          firstDate = as.Date(x$firstDate),
          lastDate = if(!is.null(x$lastDate)) as.Date(x$lastDate) else NA,
          stringsAsFactors = FALSE
        )
      }))
      
      # Sort by firstDate descending (most recent first)
      price_df <- price_df[order(price_df$firstDate, decreasing = TRUE), ]
      
      result$priceChanges <- price_df
    } else {
      # Return empty data frame with correct structure
      result$priceChanges <- data.frame(
        priceInitial = numeric(),
        priceFinal = numeric(),
        firstDate = as.Date(character()),
        lastDate = as.Date(character()),
        stringsAsFactors = FALSE
      )
    }
  } else {
    # All currencies response - process each currency's priceChanges
    if (!is.null(result$price) && length(result$price) > 0) {
      result$price <- lapply(result$price, function(curr_data) {
        if (!is.null(curr_data$priceChanges) && length(curr_data$priceChanges) > 0) {
          # Convert to data frame
          price_df <- do.call(rbind, lapply(curr_data$priceChanges, function(x) {
            data.frame(
              priceInitial = as.numeric(x$priceInitial),
              priceFinal = as.numeric(x$priceFinal),
              firstDate = as.Date(x$firstDate),
              lastDate = if(!is.null(x$lastDate)) as.Date(x$lastDate) else NA,
              stringsAsFactors = FALSE
            )
          }))
          
          # Sort by firstDate descending
          price_df <- price_df[order(price_df$firstDate, decreasing = TRUE), ]
          
          curr_data$priceChanges <- price_df
        }
        curr_data
      })
    }
  }
  
  return(result)
}