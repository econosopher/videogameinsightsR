#' Get Wishlist Data for a Game
#'
#' Retrieve historical wishlist data for a specific game, showing how many users
#' have the game on their wishlist over time.
#'
#' @param steam_app_id Integer. The Steam App ID of the game.
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A list containing:
#' \describe{
#'   \item{steamAppId}{Integer. The Steam App ID}
#'   \item{wishlistChanges}{Data frame with columns:
#'     \itemize{
#'       \item date: Date of the data point
#'       \item wishlistsTotal: Total number of wishlists on that date
#'       \item wishlistsChange: Change in wishlists from previous period
#'     }
#'   }
#' }
#'
#' @details
#' Wishlist data is a key indicator of interest and potential future sales:
#' \itemize{
#'   \item High wishlist numbers indicate strong market interest
#'   \item Wishlist spikes often follow marketing campaigns or announcements
#'   \item Conversion rate from wishlist to purchase typically ranges from 5-20%
#'   \item Wishlist trends can predict launch day sales
#' }
#'
#' @export
#' @examples
#' \dontrun{
#' # Get wishlist data for a game
#' wishlists <- vgi_insights_wishlists(steam_app_id = 892970)
#' 
#' # Calculate total wishlists and recent trend
#' current_wishlists <- tail(wishlists$wishlistChanges, 1)$wishlistsTotal
#' print(paste("Current wishlists:", format(current_wishlists, big.mark = ",")))
#' 
#' # Calculate 30-day growth
#' if (nrow(wishlists$wishlistChanges) >= 30) {
#'   thirty_days_ago <- wishlists$wishlistChanges[nrow(wishlists$wishlistChanges) - 29, ]
#'   growth <- current_wishlists - thirty_days_ago$wishlistsTotal
#'   growth_pct <- (growth / thirty_days_ago$wishlistsTotal) * 100
#'   print(paste("30-day growth:", format(growth, big.mark = ","), 
#'               sprintf("(%.1f%%)", growth_pct)))
#' }
#' 
#' # Plot wishlist trend
#' plot(wishlists$wishlistChanges$date, wishlists$wishlistChanges$wishlistsTotal,
#'      type = "l", col = "blue", lwd = 2,
#'      main = "Wishlist Trend Over Time",
#'      xlab = "Date", ylab = "Total Wishlists")
#' 
#' # Identify major wishlist spikes
#' avg_change <- mean(abs(wishlists$wishlistChanges$wishlistsChange), na.rm = TRUE)
#' spikes <- wishlists$wishlistChanges[
#'   wishlists$wishlistChanges$wishlistsChange > avg_change * 3, 
#' ]
#' if (nrow(spikes) > 0) {
#'   print("Major wishlist spikes detected on:")
#'   print(spikes[, c("date", "wishlistsChange")])
#' }
#' }
vgi_insights_wishlists <- function(steam_app_id,
                                 auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                                 headers = list()) {
  
  # Validate inputs
  validate_numeric(steam_app_id, "steam_app_id")
  
  # Make API request
  result <- make_api_request(
    endpoint = paste0("interest-level/wishlists/games/", steam_app_id),
    auth_token = auth_token,
    method = "GET",
    headers = headers
  )
  
  # Process the wishlistChanges array if it exists
  if (!is.null(result$wishlistChanges) && length(result$wishlistChanges) > 0) {
    # Convert to data frame
    changes_df <- do.call(rbind, lapply(result$wishlistChanges, function(x) {
      data.frame(
        date = as.Date(x$date),
        wishlistsTotal = as.integer(x$wishlistsTotal %||% NA),
        wishlistsChange = as.integer(x$wishlistsChange %||% NA),
        stringsAsFactors = FALSE
      )
    }))
    
    # Sort by date
    changes_df <- changes_df[order(changes_df$date), ]
    
    result$wishlistChanges <- changes_df
  } else {
    # Return empty data frame with correct structure
    result$wishlistChanges <- data.frame(
      date = as.Date(character()),
      wishlistsTotal = integer(),
      wishlistsChange = integer(),
      stringsAsFactors = FALSE
    )
  }
  
  return(result)
}