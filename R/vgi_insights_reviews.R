#' Get Review Analytics from Video Game Insights
#'
#' Retrieve review history data for a specific game, including positive and negative review counts.
#'
#' @param steam_app_id Integer. The Steam App ID of the game.
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A data frame containing review history with columns:
#' \describe{
#'   \item{steamAppId}{Integer. The Steam App ID}
#'   \item{date}{Date. The date of the review data}
#'   \item{positiveReviewsChange}{Integer. New positive reviews since last period}
#'   \item{positiveReviewsTotal}{Integer. Total cumulative positive reviews}
#'   \item{negativeReviewsChange}{Integer. New negative reviews since last period}
#'   \item{negativeReviewsTotal}{Integer. Total cumulative negative reviews}
#' }
#'
#' @details
#' The new API provides both incremental changes and cumulative totals for reviews.
#' You can calculate the overall rating percentage from the totals:
#' `rating = positiveReviewsTotal / (positiveReviewsTotal + negativeReviewsTotal) * 100`
#'
#' @export
#' @examples
#' \dontrun{
#' # Get review history for a game
#' reviews <- vgi_insights_reviews(steam_app_id = 892970)
#' 
#' # Calculate current overall rating
#' latest <- tail(reviews, 1)
#' rating <- latest$positiveReviewsTotal / 
#'           (latest$positiveReviewsTotal + latest$negativeReviewsTotal) * 100
#' print(paste("Overall rating:", round(rating, 1), "%"))
#' 
#' # Plot review trends
#' plot(reviews$date, reviews$positiveReviewsChange, 
#'      type = "l", col = "green",
#'      main = "Daily Review Trends",
#'      xlab = "Date", ylab = "New Reviews")
#' lines(reviews$date, reviews$negativeReviewsChange, col = "red")
#' legend("topright", c("Positive", "Negative"), 
#'        col = c("green", "red"), lty = 1)
#' 
#' # Find review bombs (days with unusual negative reviews)
#' avg_negative <- mean(reviews$negativeReviewsChange, na.rm = TRUE)
#' sd_negative <- sd(reviews$negativeReviewsChange, na.rm = TRUE)
#' review_bombs <- reviews[reviews$negativeReviewsChange > avg_negative + 2*sd_negative, ]
#' }
vgi_insights_reviews <- function(steam_app_id,
                                auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                                headers = list()) {
  
  validate_numeric(steam_app_id, "steam_app_id")
  
  hist <- vgi_historical_data(steam_app_id, auth_token = auth_token, headers = headers)
  
  rv <- hist$reviews
  if (is.null(rv) || nrow(rv) == 0) {
    return(.vgi_clean_names(tibble::tibble(
      steamAppId = integer(), date = as.Date(character()),
      positive = integer(), negative = integer(), total = integer(),
      positiveRatio = numeric()
    )))
  }
  
  total <- as.integer(rv$positive) + as.integer(rv$negative)
  .vgi_clean_names(tibble::tibble(
    steamAppId = as.integer(steam_app_id),
    date = as.Date(rv$date),
    positive = as.integer(rv$positive),
    negative = as.integer(rv$negative),
    total = total,
    positiveRatio = ifelse(total > 0, as.integer(rv$positive) / total, NA_real_),

  ))
}
