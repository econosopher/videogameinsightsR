#' Get Reviews Data by Date
#'
#' Retrieve review data for all games on a specific date, useful for
#' market-wide analysis and trend identification.
#'
#' @param date Character string or Date. The date for which to retrieve data
#'   in "YYYY-MM-DD" format.
#' @param steam_app_ids Numeric vector. Optional. Steam App IDs to filter results.
#'   If not provided, returns data for all available games.
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{steamAppId}{Integer. The Steam App ID}
#'   \item{date}{Character. The date of the data}
#'   \item{positiveReviews}{Integer. Number of positive reviews}
#'   \item{negativeReviews}{Integer. Number of negative reviews}
#'   \item{totalReviews}{Integer. Total number of reviews}
#'   \item{positiveRatio}{Numeric. Ratio of positive reviews (0-1)}
#' }
#'
#' @details
#' This endpoint enables:
#' \itemize{
#'   \item Daily market sentiment analysis
#'   \item Identifying games with review bombs or surges
#'   \item Tracking industry-wide review trends
#'   \item Comparing review activity across multiple games
#'   \item Building review trend dashboards
#' }
#' 
#' The data represents the cumulative review counts as of the specified date.
#'
#' @export
#' @examples
#' \dontrun{
#' # Get reviews for a specific date
#' reviews_data <- vgi_reviews_by_date("2024-01-15")
#' 
#' # Find games with most reviews on this date
#' top_reviewed <- head(reviews_data[order(-reviews_data$totalReviews), ], 20)
#' print(top_reviewed)
#' 
#' # Find games with best review ratios (min 100 reviews)
#' qualified <- reviews_data[reviews_data$totalReviews >= 100, ]
#' best_rated <- head(qualified[order(-qualified$positiveRatio), ], 20)
#' cat("Best rated games with 100+ reviews:\n")
#' print(best_rated[, c("steamAppId", "positiveRatio", "totalReviews")])
#' 
#' # Analyze review distribution
#' hist(reviews_data$positiveRatio[reviews_data$totalReviews >= 10],
#'      breaks = 20,
#'      main = "Distribution of Review Scores",
#'      xlab = "Positive Review Ratio",
#'      col = "lightblue")
#' abline(v = 0.7, col = "green", lwd = 2, lty = 2)
#' abline(v = 0.5, col = "orange", lwd = 2, lty = 2)
#' 
#' # Compare with previous date
#' prev_date <- as.Date("2024-01-15") - 1
#' prev_reviews <- vgi_reviews_by_date(as.character(prev_date))
#' 
#' # Merge to find daily changes
#' comparison <- merge(reviews_data, prev_reviews, 
#'                    by = "steamAppId", 
#'                    suffixes = c("_today", "_yesterday"))
#' 
#' # Calculate daily review additions
#' comparison$new_reviews <- comparison$totalReviews_today - comparison$totalReviews_yesterday
#' comparison$ratio_change <- comparison$positiveRatio_today - comparison$positiveRatio_yesterday
#' 
#' # Find games with biggest review changes
#' biggest_changes <- head(comparison[order(-abs(comparison$new_reviews)), ], 10)
#' cat("Games with most review activity:\n")
#' print(biggest_changes[, c("steamAppId", "new_reviews", "ratio_change")])
#' }
vgi_reviews_by_date <- function(date,
                               steam_app_ids = NULL,
                               auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                               headers = list()) {
  
  # Validate and format date
  formatted_date <- format_date(date)
  
  # Build query parameters if steam_app_ids provided
  query_params <- list()
  if (!is.null(steam_app_ids)) {
    # Ensure numeric and convert to comma-separated string
    steam_app_ids <- as.numeric(steam_app_ids)
    ids_string <- paste(steam_app_ids, collapse = ",")
    query_params$steamAppIds <- ids_string
  }
  
  # Make API request
  result <- make_api_request(
    endpoint = paste0("reception/reviews/", formatted_date),
    query_params = query_params,
    auth_token = auth_token,
    method = "GET",
    headers = headers
  )
  
  # Convert to data frame
  if (is.list(result) && length(result) > 0) {
    df <- do.call(rbind, lapply(result, function(x) {
      positive <- as.integer(x$positiveReviews %||% 0)
      negative <- as.integer(x$negativeReviews %||% 0)
      total <- positive + negative
      
      data.frame(
        steamAppId = as.integer(x$steamAppId),
        date = formatted_date,
        positiveReviews = positive,
        negativeReviews = negative,
        totalReviews = total,
        positiveRatio = if (total > 0) positive / total else NA,
        stringsAsFactors = FALSE
      )
    }))
    
    # Sort by total reviews descending
    df <- df[order(-df$totalReviews), ]
    
    return(df)
  } else {
    # Return empty data frame with correct structure
    return(data.frame(
      steamAppId = integer(),
      date = character(),
      positiveReviews = integer(),
      negativeReviews = integer(),
      totalReviews = integer(),
      positiveRatio = numeric(),
      stringsAsFactors = FALSE
    ))
  }
}