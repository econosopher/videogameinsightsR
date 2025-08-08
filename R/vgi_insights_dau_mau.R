#' Get Daily and Monthly Active Users Data
#'
#' Retrieve daily active users (DAU) and monthly active users (MAU) data for a specific game.
#'
#' @param steam_app_id Integer. The Steam App ID of the game.
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A list containing:
#' \describe{
#'   \item{steamAppId}{Integer. The Steam App ID}
#'   \item{playerHistory}{Data frame with columns:
#'     \itemize{
#'       \item date: Date of the data point
#'       \item dau: Daily active users count
#'       \item mau: Monthly active users count
#'     }
#'   }
#' }
#'
#' @details
#' You can calculate the DAU/MAU ratio from the returned data:
#' `dau_mau_ratio = dau / mau`
#' 
#' The DAU/MAU ratio is a key metric for measuring player engagement:
#' \itemize{
#'   \item A ratio of 1.0 means every monthly user plays daily (perfect retention)
#'   \item A ratio of 0.5 means the average player plays 15 days per month
#'   \item A ratio of 0.1 means the average player plays 3 days per month
#' }
#' 
#' Industry benchmarks:
#' \itemize{
#'   \item Casual games: 0.1-0.2
#'   \item Mid-core games: 0.2-0.4
#'   \item Hardcore games: 0.4-0.6
#' }
#'
#' @export
#' @examples
#' \dontrun{
#' # Get DAU/MAU data for a game
#' active_players <- vgi_insights_dau_mau(steam_app_id = 730)
#' 
#' # Calculate DAU/MAU ratios
#' active_players$playerHistory$dau_mau_ratio <- 
#'   active_players$playerHistory$dau / active_players$playerHistory$mau
#' 
#' # Calculate average DAU/MAU ratio
#' avg_ratio <- mean(active_players$playerHistory$dau_mau_ratio, na.rm = TRUE)
#' print(paste("Average DAU/MAU ratio:", round(avg_ratio, 3)))
#' 
#' # Plot DAU and MAU over time
#' par(mfrow = c(2, 1))
#' plot(active_players$playerHistory$date, active_players$playerHistory$dau, 
#'      type = "l", col = "blue",
#'      main = "Daily Active Users", 
#'      xlab = "Date", ylab = "DAU")
#' plot(active_players$playerHistory$date, active_players$playerHistory$mau, 
#'      type = "l", col = "red",
#'      main = "Monthly Active Users", 
#'      xlab = "Date", ylab = "MAU")
#' 
#' # Analyze retention trends
#' plot(active_players$playerHistory$date, 
#'      active_players$playerHistory$dau_mau_ratio,
#'      type = "l", ylim = c(0, 1),
#'      main = "Player Retention (DAU/MAU Ratio)",
#'      xlab = "Date", ylab = "DAU/MAU Ratio")
#' abline(h = 0.3, col = "gray", lty = 2)  # Industry average
#' }
vgi_insights_dau_mau <- function(steam_app_id, 
                               auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                               headers = list()) {
  
  # Validate inputs
  validate_numeric(steam_app_id, "steam_app_id")
  
  # Make API request to the new endpoint
  result <- make_api_request(
    endpoint = paste0("engagement/active-players/games/", steam_app_id),
    auth_token = auth_token,
    method = "GET",
    headers = headers
  )
  
  # Process the playerHistory array if it exists
  ph <- result$playerHistory
  if (!is.null(ph) && length(ph) > 0) {
    data_df <- NULL
    # Case 1: already a data.frame or tibble
    if (is.data.frame(ph)) {
      df <- ph
      # Normalize column names if needed
      if (!"date" %in% names(df) && "day" %in% names(df)) df$date <- df$day
      if (!"dau" %in% names(df)) df$dau <- df$dau %||% NA
      if (!"mau" %in% names(df)) df$mau <- df$mau %||% NA
      # Coerce types
      df$date <- tryCatch(as.Date(df$date), error = function(e) as.Date(character()))
      df$dau <- suppressWarnings(as.integer(df$dau))
      df$mau <- suppressWarnings(as.integer(df$mau))
      data_df <- df[, c("date", "dau", "mau")]
    } else if (is.list(ph)) {
      # Case 2: list of entries; each entry may be a list or atomic
      rows <- lapply(ph, function(x) {
        if (is.list(x)) {
          date_val <- x$date %||% x$day %||% NA
          data.frame(
            date = tryCatch(as.Date(date_val), error = function(e) as.Date(NA_character_)),
            dau = suppressWarnings(as.integer(x$dau %||% NA)),
            mau = suppressWarnings(as.integer(x$mau %||% NA)),
            stringsAsFactors = FALSE
          )
        } else if (is.atomic(x)) {
          # If atomic, assume it's a date; DAU/MAU unknown
          data.frame(
            date = tryCatch(as.Date(x), error = function(e) as.Date(NA_character_)),
            dau = as.integer(NA),
            mau = as.integer(NA),
            stringsAsFactors = FALSE
          )
        } else {
          NULL
        }
      })
      rows <- rows[!sapply(rows, is.null)]
      if (length(rows) > 0) data_df <- do.call(rbind, rows)
    } else if (is.atomic(ph)) {
      # Case 3: atomic vector of dates
      data_df <- data.frame(
        date = tryCatch(as.Date(ph), error = function(e) as.Date(character())),
        dau = as.integer(NA),
        mau = as.integer(NA),
        stringsAsFactors = FALSE
      )
    }

    if (!is.null(data_df)) {
      data_df <- data_df[order(data_df$date), ]
      result$playerHistory <- data_df
    } else {
      result$playerHistory <- data.frame(
        date = as.Date(character()),
        dau = integer(),
        mau = integer(),
        stringsAsFactors = FALSE
      )
    }
  } else {
    # Return empty data frame with correct structure
    result$playerHistory <- data.frame(
      date = as.Date(character()),
      dau = integer(),
      mau = integer(),
      stringsAsFactors = FALSE
    )
  }
  
  return(result)
}