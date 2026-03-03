#' Get Active Players Data by Date
#'
#' Retrieve daily and monthly active user (DAU/MAU) data for all games
#' on a specific date, providing engagement metrics across the market.
#'
#' @param date Character string or Date. The date for which to retrieve data
#'   in "YYYY-MM-DD" format.
#' @param steam_app_ids Numeric vector. Optional Steam App IDs to filter results.
#'   If not provided, returns data for all available games.
#' @param offset Integer. How many results to skip over. Minimum is 0. Optional.
#' @param limit Integer. Maximum number of results to return. Minimum is 1, 
#'   maximum is 1000. Optional.
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{steamAppId}{Integer. The Steam App ID}
#'   \item{date}{Character. The date of the data}
#'   \item{dau}{Integer. Daily active users}
#'   \item{mau}{Integer. Monthly active users}
#'   \item{dauMauRatio}{Numeric. DAU/MAU ratio (engagement rate)}
#'   \item{activeRank}{Integer. Rank by DAU}
#' }
#'
#' @details
#' Active player metrics provide deeper engagement insights than concurrent players:
#' \itemize{
#'   \item DAU: Unique players who launched the game on this date
#'   \item MAU: Unique players who launched the game in the past 30 days
#'   \item DAU/MAU ratio: Key metric for player retention (higher = better)
#'   \item Industry benchmark: 10-20\% is good, >25\% is excellent
#' }
#' 
#' These metrics are essential for:
#' \itemize{
#'   \item Measuring true player engagement
#'   \item Identifying games with strong retention
#'   \item Tracking seasonal patterns
#'   \item Comparing engagement across genres
#'   \item Forecasting player trends
#' }
#'
#' @export
#' @examples
#' \dontrun{
#' # Get active player data
#' active_data <- vgi_active_players_by_date("2024-01-15")
#' 
#' # Top 20 games by DAU
#' top_dau <- head(active_data, 20)
#' cat("Top 20 games by daily active users:\n")
#' print(top_dau[, c("steamAppId", "dau", "mau", "dauMauRatio")])
#' 
#' # Find games with excellent retention (DAU/MAU > 25%)
#' high_retention <- active_data[active_data$dauMauRatio > 0.25 & 
#'                              active_data$dau > 1000, ]
#' cat("Games with >25% DAU/MAU ratio:", nrow(high_retention), "\n")
#' print(head(high_retention[order(-high_retention$dauMauRatio), ], 10))
#' 
#' # Compare with concurrent player data
#' ccu_data <- vgi_concurrent_players_by_date("2024-01-15")
#' engagement <- merge(active_data, ccu_data, by = "steamAppId")
#' 
#' # Calculate concurrent-to-DAU ratio (session intensity)
#' engagement$ccu_dau_ratio <- engagement$peakConcurrent / (engagement$dau + 1)
#' 
#' # Games with high session intensity (many concurrent per daily user)
#' high_intensity <- engagement[engagement$ccu_dau_ratio > 0.3 & 
#'                             engagement$dau > 1000, ]
#' cat("High session intensity games:", nrow(high_intensity), "\n")
#' 
#' # Analyze retention tiers
#' active_data$retention_tier <- cut(active_data$dauMauRatio,
#'                                   breaks = c(0, 0.1, 0.2, 0.3, 0.5, 1),
#'                                   labels = c("Poor", "Below Avg", "Good", 
#'                                             "Excellent", "Outstanding"))
#' 
#' retention_summary <- table(active_data$retention_tier[active_data$dau > 100])
#' barplot(retention_summary,
#'         main = "Games by Retention Tier (DAU > 100)",
#'         xlab = "Retention Tier",
#'         ylab = "Number of Games",
#'         col = rainbow(5))
#' 
#' # Monthly trend analysis
#' month_ago <- as.Date("2024-01-15") - 30
#' active_prev <- vgi_active_players_by_date(as.character(month_ago))
#' 
#' trend <- merge(active_data, active_prev,
#'               by = "steamAppId",
#'               suffixes = c("_now", "_prev"))
#' 
#' # Calculate monthly growth
#' trend$dau_growth <- ((trend$dau_now - trend$dau_prev) / 
#'                     (trend$dau_prev + 1)) * 100
#' trend$mau_growth <- ((trend$mau_now - trend$mau_prev) / 
#'                     (trend$mau_prev + 1)) * 100
#' 
#' # Find rapidly growing games
#' growing <- trend[trend$dau_growth > 50 & trend$dau_now > 1000, ]
#' cat("Games with >50% DAU growth:", nrow(growing), "\n")
#' 
#' # Identify games losing players
#' declining <- trend[trend$mau_growth < -20 & trend$mau_prev > 5000, ]
#' cat("Games losing >20% MAU:", nrow(declining), "\n")
#' print(head(declining[order(declining$mau_growth), 
#'            c("steamAppId", "mau_prev", "mau_now", "mau_growth")]))
#' }
vgi_active_players_by_date <- function(date,
                                      steam_app_ids = NULL,
                                      offset = NULL,
                                      limit = NULL,
                                      auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                                      headers = list()) {
  
  # Validate and format date
  formatted_date <- format_date(date)
  requested_date <- as.Date(formatted_date)
  
  # Validate inputs
  if (!is.null(offset)) {
    validate_numeric(offset, "offset", min_val = 0)
  }
  
  if (!is.null(limit)) {
    validate_numeric(limit, "limit", min_val = 1, max_val = 1000)
  }
  
  steam_app_ids <- if (is.null(steam_app_ids)) NULL else as.numeric(steam_app_ids)
  fetch_limit <- if (!is.null(limit)) {
    as.integer(limit + (offset %||% 0))
  } else if (!is.null(steam_app_ids)) {
    max(50, length(steam_app_ids) * 5)
  } else {
    2000
  }

  result <- .vgi_historical_results(
    date = formatted_date,
    steam_app_ids = steam_app_ids,
    limit = fetch_limit,
    auth_token = auth_token,
    headers = headers
  )

  if (!is.data.frame(result) || nrow(result) == 0) {
    empty_df <- .vgi_clean_names(data.frame(
      steamAppId = integer(),
      date = character(),
      dau = integer(),
      mau = integer(),
      dauMauRatio = numeric(),
      activeRank = integer(),
      stringsAsFactors = FALSE
    ))
    attr(empty_df, "requested_date") <- date
    attr(empty_df, "actual_date") <- formatted_date
    return(empty_df)
  }

  if ("platform" %in% names(result)) {
    result <- result[result$platform == "steam", , drop = FALSE]
  }

  df <- data.frame(
    steamAppId = as.integer(result$externalId %||% NA),
    date = formatted_date,
    dau = as.integer(result$dau %||% NA),
    mau = as.integer(result$mau %||% NA),
    stringsAsFactors = FALSE
  )
  df <- df[!is.na(df$steamAppId), , drop = FALSE]
  if (nrow(df) == 0) {
    empty_df <- .vgi_clean_names(data.frame(
      steamAppId = integer(),
      date = character(),
      dau = integer(),
      mau = integer(),
      dauMauRatio = numeric(),
      activeRank = integer(),
      stringsAsFactors = FALSE
    ))
    attr(empty_df, "requested_date") <- date
    attr(empty_df, "actual_date") <- formatted_date
    return(empty_df)
  }

  df$dauMauRatio <- ifelse(df$mau > 0, df$dau / df$mau, NA_real_)
  df <- df[order(-df$dau), , drop = FALSE]
  df$activeRank <- seq_len(nrow(df))

  if (!is.null(offset) && offset > 0 && nrow(df) > offset) {
    df <- df[(offset + 1):nrow(df), , drop = FALSE]
  } else if (!is.null(offset) && offset >= nrow(df)) {
    df <- df[0, , drop = FALSE]
  }
  if (!is.null(limit) && nrow(df) > limit) {
    df <- df[seq_len(limit), , drop = FALSE]
  }

  warn_if_stale_ids(df$steamAppId)
  out <- .vgi_clean_names(df)
  attr(out, "requested_date") <- requested_date
  attr(out, "actual_date") <- formatted_date
  out
}