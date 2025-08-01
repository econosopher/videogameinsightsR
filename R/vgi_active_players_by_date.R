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
  
  # Check DAU/MAU availability dates
  dau_start_date <- as.Date("2024-03-18")
  mau_start_date <- as.Date("2024-03-23")
  
  # If requested date is before DAU availability, adjust and warn
  if (requested_date < dau_start_date) {
    warning(paste0("DAU data is only available from 2024-03-18. ",
                   "Fetching data for the earliest available date instead."))
    formatted_date <- "2024-03-18"
  }
  
  # Validate inputs
  if (!is.null(offset)) {
    validate_numeric(offset, "offset", min_val = 0)
  }
  
  if (!is.null(limit)) {
    validate_numeric(limit, "limit", min_val = 1, max_val = 1000)
  }
  
  # Build query parameters
  query_params <- list()
  if (!is.null(steam_app_ids)) {
    # Convert to comma-separated string
    ids_string <- paste(steam_app_ids, collapse = ",")
    query_params$steamAppIds <- ids_string
  }
  if (!is.null(offset)) {
    query_params$offset <- offset
  }
  if (!is.null(limit)) {
    query_params$limit <- limit
  }
  
  # Make API request
  result <- make_api_request(
    endpoint = paste0("engagement/active-players/", formatted_date),
    query_params = query_params,
    auth_token = auth_token,
    method = "GET",
    headers = headers
  )
  
  # Convert to data frame
  if (is.list(result) && length(result) > 0) {
    df <- do.call(rbind, lapply(result, function(x) {
      dau <- as.integer(x$dau %||% 0)
      mau <- as.integer(x$mau %||% 0)
      
      data.frame(
        steamAppId = as.integer(x$steamAppId),
        date = formatted_date,
        dau = dau,
        mau = mau,
        dauMauRatio = if (mau > 0) dau / mau else NA,
        stringsAsFactors = FALSE
      )
    }))
    
    # Sort by DAU descending and add rank
    df <- df[order(-df$dau), ]
    df$activeRank <- seq_len(nrow(df))
    
    # Add metadata about the actual date returned
    attr(df, "requested_date") <- date
    attr(df, "actual_date") <- formatted_date
    
    # If date was adjusted, note that MAU might be limited
    if (requested_date < mau_start_date && requested_date >= dau_start_date) {
      attr(df, "mau_limited") <- TRUE
      message("Note: MAU data is only available from 2024-03-23. MAU values may be incomplete.")
    }
    
    # Add warning if only old games are returned
    if (all(df$steamAppId < 1000, na.rm = TRUE)) {
      warning("API returned only old games (Steam IDs < 1000). This may indicate stale data.")
    }
    
    return(df)
  } else {
    # Return empty data frame with correct structure
    empty_df <- data.frame(
      steamAppId = integer(),
      date = character(),
      dau = integer(),
      mau = integer(),
      dauMauRatio = numeric(),
      activeRank = integer(),
      stringsAsFactors = FALSE
    )
    
    # Add metadata even for empty results
    attr(empty_df, "requested_date") <- date
    attr(empty_df, "actual_date") <- formatted_date
    
    return(empty_df)
  }
}