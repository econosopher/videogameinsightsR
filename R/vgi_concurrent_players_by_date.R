#' Get Concurrent Players Data by Date
#'
#' Retrieve concurrent player counts for all games on a specific date,
#' providing a snapshot of active player engagement across the market.
#'
#' @param date Character string or Date. The date for which to retrieve data
#'   in "YYYY-MM-DD" format.
#' @param steam_app_ids Numeric vector. Optional Steam App IDs to filter results.
#'   If not provided, returns data for all available games.
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{steamAppId}{Integer. The Steam App ID}
#'   \item{date}{Character. The date of the data}
#'   \item{peakConcurrent}{Integer. Peak concurrent players on this date}
#'   \item{avgConcurrent}{Integer. Average concurrent players on this date}
#'   \item{concurrentRank}{Integer. Rank by peak concurrent players}
#' }
#'
#' @details
#' Concurrent player data is one of the most important engagement metrics:
#' \itemize{
#'   \item Real-time indicator of game health
#'   \item Direct measure of active player base
#'   \item Useful for identifying trending games
#'   \item Critical for multiplayer game analysis
#'   \item Key metric for seasonal event planning
#' }
#' 
#' Peak concurrent typically occurs during prime gaming hours,
#' while average provides a more stable engagement metric.
#'
#' @export
#' @examples
#' \dontrun{
#' # Get concurrent player data
#' ccu_data <- vgi_concurrent_players_by_date("2024-01-15")
#' 
#' # Top 20 games by concurrent players
#' top_ccu <- head(ccu_data, 20)
#' cat("Top 20 games by peak concurrent players:\n")
#' print(top_ccu[, c("steamAppId", "peakConcurrent", "avgConcurrent")])
#' 
#' # Calculate peak-to-average ratio (indicates play pattern)
#' ccu_data$peak_avg_ratio <- ccu_data$peakConcurrent / 
#'                           (ccu_data$avgConcurrent + 1)
#' 
#' # Games with spiky play patterns (high peak/avg ratio)
#' spiky_games <- ccu_data[ccu_data$peak_avg_ratio > 3 & 
#'                         ccu_data$peakConcurrent > 1000, ]
#' cat("Games with concentrated play times:", nrow(spiky_games), "\n")
#' 
#' # Compare weekend vs weekday
#' is_weekend <- weekdays(as.Date("2024-01-15")) %in% c("Saturday", "Sunday")
#' if (!is_weekend) {
#'   # Get previous weekend data
#'   last_saturday <- as.Date("2024-01-15") - 
#'                   ((as.numeric(format(as.Date("2024-01-15"), "%w")) + 1) %% 7)
#'   weekend_ccu <- vgi_concurrent_players_by_date(as.character(last_saturday))
#'   
#'   comparison <- merge(ccu_data, weekend_ccu,
#'                      by = "steamAppId",
#'                      suffixes = c("_weekday", "_weekend"))
#'   
#'   # Calculate weekend boost
#'   comparison$weekend_boost <- (comparison$peakConcurrent_weekend - 
#'                               comparison$peakConcurrent_weekday) / 
#'                              comparison$peakConcurrent_weekday * 100
#'   
#'   # Games with biggest weekend boost
#'   weekend_games <- comparison[comparison$weekend_boost > 50 & 
#'                              comparison$peakConcurrent_weekday > 500, ]
#'   cat("Games with >50% weekend boost:", nrow(weekend_games), "\n")
#' }
#' 
#' # Analyze player concentration
#' total_players <- sum(ccu_data$peakConcurrent)
#' top_10_players <- sum(head(ccu_data$peakConcurrent, 10))
#' concentration <- (top_10_players / total_players) * 100
#' 
#' cat(sprintf("Top 10 games account for %.1f%% of all concurrent players\n", 
#'             concentration))
#' 
#' # Distribution analysis
#' hist(log10(ccu_data$peakConcurrent + 1),
#'      breaks = 40,
#'      main = "Distribution of Peak Concurrent Players (log scale)",
#'      xlab = "Log10(Peak CCU + 1)",
#'      col = "orange")
#' 
#' # Find multiplayer vs single-player patterns
#' # (Multiplayer games typically have higher avg/peak ratios)
#' ccu_data$avg_peak_ratio <- ccu_data$avgConcurrent / 
#'                           (ccu_data$peakConcurrent + 1)
#' likely_multiplayer <- ccu_data[ccu_data$avg_peak_ratio > 0.6 & 
#'                               ccu_data$peakConcurrent > 1000, ]
#' cat("Likely multiplayer games (high avg/peak):", nrow(likely_multiplayer), "\n")
#' }
vgi_concurrent_players_by_date <- function(date,
                                          steam_app_ids = NULL,
                                          auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                                          headers = list()) {
  
  # Validate and format date
  formatted_date <- format_date(date)
  
  # Build query parameters if steam_app_ids provided
  query_params <- list()
  if (!is.null(steam_app_ids)) {
    # Convert to comma-separated string
    ids_string <- paste(steam_app_ids, collapse = ",")
    query_params$steamAppIds <- ids_string
  }
  
  # Make API request
  result <- make_api_request(
    endpoint = paste0("engagement/concurrent-players/", formatted_date),
    query_params = query_params,
    auth_token = auth_token,
    method = "GET",
    headers = headers
  )
  
  # Handle different response formats
  if (is.null(result) || length(result) == 0) {
    # Return empty data frame with correct structure
    return(data.frame(
      steamAppId = integer(),
      date = character(),
      peakConcurrent = integer(),
      avgConcurrent = integer(),
      concurrentRank = integer(),
      stringsAsFactors = FALSE
    ))
  }
  
  # If result is already a data frame or tibble
  if (is.data.frame(result)) {
    df <- result
    # Ensure column names are standardized
    if ("steam_app_id" %in% names(df) && !"steamAppId" %in% names(df)) {
      df$steamAppId <- df$steam_app_id
    }
    df$date <- formatted_date
  } else if (is.list(result)) {
    # Check if it's a list with a data element
    if (!is.null(result$data)) {
      df <- as.data.frame(result$data)
    } else {
      # Process list of game data
      df_list <- lapply(seq_along(result), function(i) {
        item <- result[[i]]
        
        # Handle case where item is a list
        if (is.list(item) && !is.data.frame(item)) {
          # Check if we have the new format (avg, median, max, min)
          if (!is.null(item$avg) || !is.null(item$max)) {
            data.frame(
              steamAppId = as.integer(item$steamAppId %||% item$steam_app_id %||% NA),
              date = item$date %||% formatted_date,
              avg = as.numeric(item$avg %||% 0),
              median = as.numeric(item$median %||% 0),
              max = as.integer(item$max %||% 0),
              min = as.integer(item$min %||% 0),
              stringsAsFactors = FALSE
            )
          } else {
            # Old format with peakConcurrent/avgConcurrent
            data.frame(
              steamAppId = as.integer(item$steamAppId %||% item$steam_app_id %||% NA),
              peakConcurrent = as.integer(item$peakConcurrent %||% item$peak_concurrent %||% 0),
              avgConcurrent = as.integer(item$avgConcurrent %||% item$avg_concurrent %||% 0),
              stringsAsFactors = FALSE
            )
          }
        } else if (is.numeric(item) || is.character(item)) {
          # Handle case where result is a simple vector
          data.frame(
            steamAppId = as.integer(names(result)[i]),
            peakConcurrent = as.integer(item),
            avgConcurrent = 0,
            stringsAsFactors = FALSE
          )
        } else {
          NULL
        }
      })
      
      # Remove NULL entries and bind
      df_list <- df_list[!sapply(df_list, is.null)]
      if (length(df_list) > 0) {
        df <- do.call(rbind, df_list)
      } else {
        return(data.frame(
          steamAppId = integer(),
          date = character(),
          peakConcurrent = integer(),
          avgConcurrent = integer(),
          concurrentRank = integer(),
          stringsAsFactors = FALSE
        ))
      }
    }
    if (!"date" %in% names(df)) df$date <- formatted_date
  } else {
    stop("Unexpected response format from API")
  }
  
  # Check if we have the new format (avg, median, max, min) and convert to old format
  if ("avg" %in% names(df) && "max" %in% names(df)) {
    # Use max as peakConcurrent and avg as avgConcurrent for backward compatibility
    df$peakConcurrent <- df$max
    df$avgConcurrent <- df$avg
  }
  
  # Ensure required columns exist
  if (!"avgConcurrent" %in% names(df)) df$avgConcurrent <- 0
  if (!"peakConcurrent" %in% names(df)) df$peakConcurrent <- 0
  
  # Sort by peak concurrent descending and add rank
  df <- df[order(-df$peakConcurrent), ]
  df$concurrentRank <- seq_len(nrow(df))
  
  # Return standardized data frame
  return(data.frame(
    steamAppId = as.integer(df$steamAppId),
    date = df$date,
    peakConcurrent = as.integer(df$peakConcurrent),
    avgConcurrent = as.integer(df$avgConcurrent),
    concurrentRank = as.integer(df$concurrentRank),
    stringsAsFactors = FALSE
  ))
}