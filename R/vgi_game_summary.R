#' Get Comprehensive Game Summary
#'
#' Retrieve all available metrics for specified games over a date range.
#' This function aggregates data from multiple VGI endpoints to provide
#' a complete overview of game performance.
#'
#' @param steam_app_ids Numeric vector. Steam App IDs to analyze.
#' @param start_date Character string or Date. Start date for the analysis.
#' @param end_date Character string or Date. End date for the analysis. 
#'   Defaults to start_date if not provided (single day analysis).
#' @param metrics Character vector. Which metrics to retrieve. Defaults to all.
#'   Options: "concurrent", "active", "revenue", "units", "reviews", 
#'   "followers", "wishlists", "price", "metadata"
#' @param game_names Character vector. Optional game names corresponding to
#'   steam_app_ids. If not provided, names will be fetched from metadata.
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#'
#' @return A list containing:
#' \describe{
#'   \item{summary_table}{Aggregated metrics for each game}
#'   \item{time_series}{List of time series data frames by metric}
#'   \item{metadata}{Game metadata if requested}
#'   \item{api_calls}{Number of API calls made}
#'   \item{errors}{Any errors encountered during data collection}
#' }
#'
#' @details
#' This function makes multiple API calls to gather comprehensive data.
#' For efficiency, it uses the steamAppIds parameter where supported.
#' 
#' Note: Some endpoints may have data availability limitations:
#' - Active players: DAU from 2024-03-18, MAU from 2024-03-23
#' - Some metrics may not be available for all games
#'
#' @export
#' @examples
#' \dontrun{
#' # Analyze a single game for one week
#' bf5_summary <- vgi_game_summary(
#'   steam_app_ids = 1238810,  # Battlefield V
#'   start_date = "2024-07-01",
#'   end_date = "2024-07-07"
#' )
#' 
#' # Analyze multiple games
#' battlefield_summary <- vgi_game_summary(
#'   steam_app_ids = c(1517290, 1238810),  # BF2042 and BFV
#'   start_date = "2024-07-01", 
#'   end_date = "2024-07-07",
#'   metrics = c("concurrent", "active", "revenue", "units")
#' )
#' 
#' # View summary table
#' print(battlefield_summary$summary_table)
#' 
#' # Plot time series
#' ggplot(battlefield_summary$time_series$concurrent,
#'        aes(x = date, y = peakConcurrent, color = factor(steamAppId))) +
#'   geom_line() +
#'   labs(title = "Peak CCU Over Time")
#' }
vgi_game_summary <- function(steam_app_ids,
                           start_date,
                           end_date = NULL,
                           metrics = c("concurrent", "active", "revenue", "units"),
                           game_names = NULL,
                           auth_token = Sys.getenv("VGI_AUTH_TOKEN")) {
  
  # Initialize tracking
  results <- list(
    summary_table = NULL,
    time_series = list(),
    metadata = NULL,
    api_calls = 0,
    errors = list()
  )
  
  # Format dates
  start_date <- format_date(start_date)
  if (is.null(end_date)) {
    end_date <- start_date
  } else {
    end_date <- format_date(end_date)
  }
  
  # Create date sequence
  date_seq <- seq(as.Date(start_date), as.Date(end_date), by = "day")
  
  # Ensure steam_app_ids is numeric
  steam_app_ids <- as.numeric(steam_app_ids)
  
  message(sprintf("Fetching data for %d games from %s to %s", 
                  length(steam_app_ids), start_date, end_date))
  
  # Calculate expected API calls
  n_days <- length(date_seq)
  n_games <- length(steam_app_ids)
  expected_calls <- 0
  
  if ("concurrent" %in% metrics) expected_calls <- expected_calls + n_days
  if ("active" %in% metrics) expected_calls <- expected_calls + n_days
  if ("revenue" %in% metrics) expected_calls <- expected_calls + n_days
  if ("units" %in% metrics) expected_calls <- expected_calls + n_days
  if ("reviews" %in% metrics) expected_calls <- expected_calls + n_days
  if ("followers" %in% metrics) expected_calls <- expected_calls + n_days
  if ("wishlists" %in% metrics) expected_calls <- expected_calls + n_days
  if ("metadata" %in% metrics) expected_calls <- expected_calls + n_games
  
  message(sprintf("Expected API calls: ~%d (for %d metrics over %d days)", 
                  expected_calls, length(metrics), n_days))
  
  # 1. CONCURRENT PLAYERS
  if ("concurrent" %in% metrics) {
    message("Fetching concurrent players...")
    concurrent_data <- list()
    
    for (i in seq_along(date_seq)) {
      date <- date_seq[i]
      tryCatch({
        results$api_calls <- results$api_calls + 1  # Move to beginning
        daily_ccu <- vgi_concurrent_players_by_date(
          date = as.character(date),
          steam_app_ids = steam_app_ids,
          auth_token = auth_token
        )
        if (!is.null(daily_ccu) && nrow(daily_ccu) > 0) {
          concurrent_data[[as.character(date)]] <- daily_ccu
          message(paste0("  Day ", i, "/", length(date_seq), ": Retrieved ", 
                        nrow(daily_ccu), " games"))
        } else {
          message(paste0("  Day ", i, "/", length(date_seq), ": No data"))
        }
      }, error = function(e) {
        message(paste0("  Day ", i, "/", length(date_seq), ": Error - ", e$message))
        results$errors[["concurrent"]] <- c(results$errors[["concurrent"]], 
                                          paste0(date, ": ", e$message))
      })
    }
    
    if (length(concurrent_data) > 0) {
      results$time_series$concurrent <- do.call(rbind, concurrent_data)
    }
  }
  
  # 2. ACTIVE PLAYERS
  if ("active" %in% metrics) {
    message("Fetching active players...")
    active_data <- list()
    
    # Check if any dates are before DAU availability
    dau_start <- as.Date("2024-03-18")
    if (any(date_seq < dau_start)) {
      message("  Note: DAU data only available from 2024-03-18 onwards")
    }
    
    for (i in seq_along(date_seq)) {
      date <- date_seq[i]
      tryCatch({
        # Skip dates before DAU availability
        if (date < dau_start) {
          next
        }
        
        results$api_calls <- results$api_calls + 1  # Move to before API call
        daily_active <- suppressWarnings({
          vgi_active_players_by_date(
            date = as.character(date),
            steam_app_ids = steam_app_ids,
            auth_token = auth_token
          )
        })
        
        if (!is.null(daily_active) && nrow(daily_active) > 0) {
          active_data[[as.character(date)]] <- daily_active
        }
      }, error = function(e) {
        # Only log non-date-related errors
        if (!grepl("DAU data|only available from", e$message)) {
          results$errors[["active"]] <- c(results$errors[["active"]], 
                                         paste0(date, ": ", e$message))
        }
      })
    }
    
    if (length(active_data) > 0) {
      results$time_series$active <- do.call(rbind, active_data)
      message(paste0("  Retrieved active player data for ", length(active_data), " days"))
    } else {
      message("  No active player data retrieved (dates may be before availability)")
    }
  }
  
  # 3. REVENUE (if function supports multiple IDs)
  if ("revenue" %in% metrics) {
    message("Fetching revenue data...")
    revenue_data <- list()
    revenue_warnings <- 0
    
    # Now that revenue supports steam_app_ids, we can fetch all games at once
    for (i in seq_along(date_seq)) {
      date <- date_seq[i]
      tryCatch({
        results$api_calls <- results$api_calls + 1  # Move to beginning
        daily_rev <- vgi_revenue_by_date(
          date = as.character(date),
          steam_app_ids = steam_app_ids,  # Use the new parameter
          auth_token = auth_token
        )
        
        if (!is.null(daily_rev) && nrow(daily_rev) > 0) {
          revenue_data[[as.character(date)]] <- daily_rev
        }
      }, error = function(e) {
          revenue_warnings <- revenue_warnings + 1
          # Only log unique errors
          if (!e$message %in% results$errors[["revenue"]]) {
            results$errors[["revenue"]] <- c(results$errors[["revenue"]], e$message)
          }
          message(paste0("  Revenue error for ", date, ": ", e$message))
        })
    }
    
    if (length(revenue_data) > 0) {
      results$time_series$revenue <- do.call(rbind, revenue_data)
      message(paste0("  Retrieved revenue data for ", length(revenue_data), " game-days"))
    } else {
      message("  No revenue data retrieved (may not be available for these games)")
    }
    
    if (revenue_warnings > 0) {
      message(paste0("  Note: ", revenue_warnings, " revenue queries had no data"))
    }
  }
  
  # ALWAYS FETCH METADATA FOR GAME NAMES
  # This is done regardless of metrics parameter to ensure we have game names
  message("Fetching game metadata...")
  metadata_list <- list()
  metadata_failures <- 0
  
  for (id in steam_app_ids) {
    tryCatch({
      results$api_calls <- results$api_calls + 1
      game_meta <- vgi_game_metadata(
        steam_app_id = id,
        auth_token = auth_token
      )
      
      if (!is.null(game_meta) && nrow(game_meta) > 0) {
        metadata_list[[as.character(id)]] <- game_meta
      } else {
        metadata_failures <- metadata_failures + 1
      }
    }, error = function(e) {
      metadata_failures <- metadata_failures + 1
      # Only log unique errors
      if (!e$message %in% results$errors[["metadata"]]) {
        results$errors[["metadata"]] <- c(results$errors[["metadata"]], e$message)
      }
    })
  }
  
  if (length(metadata_list) > 0) {
    results$metadata <- do.call(rbind, metadata_list)
    message(paste0("  Retrieved metadata for ", length(metadata_list), " games"))
  } else {
    message("  No metadata retrieved")
  }
  
  if (metadata_failures > 0) {
    message(paste0("  Note: ", metadata_failures, " games had no metadata available"))
  }
  
  # 5. UNITS SOLD
  if ("units" %in% metrics) {
    message("Fetching units sold data...")
    units_data <- list()
    
    for (i in seq_along(date_seq)) {
      date <- date_seq[i]
      tryCatch({
        results$api_calls <- results$api_calls + 1
        daily_units <- vgi_units_sold_by_date(
          date = as.character(date),
          steam_app_ids = steam_app_ids,
          auth_token = auth_token
        )
        
        if (!is.null(daily_units) && nrow(daily_units) > 0) {
          units_data[[as.character(date)]] <- daily_units
        }
      }, error = function(e) {
        if (!e$message %in% results$errors[["units"]]) {
          results$errors[["units"]] <- c(results$errors[["units"]], e$message)
        }
        message(paste0("  Units error for ", date, ": ", e$message))
      })
    }
    
    if (length(units_data) > 0) {
      results$time_series$units <- do.call(rbind, units_data)
      message(paste0("  Retrieved units data for ", length(units_data), " days"))
    } else {
      message("  No units sold data retrieved")
    }
  }
  
  # 6. REVIEWS
  if ("reviews" %in% metrics) {
    message("Fetching reviews data...")
    reviews_data <- list()
    
    for (date in date_seq) {
      tryCatch({
        results$api_calls <- results$api_calls + 1
        daily_reviews <- suppressWarnings({
          vgi_reviews_by_date(
            date = as.character(date),
            steam_app_ids = steam_app_ids,
            auth_token = auth_token
          )
        })
        
        if (!is.null(daily_reviews) && nrow(daily_reviews) > 0) {
          reviews_data[[as.character(date)]] <- daily_reviews
        }
      }, error = function(e) {
        if (!e$message %in% results$errors[["reviews"]]) {
          results$errors[["reviews"]] <- c(results$errors[["reviews"]], e$message)
        }
      })
    }
    
    if (length(reviews_data) > 0) {
      results$time_series$reviews <- do.call(rbind, reviews_data)
      message(paste0("  Retrieved reviews data for ", length(reviews_data), " days"))
    } else {
      message("  No reviews data retrieved")
    }
  }
  
  # 7. FOLLOWERS
  if ("followers" %in% metrics) {
    message("Fetching followers data...")
    followers_data <- list()
    
    for (date in date_seq) {
      tryCatch({
        results$api_calls <- results$api_calls + 1
        daily_followers <- suppressWarnings({
          vgi_followers_by_date(
            date = as.character(date),
            steam_app_ids = steam_app_ids,
            auth_token = auth_token
          )
        })
        
        if (!is.null(daily_followers) && nrow(daily_followers) > 0) {
          followers_data[[as.character(date)]] <- daily_followers
        }
      }, error = function(e) {
        if (!e$message %in% results$errors[["followers"]]) {
          results$errors[["followers"]] <- c(results$errors[["followers"]], e$message)
        }
      })
    }
    
    if (length(followers_data) > 0) {
      results$time_series$followers <- do.call(rbind, followers_data)
      message(paste0("  Retrieved followers data for ", length(followers_data), " days"))
    } else {
      message("  No followers data retrieved")
    }
  }
  
  # 8. WISHLISTS
  if ("wishlists" %in% metrics) {
    message("Fetching wishlists data...")
    wishlists_data <- list()
    
    for (date in date_seq) {
      tryCatch({
        results$api_calls <- results$api_calls + 1
        daily_wishlists <- suppressWarnings({
          vgi_wishlists_by_date(
            date = as.character(date),
            steam_app_ids = steam_app_ids,
            auth_token = auth_token
          )
        })
        
        if (!is.null(daily_wishlists) && nrow(daily_wishlists) > 0) {
          wishlists_data[[as.character(date)]] <- daily_wishlists
        }
      }, error = function(e) {
        if (!e$message %in% results$errors[["wishlists"]]) {
          results$errors[["wishlists"]] <- c(results$errors[["wishlists"]], e$message)
        }
      })
    }
    
    if (length(wishlists_data) > 0) {
      results$time_series$wishlists <- do.call(rbind, wishlists_data)
      message(paste0("  Retrieved wishlists data for ", length(wishlists_data), " days"))
    } else {
      message("  No wishlists data retrieved")
    }
  }
  
  # CREATE SUMMARY TABLE
  message("Creating summary table...")
  summary_data <- list()
  
  for (id in steam_app_ids) {
    game_summary <- data.frame(
      steamAppId = id,
      stringsAsFactors = FALSE
    )
    
    # Add game name from metadata
    if (!is.null(results$metadata)) {
      game_meta <- results$metadata[results$metadata$steamAppId == id, ]
      if (nrow(game_meta) > 0) {
        game_summary$name <- game_meta$name[1]
      }
    }
    
    # Aggregate concurrent players
    if (!is.null(results$time_series$concurrent)) {
      game_ccu <- results$time_series$concurrent[results$time_series$concurrent$steamAppId == id, ]
      if (nrow(game_ccu) > 0) {
        game_summary$avg_peak_ccu <- round(mean(game_ccu$peakConcurrent, na.rm = TRUE))
        game_summary$max_peak_ccu <- max(game_ccu$peakConcurrent, na.rm = TRUE)
        game_summary$avg_avg_ccu <- round(mean(game_ccu$avgConcurrent, na.rm = TRUE))
      }
    }
    
    # Aggregate active players
    if (!is.null(results$time_series$active)) {
      game_active <- results$time_series$active[results$time_series$active$steamAppId == id, ]
      if (nrow(game_active) > 0) {
        game_summary$avg_dau <- round(mean(game_active$dau, na.rm = TRUE))
        game_summary$avg_mau <- round(mean(game_active$mau, na.rm = TRUE))
        game_summary$avg_dau_mau_ratio <- round(mean(game_active$dauMauRatio, na.rm = TRUE), 3)
      }
    }
    
    # Aggregate revenue
    if (!is.null(results$time_series$revenue)) {
      game_rev <- results$time_series$revenue[results$time_series$revenue$steamAppId == id, ]
      if (nrow(game_rev) > 0) {
        game_summary$total_revenue <- sum(game_rev$dailyRevenue, na.rm = TRUE)
        game_summary$avg_daily_revenue <- round(mean(game_rev$dailyRevenue, na.rm = TRUE))
      }
    }
    
    # Aggregate units sold
    if (!is.null(results$time_series$units)) {
      game_units <- results$time_series$units[results$time_series$units$steamAppId == id, ]
      if (nrow(game_units) > 0) {
        # Get latest total units and calculate daily average
        game_summary$total_units <- max(game_units$unitsSold, na.rm = TRUE)
        game_summary$avg_daily_units <- round(mean(game_units$dailyUnits, na.rm = TRUE))
      }
    }
    
    # Aggregate reviews
    if (!is.null(results$time_series$reviews)) {
      game_reviews <- results$time_series$reviews[results$time_series$reviews$steamAppId == id, ]
      if (nrow(game_reviews) > 0) {
        # Get latest review counts
        latest_reviews <- game_reviews[which.max(as.Date(game_reviews$date)), ]
        game_summary$total_reviews <- latest_reviews$totalReviews
        game_summary$positive_reviews <- latest_reviews$positiveReviews
        game_summary$negative_reviews <- latest_reviews$negativeReviews
        game_summary$positive_ratio <- round(latest_reviews$positiveRatio, 3)
      }
    }
    
    # Aggregate followers
    if (!is.null(results$time_series$followers)) {
      game_followers <- results$time_series$followers[results$time_series$followers$steamAppId == id, ]
      if (nrow(game_followers) > 0) {
        # Get latest follower count and calculate change
        game_summary$current_followers <- max(game_followers$followerCount, na.rm = TRUE)
        if (nrow(game_followers) > 1) {
          first_count <- game_followers$followerCount[1]
          last_count <- game_followers$followerCount[nrow(game_followers)]
          game_summary$follower_change <- last_count - first_count
        }
      }
    }
    
    # Aggregate wishlists
    if (!is.null(results$time_series$wishlists)) {
      game_wishlists <- results$time_series$wishlists[results$time_series$wishlists$steamAppId == id, ]
      if (nrow(game_wishlists) > 0) {
        # Get latest wishlist count and calculate change
        game_summary$current_wishlists <- max(game_wishlists$wishlistCount, na.rm = TRUE)
        if (nrow(game_wishlists) > 1) {
          first_count <- game_wishlists$wishlistCount[1]
          last_count <- game_wishlists$wishlistCount[nrow(game_wishlists)]
          game_summary$wishlist_change <- last_count - first_count
        }
      }
    }
    
    summary_data[[as.character(id)]] <- game_summary
  }
  
  if (length(summary_data) > 0) {
    results$summary_table <- do.call(rbind, summary_data)
  }
  
  # Add date range to results
  results$date_range <- list(start = start_date, end = end_date)
  results$games_analyzed <- steam_app_ids
  
  message(sprintf("Complete. Made %d API calls.", results$api_calls))
  
  # Debug: Show what data we collected
  if (results$api_calls == 0) {
    message("WARNING: No API calls were made. Check date range and metrics.")
  }
  if (length(results$errors) > 0) {
    warning(sprintf("Encountered %d errors. Check results$errors for details.", 
                   sum(sapply(results$errors, length))))
  }
  
  return(results)
}