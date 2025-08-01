#' Batched Data Fetching Functions
#'
#' Internal functions that add batching capabilities to existing
#' date-based VGI functions to reduce API stress.
#'
#' @name batched_fetch
#' @keywords internal
NULL

#' Fetch Concurrent Players with Batching
#'
#' Wrapper around vgi_concurrent_players_by_date that adds batching
#' and rate limiting for multiple dates.
#'
#' @inheritParams batch_process_dates
#' @param steam_app_ids Steam App IDs to fetch
#' @param auth_token API authentication token
#' @return List of concurrent player data by date
#' @noRd
#' @keywords internal
fetch_concurrent_with_batching <- function(date_seq, 
                                         steam_app_ids,
                                         auth_token,
                                         batch_size = 10,
                                         batch_delay = 1,
                                         show_progress = TRUE) {
  
  api_call_count <- 0
  concurrent_data <- list()
  n_dates <- length(date_seq)
  
  for (i in seq_along(date_seq)) {
    date <- date_seq[i]
    
    # Show progress
    if (show_progress && i == 1) {
      message("Fetching concurrent players...")
    }
    
    tryCatch({
      api_call_count <- api_call_count + 1
      daily_ccu <- vgi_concurrent_players_by_date(
        date = as.character(date),
        steam_app_ids = steam_app_ids,
        auth_token = auth_token
      )
      
      if (!is.null(daily_ccu) && nrow(daily_ccu) > 0) {
        concurrent_data[[as.character(date)]] <- daily_ccu
        if (show_progress) {
          message(sprintf("  Day %d/%d: Retrieved %d games", i, n_dates, nrow(daily_ccu)))
        }
      }
    }, error = function(e) {
      warning(sprintf("Error fetching CCU for %s: %s", date, e$message))
    })
    
    # Apply rate limiting
    if (i %% batch_size == 0 && i < n_dates && batch_delay > 0) {
      if (show_progress) {
        message(sprintf("  Rate limiting: pausing %g seconds...", batch_delay))
      }
      Sys.sleep(batch_delay)
    }
  }
  
  attr(concurrent_data, "api_calls") <- api_call_count
  return(concurrent_data)
}

#' Fetch Revenue with Batching
#'
#' @inheritParams fetch_concurrent_with_batching
#' @noRd
#' @keywords internal
fetch_revenue_with_batching <- function(date_seq, 
                                      steam_app_ids,
                                      auth_token,
                                      batch_size = 10,
                                      batch_delay = 1,
                                      show_progress = TRUE) {
  
  api_call_count <- 0
  revenue_data <- list()
  n_dates <- length(date_seq)
  
  for (i in seq_along(date_seq)) {
    date <- date_seq[i]
    
    if (show_progress && i == 1) {
      message("Fetching revenue data...")
    }
    
    tryCatch({
      api_call_count <- api_call_count + 1
      daily_rev <- vgi_revenue_by_date(
        date = as.character(date),
        steam_app_ids = steam_app_ids,
        auth_token = auth_token
      )
      
      if (!is.null(daily_rev) && nrow(daily_rev) > 0) {
        revenue_data[[as.character(date)]] <- daily_rev
        if (show_progress && i %% 10 == 0) {
          message(sprintf("  Progress: %d/%d dates", i, n_dates))
        }
      }
    }, error = function(e) {
      warning(sprintf("Error fetching revenue for %s: %s", date, e$message))
    })
    
    # Apply rate limiting
    if (i %% batch_size == 0 && i < n_dates && batch_delay > 0) {
      if (show_progress) {
        message(sprintf("  Rate limiting: pausing %g seconds...", batch_delay))
      }
      Sys.sleep(batch_delay)
    }
  }
  
  attr(revenue_data, "api_calls") <- api_call_count
  return(revenue_data)
}

#' Apply Batching to Any Date-based Function
#'
#' Generic function to apply batching to any VGI function that takes dates.
#'
#' @param date_seq Sequence of dates to process
#' @param fetch_function The VGI function to call for each date
#' @param batch_config List with batch_size and batch_delay
#' @param show_progress Whether to show progress messages
#' @param ... Additional arguments passed to fetch_function
#' @return List of results by date with api_calls attribute
#' @noRd
#' @keywords internal
apply_batching <- function(date_seq,
                          fetch_function,
                          metric_name = "data",
                          batch_config = list(batch_size = 10, batch_delay = 1),
                          show_progress = TRUE,
                          ...) {
  
  api_call_count <- 0
  results <- list()
  n_dates <- length(date_seq)
  
  if (show_progress) {
    message(sprintf("Fetching %s...", metric_name))
  }
  
  start_time <- Sys.time()
  
  for (i in seq_along(date_seq)) {
    date <- date_seq[i]
    
    tryCatch({
      api_call_count <- api_call_count + 1
      result <- fetch_function(date = as.character(date), ...)
      
      if (!is.null(result) && nrow(result) > 0) {
        results[[as.character(date)]] <- result
      }
      
      # Progress reporting
      if (show_progress && (i %% 10 == 0 || i == n_dates)) {
        elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
        rate <- i / elapsed
        remaining <- (n_dates - i) / rate
        message(sprintf("  Progress: %d/%d (%.1f%%) - Est. remaining: %.0f seconds",
                       i, n_dates, (i/n_dates)*100, remaining))
      }
    }, error = function(e) {
      warning(sprintf("Error fetching %s for %s: %s", metric_name, date, e$message))
    })
    
    # Apply rate limiting
    if (i %% batch_config$batch_size == 0 && i < n_dates && batch_config$batch_delay > 0) {
      if (show_progress) {
        message(sprintf("  Rate limiting: pausing %g seconds...", batch_config$batch_delay))
      }
      Sys.sleep(batch_config$batch_delay)
    }
  }
  
  if (show_progress && n_dates > 0) {
    total_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    message(sprintf("  Completed %d dates in %.1f seconds (%.2f dates/second)",
                   n_dates, total_time, n_dates/total_time))
  }
  
  attr(results, "api_calls") <- api_call_count
  return(results)
}