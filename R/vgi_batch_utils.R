#' Batch Processing Utilities
#'
#' Internal utilities for batching API requests to reduce stress on the API
#' while maintaining performance and reliability.
#'
#' @name batch_utils
#' @keywords internal
NULL

#' Process Dates in Batches
#'
#' Internal function to process multiple dates with configurable batching
#' and rate limiting to reduce API stress.
#'
#' @param dates Vector of dates to process
#' @param fetch_fn Function to call for each date
#' @param batch_size Number of dates to process before pausing
#' @param delay_seconds Delay in seconds between batches
#' @param progress Logical. Show progress messages
#' @param ... Additional arguments passed to fetch_fn
#'
#' @return List of results keyed by date
#' @noRd
#' @keywords internal
batch_process_dates <- function(dates, 
                               fetch_fn, 
                               batch_size = 10,
                               delay_seconds = 1,
                               progress = TRUE,
                               ...) {
  
  if (length(dates) == 0) {
    return(list())
  }
  
  # Initialize results
  results <- list()
  total_dates <- length(dates)
  
  # Process in batches
  for (i in seq_along(dates)) {
    date <- dates[i]
    
    # Show progress
    if (progress && (i %% 10 == 1 || i == 1)) {
      message(sprintf("  Processing dates %d-%d of %d...", 
                     i, min(i + 9, total_dates), total_dates))
    }
    
    # Fetch data for this date
    tryCatch({
      result <- fetch_fn(date = as.character(date), ...)
      if (!is.null(result) && nrow(result) > 0) {
        results[[as.character(date)]] <- result
      }
    }, error = function(e) {
      warning(sprintf("Error fetching data for %s: %s", date, e$message))
    })
    
    # Apply rate limiting between batches
    if (i %% batch_size == 0 && i < total_dates) {
      if (progress) {
        message(sprintf("  Rate limiting: pausing for %g seconds...", delay_seconds))
      }
      Sys.sleep(delay_seconds)
    }
  }
  
  return(results)
}

#' Get Batch Configuration
#'
#' Get batch processing configuration from environment or defaults.
#' Users can control batching behavior via environment variables.
#'
#' @return List with batch_size and delay_seconds
#' @noRd
#' @keywords internal
get_batch_config <- function() {
  # Check environment variables
  batch_size <- as.numeric(Sys.getenv("VGI_BATCH_SIZE", "10"))
  delay_seconds <- as.numeric(Sys.getenv("VGI_BATCH_DELAY", "1"))
  
  # Validate values
  if (is.na(batch_size) || batch_size < 1) {
    batch_size <- 10
  }
  if (is.na(delay_seconds) || delay_seconds < 0) {
    delay_seconds <- 1
  }
  
  # Cap maximum values for safety
  batch_size <- min(batch_size, 50)
  delay_seconds <- min(delay_seconds, 10)
  
  list(
    batch_size = batch_size,
    delay_seconds = delay_seconds
  )
}

#' Calculate Optimal Batch Size
#'
#' Dynamically calculate optimal batch size based on date range
#' and available metrics to balance performance and API load.
#'
#' @param date_range Number of dates to process
#' @param num_metrics Number of different metrics being fetched
#' @return Recommended batch size
#' @noRd
#' @keywords internal
calculate_optimal_batch_size <- function(date_range, num_metrics = 1) {
  total_calls <- date_range * num_metrics
  
  # Adjust batch size based on total API calls
  if (total_calls <= 50) {
    # Small request - no batching needed
    return(50)
  } else if (total_calls <= 200) {
    # Medium request - moderate batching
    return(20)
  } else if (total_calls <= 500) {
    # Large request - standard batching
    return(10)
  } else {
    # Very large request - aggressive batching
    return(5)
  }
}

#' Batch Fetch with Progress
#'
#' Enhanced batch processing with detailed progress reporting
#' and estimated time remaining.
#'
#' @param items Vector of items to process
#' @param fetch_fn Function to process each item
#' @param item_name Name of items for progress messages
#' @param batch_config Batch configuration (or NULL for defaults)
#' @param ... Additional arguments for fetch_fn
#' @return Combined results
#' @noRd
#' @keywords internal
batch_fetch_with_progress <- function(items,
                                    fetch_fn,
                                    item_name = "items",
                                    batch_config = NULL,
                                    ...) {
  
  if (length(items) == 0) {
    return(list())
  }
  
  # Get batch configuration
  if (is.null(batch_config)) {
    batch_config <- get_batch_config()
  }
  
  total_items <- length(items)
  results <- list()
  start_time <- Sys.time()
  
  message(sprintf("Processing %d %s in batches of %d...", 
                 total_items, item_name, batch_config$batch_size))
  
  for (i in seq_along(items)) {
    item <- items[i]
    
    # Fetch data
    tryCatch({
      result <- fetch_fn(item, ...)
      if (!is.null(result)) {
        results[[as.character(item)]] <- result
      }
    }, error = function(e) {
      warning(sprintf("Error processing %s: %s", item, e$message))
    })
    
    # Progress reporting
    if (i %% 10 == 0 || i == total_items) {
      elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      rate <- i / elapsed
      remaining <- (total_items - i) / rate
      
      message(sprintf("  Progress: %d/%d (%.1f%%) - Est. remaining: %.0f seconds",
                     i, total_items, (i/total_items)*100, remaining))
    }
    
    # Rate limiting
    if (i %% batch_config$batch_size == 0 && i < total_items) {
      Sys.sleep(batch_config$delay_seconds)
    }
  }
  
  total_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  message(sprintf("Completed in %.1f seconds (%.1f %s/second)", 
                 total_time, total_items/total_time, item_name))
  
  return(results)
}