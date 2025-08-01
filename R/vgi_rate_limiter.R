#' Simple Rate Limiter for VGI API Calls
#'
#' Provides a simple counter-based rate limiter that can be used
#' in existing loops to reduce API stress.
#'
#' @name rate_limiter
#' @keywords internal
NULL

#' Create a Rate Limiter
#'
#' Creates a rate limiter object that tracks API calls and applies
#' delays when thresholds are reached.
#'
#' @param calls_per_batch Number of calls before applying delay
#' @param delay_seconds Seconds to pause between batches
#' @param show_messages Whether to show rate limiting messages
#' @return A rate limiter environment with increment() method
#' @export
#' @examples
#' \dontrun{
#' # Create a rate limiter
#' limiter <- create_rate_limiter(calls_per_batch = 10, delay_seconds = 1)
#' 
#' # Use in a loop
#' for (i in 1:50) {
#'   # Make API call
#'   result <- some_api_function()
#'   
#'   # Apply rate limiting
#'   limiter$increment()
#' }
#' }
create_rate_limiter <- function(calls_per_batch = 10, 
                              delay_seconds = 1,
                              show_messages = TRUE) {
  
  # Get from environment if not specified
  if (is.null(calls_per_batch)) {
    calls_per_batch <- as.numeric(Sys.getenv("VGI_BATCH_SIZE", "10"))
  }
  if (is.null(delay_seconds)) {
    delay_seconds <- as.numeric(Sys.getenv("VGI_BATCH_DELAY", "1"))
  }
  
  # Create environment to store state
  limiter <- new.env(parent = emptyenv())
  limiter$call_count <- 0
  limiter$total_calls <- 0
  limiter$calls_per_batch <- calls_per_batch
  limiter$delay_seconds <- delay_seconds
  limiter$show_messages <- show_messages
  limiter$start_time <- Sys.time()
  
  # Increment method
  limiter$increment <- function() {
    limiter$call_count <- limiter$call_count + 1
    limiter$total_calls <- limiter$total_calls + 1
    
    # Check if we need to apply delay
    if (limiter$call_count >= limiter$calls_per_batch && 
        limiter$delay_seconds > 0) {
      
      if (limiter$show_messages) {
        message(sprintf("  Rate limiting: pausing %g seconds after %d API calls...", 
                       limiter$delay_seconds, limiter$calls_per_batch))
      }
      
      Sys.sleep(limiter$delay_seconds)
      limiter$call_count <- 0  # Reset batch counter
    }
  }
  
  # Get stats method
  limiter$get_stats <- function() {
    elapsed <- as.numeric(difftime(Sys.time(), limiter$start_time, units = "secs"))
    list(
      total_calls = limiter$total_calls,
      elapsed_seconds = elapsed,
      calls_per_second = limiter$total_calls / elapsed
    )
  }
  
  # Reset method
  limiter$reset <- function() {
    limiter$call_count <- 0
    limiter$total_calls <- 0
    limiter$start_time <- Sys.time()
  }
  
  class(limiter) <- c("vgi_rate_limiter", "environment")
  return(limiter)
}

#' Apply Rate Limiting to Date Sequence
#'
#' Simple function to add rate limiting to date-based loops
#' 
#' @param date_position Current position in date sequence
#' @param total_dates Total number of dates
#' @param calls_per_batch Calls per batch (default from environment)
#' @param delay_seconds Delay between batches (default from environment)
#' @param show_messages Whether to show messages
#' @export
apply_rate_limit <- function(date_position, 
                           total_dates,
                           calls_per_batch = NULL,
                           delay_seconds = NULL,
                           show_messages = TRUE) {
  
  # Get defaults from environment
  if (is.null(calls_per_batch)) {
    calls_per_batch <- as.numeric(Sys.getenv("VGI_BATCH_SIZE", "10"))
  }
  if (is.null(delay_seconds)) {
    delay_seconds <- as.numeric(Sys.getenv("VGI_BATCH_DELAY", "1"))
  }
  
  # Apply delay if we've hit the batch size
  if (date_position %% calls_per_batch == 0 && 
      date_position < total_dates && 
      delay_seconds > 0) {
    
    if (show_messages) {
      message(sprintf("  Rate limiting: pausing %g seconds...", delay_seconds))
    }
    
    Sys.sleep(delay_seconds)
  }
}

#' Check if Rate Limiting Should Be Applied
#'
#' Determines if rate limiting should be applied based on the
#' expected number of API calls.
#'
#' @param num_dates Number of dates to process
#' @param num_metrics Number of metrics being fetched
#' @return Logical indicating if rate limiting is recommended
#' @export
should_use_rate_limiting <- function(num_dates, num_metrics = 1) {
  # Check environment variable
  force_batching <- Sys.getenv("VGI_FORCE_BATCHING", "")
  if (force_batching == "TRUE") return(TRUE)
  if (force_batching == "FALSE") return(FALSE)
  
  # Calculate total expected calls
  total_calls <- num_dates * num_metrics
  
  # Recommend rate limiting for larger requests
  return(total_calls > 50)
}