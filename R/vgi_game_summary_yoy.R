#' Get Year-over-Year Game Summary Comparison
#'
#' Compare game metrics across multiple years for the same time period.
#' Supports flexible date specification using months or explicit dates.
#'
#' @param steam_app_ids Numeric vector. Steam App IDs to analyze.
#' @param years Numeric vector. Years to compare (e.g., c(2023, 2024, 2025)).
#' @param start_month Character string. Start month name or abbreviation 
#'   (e.g., "January", "Jan", "1"). Ignored if start_date is provided.
#' @param end_month Character string. End month name or abbreviation.
#'   Defaults to start_month if not provided. Ignored if end_date is provided.
#' @param start_date Character string or Date. Explicit start date in "MM-DD" or
#'   "YYYY-MM-DD" format. Takes precedence over start_month.
#' @param end_date Character string or Date. Explicit end date. Takes precedence
#'   over end_month.
#' @param metrics Character vector. Which metrics to retrieve. Defaults to
#'   c("concurrent", "revenue", "units").
#' @param include_growth Logical. Whether to calculate year-over-year growth
#'   percentages. Defaults to TRUE.
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param batch_size Integer. Number of API calls per batch before pausing.
#'   Defaults to automatic calculation based on date range.
#' @param batch_delay Numeric. Seconds to pause between batches.
#'   Defaults to value from VGI_BATCH_DELAY environment variable or 1 second.
#'
#' @return A list containing:
#' \describe{
#'   \item{comparison_table}{Summary table with metrics for each year and YoY growth}
#'   \item{yearly_summaries}{List of detailed summaries by year}
#'   \item{time_series_comparison}{Combined time series data with year labels}
#'   \item{period}{Description of the comparison period}
#'   \item{api_calls}{Total number of API calls made}
#' }
#'
#' @details
#' The function supports two ways to specify the comparison period:
#' 
#' 1. **Month-based**: Specify start_month and optionally end_month. The function
#'    will use these months for each year in the years parameter.
#' 
#' 2. **Date-based**: Specify start_date and end_date with explicit month/day
#'    (e.g., "03-15" for March 15). These will be applied to each year.
#'
#' Year-over-year growth is calculated as: ((current - previous) / previous) * 100
#'
#' @export
#' @examples
#' \dontrun{
#' # Compare Q1 performance across years
#' q1_comparison <- vgi_game_summary_yoy(
#'   steam_app_ids = c(892970, 1145360),
#'   years = c(2023, 2024, 2025),
#'   start_month = "Jan",
#'   end_month = "Mar"
#' )
#' 
#' # Compare specific date ranges
#' holiday_comparison <- vgi_game_summary_yoy(
#'   steam_app_ids = 892970,
#'   years = c(2022, 2023, 2024),
#'   start_date = "11-15",  # Nov 15
#'   end_date = "01-15"     # Jan 15 (crosses year boundary)
#' )
#' 
#' # Compare full years
#' yearly_comparison <- vgi_game_summary_yoy(
#'   steam_app_ids = c(1517290, 1238810),
#'   years = c(2023, 2024),
#'   start_month = "January",
#'   end_month = "December"
#' )
#' }
vgi_game_summary_yoy <- function(steam_app_ids,
                                years,
                                start_month = NULL,
                                end_month = NULL,
                                start_date = NULL,
                                end_date = NULL,
                                metrics = c("concurrent", "revenue", "units"),
                                include_growth = TRUE,
                                auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                                batch_size = NULL,
                                batch_delay = NULL) {
  
  # Validate inputs
  if (missing(steam_app_ids) || length(steam_app_ids) == 0) {
    stop("steam_app_ids is required")
  }
  
  if (missing(years) || length(years) == 0) {
    stop("years parameter is required")
  }
  
  # Sort years for proper comparison
  years <- sort(unique(years))
  
  # Determine the period for each year
  period_dates <- determine_yoy_period(
    years = years,
    start_month = start_month,
    end_month = end_month,
    start_date = start_date,
    end_date = end_date
  )
  
  # Initialize results
  yearly_summaries <- list()
  total_api_calls <- 0
  
  # Fetch data for each year
  cat("Fetching data for year-over-year comparison...\n")
  for (i in seq_along(years)) {
    year <- years[i]
    dates <- period_dates[[as.character(year)]]
    
    cat(sprintf("  Year %d: %s to %s\n", year, dates$start, dates$end))
    
    # Get summary for this year's period
    year_summary <- vgi_game_summary(
      steam_app_ids = steam_app_ids,
      start_date = dates$start,
      end_date = dates$end,
      metrics = metrics,
      auth_token = auth_token,
      batch_size = batch_size,
      batch_delay = batch_delay
    )
    
    # Store the summary with year label
    year_summary$year <- year
    year_summary$period_start <- dates$start
    year_summary$period_end <- dates$end
    
    yearly_summaries[[as.character(year)]] <- year_summary
    total_api_calls <- total_api_calls + year_summary$api_calls
  }
  
  # Build comparison table
  comparison_table <- build_yoy_comparison_table(
    yearly_summaries = yearly_summaries,
    include_growth = include_growth
  )
  
  # Combine time series data for visualization
  time_series_comparison <- combine_yoy_time_series(yearly_summaries)
  
  # Create period description
  period_desc <- describe_yoy_period(period_dates)
  
  # Return results
  result <- list(
    comparison_table = comparison_table,
    yearly_summaries = yearly_summaries,
    time_series_comparison = time_series_comparison,
    period = period_desc,
    years = years,
    api_calls = total_api_calls
  )
  
  class(result) <- c("vgi_yoy_comparison", "list")
  return(result)
}

#' Determine Year-over-Year Period Dates
#'
#' Internal function to calculate start and end dates for each year
#' based on the provided parameters.
#'
#' @noRd
#' @noRd
#' @keywords internal
determine_yoy_period <- function(years, start_month, end_month, start_date, end_date) {
  
  # If explicit dates are provided, use them
  if (!is.null(start_date) && !is.null(end_date)) {
    return(parse_explicit_dates(years, start_date, end_date))
  }
  
  # If months are provided, use them
  if (!is.null(start_month)) {
    if (is.null(end_month)) {
      end_month <- start_month
    }
    return(parse_month_dates(years, start_month, end_month))
  }
  
  # If nothing provided, default to full year
  warning("No date parameters provided. Defaulting to full year comparison.")
  return(parse_month_dates(years, "January", "December"))
}

#' Parse Explicit Dates for Each Year
#'
#' @noRd
#' @keywords internal
parse_explicit_dates <- function(years, start_date, end_date) {
  period_dates <- list()
  
  # Parse the date components
  start_parts <- parse_date_string(start_date)
  end_parts <- parse_date_string(end_date)
  
  for (year in years) {
    # Handle year boundary crossing
    start_year <- year
    end_year <- year
    
    # If end month is before start month, it crosses into next year
    if (end_parts$month < start_parts$month) {
      end_year <- year + 1
    }
    
    period_dates[[as.character(year)]] <- list(
      start = sprintf("%04d-%02d-%02d", start_year, start_parts$month, start_parts$day),
      end = sprintf("%04d-%02d-%02d", end_year, end_parts$month, end_parts$day)
    )
  }
  
  return(period_dates)
}

#' Parse Month-Based Dates for Each Year
#'
#' @noRd
#' @keywords internal
parse_month_dates <- function(years, start_month, end_month) {
  period_dates <- list()
  
  # Convert month names to numbers
  start_month_num <- month_to_number(start_month)
  end_month_num <- month_to_number(end_month)
  
  for (year in years) {
    # Handle year boundary crossing
    start_year <- year
    end_year <- year
    
    if (end_month_num < start_month_num) {
      end_year <- year + 1
    }
    
    # Get last day of end month
    end_day <- get_last_day_of_month(end_year, end_month_num)
    
    period_dates[[as.character(year)]] <- list(
      start = sprintf("%04d-%02d-01", start_year, start_month_num),
      end = sprintf("%04d-%02d-%02d", end_year, end_month_num, end_day)
    )
  }
  
  return(period_dates)
}

#' Convert Month Name/Number to Number
#'
#' @noRd
#' @keywords internal
month_to_number <- function(month) {
  if (is.numeric(month)) {
    return(as.integer(month))
  }
  
  # Try to match month name
  month_names <- c("january", "february", "march", "april", "may", "june",
                   "july", "august", "september", "october", "november", "december")
  month_abbr <- c("jan", "feb", "mar", "apr", "may", "jun",
                  "jul", "aug", "sep", "oct", "nov", "dec")
  
  month_lower <- tolower(trimws(month))
  
  # Check full names
  idx <- match(month_lower, month_names)
  if (!is.na(idx)) return(idx)
  
  # Check abbreviations
  idx <- match(month_lower, month_abbr)
  if (!is.na(idx)) return(idx)
  
  stop(sprintf("Invalid month: '%s'", month))
}

#' Get Last Day of Month
#'
#' @noRd
#' @keywords internal
get_last_day_of_month <- function(year, month) {
  # Handle February and leap years
  if (month == 2) {
    if ((year %% 4 == 0 && year %% 100 != 0) || (year %% 400 == 0)) {
      return(29)
    } else {
      return(28)
    }
  }
  
  # Days in each month
  days_in_month <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  return(days_in_month[month])
}

#' Parse Date String
#'
#' Parse dates in "MM-DD" or "YYYY-MM-DD" format
#'
#' @noRd
#' @keywords internal
parse_date_string <- function(date_str) {
  parts <- strsplit(date_str, "-")[[1]]
  
  if (length(parts) == 2) {
    # MM-DD format
    return(list(month = as.integer(parts[1]), day = as.integer(parts[2])))
  } else if (length(parts) == 3) {
    # YYYY-MM-DD format (ignore year)
    return(list(month = as.integer(parts[2]), day = as.integer(parts[3])))
  } else {
    stop(sprintf("Invalid date format: '%s'. Use MM-DD or YYYY-MM-DD", date_str))
  }
}

#' Build Year-over-Year Comparison Table
#'
#' @noRd
#' @keywords internal
build_yoy_comparison_table <- function(yearly_summaries, include_growth = TRUE) {
  # Extract summary tables from each year
  tables <- lapply(names(yearly_summaries), function(year) {
    summary <- yearly_summaries[[year]]
    if (!is.null(summary$summary_table)) {
      table <- summary$summary_table
      table$year <- as.integer(year)
      return(table)
    }
    return(NULL)
  })
  
  # Remove NULL entries
  tables <- tables[!sapply(tables, is.null)]
  
  if (length(tables) == 0) {
    return(data.frame())
  }
  
  # Combine all years
  combined <- do.call(rbind, tables)
  
  # Pivot to wide format for comparison
  metrics_cols <- c("avg_peak_ccu", "avg_avg_ccu", "total_revenue", 
                    "avg_daily_revenue", "total_units", "avg_daily_units")
  
  # Keep only available metrics
  available_metrics <- intersect(names(combined), metrics_cols)
  
  # Create comparison table
  comparison <- combined %>%
    select(steamAppId, name, year, all_of(available_metrics)) %>%
    arrange(steamAppId, year)
  
  # Calculate year-over-year growth if requested
  if (include_growth && length(unique(comparison$year)) > 1) {
    comparison <- calculate_yoy_growth(comparison, available_metrics)
  }
  
  return(comparison)
}

#' Calculate Year-over-Year Growth
#'
#' @noRd
#' @keywords internal
calculate_yoy_growth <- function(data, metric_cols) {
  # For each game and metric, calculate YoY growth
  data <- data %>%
    arrange(steamAppId, year) %>%
    group_by(steamAppId)
  
  # Add growth columns
  for (metric in metric_cols) {
    growth_col <- paste0(metric, "_yoy_growth")
    
    data <- data %>%
      mutate(!!growth_col := {
        current <- .data[[metric]]
        previous <- lag(.data[[metric]])
        ifelse(is.na(previous) | previous == 0, NA_real_, 
               round((current - previous) / previous * 100, 1))
      })
  }
  
  data <- ungroup(data)
  return(data)
}

#' Combine Year-over-Year Time Series Data
#'
#' @noRd
#' @keywords internal
combine_yoy_time_series <- function(yearly_summaries) {
  time_series <- list()
  
  # Process each metric type
  metric_types <- c("concurrent", "active", "revenue", "units")
  
  for (metric in metric_types) {
    combined_data <- list()
    
    for (year_str in names(yearly_summaries)) {
      summary <- yearly_summaries[[year_str]]
      
      if (!is.null(summary$time_series[[metric]])) {
        ts_data <- summary$time_series[[metric]]
        ts_data$year <- as.integer(year_str)
        
        # Normalize date to same year for comparison
        ts_data$normalized_date <- normalize_date_to_year(ts_data$date, 2000)
        
        combined_data[[year_str]] <- ts_data
      }
    }
    
    if (length(combined_data) > 0) {
      time_series[[metric]] <- do.call(rbind, combined_data)
    }
  }
  
  return(time_series)
}

#' Normalize Dates to a Common Year
#'
#' For year-over-year visualization, normalize all dates to the same year
#' while preserving month and day.
#'
#' @noRd
#' @keywords internal
normalize_date_to_year <- function(dates, target_year = 2000) {
  date_objs <- as.Date(dates)
  
  # Extract month and day
  months <- format(date_objs, "%m")
  days <- format(date_objs, "%d")
  
  # Reconstruct with target year
  normalized <- as.Date(paste(target_year, months, days, sep = "-"))
  
  return(normalized)
}

#' Describe Year-over-Year Period
#'
#' @noRd
#' @keywords internal
describe_yoy_period <- function(period_dates) {
  if (length(period_dates) == 0) {
    return("No period defined")
  }
  
  # Get the first year's period as reference
  first_period <- period_dates[[1]]
  start_date <- as.Date(first_period$start)
  end_date <- as.Date(first_period$end)
  
  # Format the period description
  if (format(start_date, "%m-%d") == "01-01" && 
      format(end_date, "%m-%d") == "12-31") {
    return("Full year comparison")
  }
  
  # Check if it crosses year boundary
  if (as.numeric(format(end_date, "%Y")) > as.numeric(format(start_date, "%Y"))) {
    return(sprintf("%s to %s (crosses year boundary)",
                   format(start_date, "%b %d"),
                   format(end_date, "%b %d")))
  }
  
  return(sprintf("%s to %s",
                 format(start_date, "%b %d"),
                 format(end_date, "%b %d")))
}

#' Print Method for Year-over-Year Comparison
#'
#' @export
print.vgi_yoy_comparison <- function(x, ...) {
  cat("Video Game Insights Year-over-Year Comparison\n")
  cat("=============================================\n")
  cat("Period:", x$period, "\n")
  cat("Years compared:", paste(x$years, collapse = ", "), "\n")
  cat("Games analyzed:", length(unique(x$comparison_table$steamAppId)), "\n")
  cat("Total API calls:", x$api_calls, "\n")
  cat("\nUse $comparison_table to see the summary table\n")
  cat("Use $time_series_comparison to see time series data\n")
}