# --- Utility Functions for videogameinsightsR ---

# This file contains helper functions used by the main data-fetching functions
# in the package. They handle tasks like input validation, query parameter
# preparation, and API request execution.

#' @importFrom rlang abort %||%
#' @importFrom stats setNames
#' @importFrom httr2 request req_user_agent req_url_path_append req_url_query
#'   req_error req_perform resp_status resp_body_raw resp_check_status
#'   resp_body_string req_headers req_auth_bearer_token req_body_json
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble
#' @importFrom utils URLencode head
#' @importFrom tidyr unnest
#' @importFrom dplyr rename all_of
#'

# --- API Configuration ---

# Define %||% operator if not available from rlang
if (!exists("%||%")) {
  `%||%` <- function(x, y) {
    if (is.null(x)) y else x
  }
}

# Base URL for the Video Game Insights API
get_base_url <- function() {
  # Config precedence: option > env var > default
  opt <- getOption("vgi.base_url", NULL)
  if (!is.null(opt) && nzchar(opt)) return(opt)
  env <- Sys.getenv("VGI_BASE_URL", "")
  if (nzchar(env)) return(env)
  "https://vginsights.com/api/v3"
}

# Build a consistent User-Agent string including package version
get_user_agent <- function() {
  pkg_ver <- tryCatch(as.character(utils::packageVersion("videogameinsightsR")), error = function(e) "0.0.0")
  sprintf("videogameinsightsR/%s", pkg_ver)
}

# --- Request-level Cache Helpers ---

.vgi_request_cache_dir <- function() {
  dir <- file.path(rappdirs::user_cache_dir("videogameinsightsR"), "request_cache")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  dir
}

.vgi_normalize_params <- function(params) {
  if (is.null(params) || length(params) == 0) return(list())
  # Sort by name to ensure stable keys
  params[order(names(params))]
}

.vgi_cache_key <- function(method, endpoint, query_params) {
  normalized <- .vgi_normalize_params(query_params)
  payload <- list(method = toupper(method %||% "GET"), endpoint = endpoint, params = normalized)
  # Use digest to create a short, filesystem-safe key
  json <- jsonlite::toJSON(payload, auto_unbox = TRUE)
  paste0("v1_", digest::digest(json, algo = "md5"))
}

.vgi_cache_get <- function(key, ttl_seconds) {
  if (is.null(ttl_seconds) || is.na(ttl_seconds) || ttl_seconds <= 0) return(NULL)
  path <- file.path(.vgi_request_cache_dir(), paste0(key, ".rds"))
  if (!file.exists(path)) return(NULL)
  age <- as.numeric(difftime(Sys.time(), file.info(path)$mtime, units = "secs"))
  if (age > ttl_seconds) return(NULL)
  # Return parsed content (list/data.frame)
  out <- tryCatch(readRDS(path), error = function(e) NULL)
  out
}

.vgi_cache_set <- function(key, value) {
  path <- file.path(.vgi_request_cache_dir(), paste0(key, ".rds"))
  tryCatch(saveRDS(value, path), error = function(e) invisible(NULL))
  invisible(TRUE)
}

# --- Global Rate Limiter Integration ---

.vgi_get_global_limiter <- function() {
  # Allow users to disable auto rate limiting
  auto <- getOption("vgi.auto_rate_limit", TRUE)
  if (!isTRUE(auto)) return(NULL)
  if (!exists(".vgi_global_limiter", envir = .GlobalEnv, inherits = FALSE)) {
    # Read defaults from env/options
    calls <- getOption("vgi.calls_per_batch", as.numeric(Sys.getenv("VGI_BATCH_SIZE", "10")))
    delay <- getOption("vgi.batch_delay", as.numeric(Sys.getenv("VGI_BATCH_DELAY", "1")))
    limiter <- create_rate_limiter(calls_per_batch = calls, delay_seconds = delay, show_messages = isTRUE(getOption("vgi.verbose", FALSE)))
    assign(".vgi_global_limiter", limiter, envir = .GlobalEnv)
  }
  get(".vgi_global_limiter", envir = .GlobalEnv, inherits = FALSE)
}

# Get authentication token
get_auth_token <- function(auth_token = NULL) {
  if (!is.null(auth_token) && auth_token != "") {
    return(auth_token)
  }
  
  token <- Sys.getenv("VGI_AUTH_TOKEN")
  if (is.null(token) || token == "") {
    stop(
      "Authentication token is required. ",
      "Set VGI_AUTH_TOKEN environment variable or pass auth_token parameter."
    )
  }
  return(token)
}

# --- HTTP Request Handling ---

# Make authenticated API request
make_api_request <- function(endpoint, 
                           query_params = list(), 
                           auth_token = NULL,
                           method = "GET",
                           headers = list()) {
  
  token <- get_auth_token(auth_token)
  base_url <- get_base_url()
  
  # Build request with api-key header and custom headers
  req <- httr2::request(base_url) |>
    httr2::req_url_path_append(endpoint) |>
    httr2::req_headers("api-key" = token, "Accept" = "application/json", !!!headers) |>
    httr2::req_user_agent(get_user_agent()) |>
    httr2::req_timeout(as.numeric(getOption("vgi.timeout", 30))) |>
    httr2::req_retry(
      max_tries = as.integer(getOption("vgi.retry_max_tries", 4)),
      backoff = function(attempt) min(60, 2^(attempt - 1)),
      is_transient = function(resp) {
        status <- httr2::resp_status(resp)
        status == 408 || status == 429 || (status >= 500 && status <= 599)
      }
    )
  
  # Add query parameters if provided
  if (length(query_params) > 0) {
    # Remove NULL values
    query_params <- query_params[!sapply(query_params, is.null)]
    req <- req |> httr2::req_url_query(!!!query_params)
  }
  
  # Request-level caching (GET only)
  ttl <- getOption("vgi.request_cache_ttl", as.numeric(Sys.getenv("VGI_REQUEST_CACHE_TTL_SECONDS", "0")))
  cache_key <- .vgi_cache_key(method, endpoint, query_params)
  if (toupper(method) == "GET") {
    cached <- .vgi_cache_get(cache_key, ttl)
    if (!is.null(cached)) {
      if (isTRUE(getOption("vgi.verbose", FALSE))) message(sprintf("[cache hit] GET %s", endpoint))
      return(cached)
    }
  }

  # Optional integrated rate limiting
  limiter <- .vgi_get_global_limiter()
  if (!is.null(limiter)) limiter$increment()

  # Perform request with error handling
  if (isTRUE(getOption("vgi.verbose", FALSE))) {
    message(sprintf("[request] %s %s", toupper(method), endpoint))
  }
  start_time <- Sys.time()
  resp <- req |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform()
  
  # Check for errors
  if (httr2::resp_status(resp) >= 400) {
    error_body <- tryCatch(
      httr2::resp_body_string(resp),
      error = function(e) "Unable to parse error response"
    )
    
    stop(sprintf(
      "API request failed [%s]: %s",
      httr2::resp_status(resp),
      error_body
    ))
  }
  
  # Parse JSON response
  content_text <- httr2::resp_body_string(resp)
  content_list <- jsonlite::fromJSON(content_text, flatten = TRUE)

  # Store in request cache (GET only)
  if (toupper(method) == "GET" && ttl > 0) {
    .vgi_cache_set(cache_key, content_list)
  }

  if (isTRUE(getOption("vgi.verbose", FALSE))) {
    dur <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    message(sprintf("[response] %s %s in %.2fs", toupper(method), endpoint, dur))
  }

  return(content_list)
}

# Make authenticated API POST request
make_api_request_post <- function(endpoint,
                                 body = list(),
                                 auth_token = NULL,
                                 headers = list()) {
  
  token <- get_auth_token(auth_token)
  base_url <- get_base_url()
  
  # Build request with api-key header and custom headers
  req <- httr2::request(base_url) |>
    httr2::req_url_path_append(endpoint) |>
    httr2::req_headers("api-key" = token, "Accept" = "application/json", !!!headers) |>
    httr2::req_user_agent(get_user_agent()) |>
    httr2::req_timeout(as.numeric(getOption("vgi.timeout", 30))) |>
    httr2::req_retry(
      max_tries = as.integer(getOption("vgi.retry_max_tries", 4)),
      backoff = function(attempt) min(60, 2^(attempt - 1)),
      is_transient = function(resp) {
        status <- httr2::resp_status(resp)
        status == 408 || status == 429 || (status >= 500 && status <= 599)
      }
    ) |>
    httr2::req_body_json(body)
  
  # Optional integrated rate limiting
  limiter <- .vgi_get_global_limiter()
  if (!is.null(limiter)) limiter$increment()

  # Perform request with error handling
  if (isTRUE(getOption("vgi.verbose", FALSE))) {
    message(sprintf("[request] POST %s", endpoint))
  }
  start_time <- Sys.time()
  resp <- req |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform()
  
  # Check for errors
  if (httr2::resp_status(resp) >= 400) {
    error_body <- tryCatch(
      httr2::resp_body_string(resp),
      error = function(e) "Unable to parse error response"
    )
    
    stop(sprintf(
      "API request failed [%s]: %s",
      httr2::resp_status(resp),
      error_body
    ))
  }
  
  # Parse JSON response
  content_text <- httr2::resp_body_string(resp)
  content_list <- jsonlite::fromJSON(content_text, flatten = TRUE)

  if (isTRUE(getOption("vgi.verbose", FALSE))) {
    dur <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    message(sprintf("[response] POST %s in %.2fs", endpoint, dur))
  }

  return(content_list)
}

# --- Input Validation ---

# Validate platform parameter
validate_platform <- function(platform) {
  valid_platforms <- c("steam", "playstation", "xbox", "nintendo", "all")
  if (!platform %in% valid_platforms) {
    stop(sprintf(
      "Invalid platform '%s'. Must be one of: %s",
      platform,
      paste(valid_platforms, collapse = ", ")
    ))
  }
}

# Validate date format
validate_date <- function(date, param_name = "date") {
  if (is.null(date)) return(NULL)
  
  # Convert to Date if string
  if (is.character(date)) {
    date <- tryCatch(
      as.Date(date),
      error = function(e) {
        stop(sprintf("%s must be a Date object or valid date string", param_name))
      }
    )
  }
  
  if (!inherits(date, "Date")) {
    stop(sprintf("%s must be a Date object or valid date string", param_name))
  }
  
  return(as.character(date))
}

# Format date for API calls
format_date <- function(date) {
  if (is.null(date)) {
    stop("Date parameter is required")
  }
  
  # If already a character, try to parse it
  if (is.character(date)) {
    date <- as.Date(date)
  }
  
  # If it's a Date object, format it
  if (inherits(date, "Date")) {
    return(format(date, "%Y-%m-%d"))
  }
  
  stop("Date must be a Date object or valid date string in YYYY-MM-DD format")
}

# Validate numeric parameters
validate_numeric <- function(value, param_name, min_val = NULL, max_val = NULL) {
  if (!is.numeric(value)) {
    stop(sprintf("%s must be numeric", param_name))
  }
  
  if (!is.null(min_val) && value < min_val) {
    stop(sprintf("%s must be at least %s", param_name, min_val))
  }
  
  if (!is.null(max_val) && value > max_val) {
    stop(sprintf("%s must be at most %s", param_name, max_val))
  }
}

# --- Data Processing ---

# Convert API response to tibble
process_api_response <- function(response_data, expected_fields = NULL) {
  if (is.null(response_data) || length(response_data) == 0) {
    return(tibble::tibble())
  }
  
  # Handle different response structures
  if (is.data.frame(response_data)) {
    result <- tibble::as_tibble(response_data)
  } else if (is.list(response_data) && !is.null(response_data$data)) {
    result <- tibble::as_tibble(response_data$data)
  } else if (is.list(response_data)) {
    result <- tibble::as_tibble(response_data)
  } else {
    stop("Unexpected API response format")
  }
  
  # Ensure expected fields exist
  if (!is.null(expected_fields)) {
    missing_fields <- setdiff(expected_fields, names(result))
    for (field in missing_fields) {
      result[[field]] <- NA
    }
  }
  
  return(result)
}

# --- Freshness warnings ---

warn_if_stale_ids <- function(steam_app_ids) {
  if (length(steam_app_ids) == 0 || all(is.na(steam_app_ids))) return(invisible(NULL))
  # Heuristic: very low IDs imply very old games
  if (all(steam_app_ids < 1000, na.rm = TRUE)) {
    warning("API returned only old games (Steam IDs < 1000). This may indicate stale data.")
  }
  invisible(NULL)
}