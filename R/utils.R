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

# Base URL for the Video Game Insights API
get_base_url <- function() {
  "https://vginsights.com/api/v3"
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
    httr2::req_headers("api-key" = token, !!!headers) |>
    httr2::req_user_agent("videogameinsightsR")
  
  # Add query parameters if provided
  if (length(query_params) > 0) {
    # Remove NULL values
    query_params <- query_params[!sapply(query_params, is.null)]
    req <- req |> httr2::req_url_query(!!!query_params)
  }
  
  # Perform request with error handling
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
    httr2::req_headers("api-key" = token, !!!headers) |>
    httr2::req_user_agent("videogameinsightsR") |>
    httr2::req_body_json(body)
  
  # Perform request with error handling
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