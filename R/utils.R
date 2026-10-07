# --- Utility Functions for VideoGameInsightsR ---

# This file contains helper functions used by the main data-fetching functions
# in the package. They handle tasks like input validation, query parameter
# preparation, and API request execution.

#' @importFrom rlang abort %||% :=
#' @importFrom rvest read_html html_attr html_node
#' @importFrom dplyr select arrange group_by mutate ungroup all_of
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom stats setNames lag
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

# --- API version handling ---

.vgi_default_api_root <- "https://vginsights.com/api"

#' Normalise an API version argument to "v3" or "v4".
#' @noRd
.vgi_api_version <- function(version = NULL) {
  if (is.null(version)) version <- getOption("vgi.api_version", "v3")
  version <- tolower(as.character(version[[1]]))
  if (grepl("^[0-9]+$", version)) version <- paste0("v", version)
  if (!version %in% c("v3", "v4")) {
    stop(sprintf("Unsupported VGI API version '%s'. Use \"v3\" or \"v4\".", version))
  }
  version
}

#' Root of the VGI API (without the version segment).
#'
#' Config precedence: option `vgi.base_url` > env var `VGI_BASE_URL` > default.
#' A configured value that already ends in `/v3` or `/v4` (the pre-0.2.0
#' convention) is accepted and the version suffix is stripped.
#' @noRd
get_api_root <- function() {
  opt <- getOption("vgi.base_url", NULL)
  root <- if (!is.null(opt) && nzchar(opt)) {
    opt
  } else {
    env <- Sys.getenv("VGI_BASE_URL", "")
    if (nzchar(env)) env else .vgi_default_api_root
  }
  root <- sub("/+$", "", root)
  sub("/v[0-9]+$", "", root)
}

# Base URL for one version of the Video Game Insights API
get_base_url <- function(version = NULL) {
  paste0(get_api_root(), "/", .vgi_api_version(version))
}

# Build a consistent User-Agent string including package version
get_user_agent <- function() {
  pkg_ver <- tryCatch(as.character(utils::packageVersion("VideoGameInsightsR")), error = function(e) "0.0.0")
  sprintf("VideoGameInsightsR/%s", pkg_ver)
}

# --- Request-level Cache Helpers ---

.vgi_request_cache_dir <- function() {
  dir <- file.path(tools::R_user_dir("VideoGameInsightsR", "cache"), "request_cache")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  dir
}

.vgi_normalize_params <- function(params) {
  if (is.null(params) || length(params) == 0) return(list())
  # Sort by name to ensure stable keys
  params[order(names(params))]
}

.vgi_cache_key <- function(method, endpoint, query_params, version = "v3") {
  normalized <- .vgi_normalize_params(query_params)
  payload <- list(method = toupper(method %||% "GET"), version = version,
                  endpoint = endpoint, params = normalized)
  # Use digest to create a short, filesystem-safe key
  json <- jsonlite::toJSON(payload, auto_unbox = TRUE)
  paste0("v2_", digest::digest(json, algo = "md5"))
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

.vgi_env <- new.env(parent = emptyenv())

.vgi_get_global_limiter <- function() {
  auto <- getOption("vgi.auto_rate_limit", TRUE)
  if (!isTRUE(auto)) return(NULL)
  if (!exists(".vgi_global_limiter", envir = .vgi_env, inherits = FALSE)) {
    calls <- getOption("vgi.calls_per_batch", as.numeric(Sys.getenv("VGI_BATCH_SIZE", "10")))
    delay <- getOption("vgi.batch_delay", as.numeric(Sys.getenv("VGI_BATCH_DELAY", "1")))
    limiter <- create_rate_limiter(calls_per_batch = calls, delay_seconds = delay, show_messages = isTRUE(getOption("vgi.verbose", FALSE)))
    assign(".vgi_global_limiter", limiter, envir = .vgi_env)
  }
  get(".vgi_global_limiter", envir = .vgi_env, inherits = FALSE)
}

# Get authentication token
get_auth_token <- function(auth_token = NULL) {
  # If explicitly provided, empty string is treated as missing and errors
  if (!is.null(auth_token)) {
    if (nzchar(auth_token)) return(auth_token)
    stop(
      "Authentication token is required. ",
      "Set VGI_AUTH_TOKEN environment variable or pass auth_token parameter."
    )
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

# Build an authenticated httr2 request for one API version
.vgi_build_request <- function(endpoint, token, headers = list(), version = "v3") {
  httr2::request(get_base_url(version)) |>
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
}

# Perform a request, raise a readable error on HTTP >= 400, parse JSON
.vgi_perform <- function(req, label) {
  if (isTRUE(getOption("vgi.verbose", FALSE))) message(sprintf("[request] %s", label))
  start_time <- Sys.time()
  resp <- req |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform()

  if (httr2::resp_status(resp) >= 400) {
    error_body <- tryCatch(
      httr2::resp_body_string(resp),
      error = function(e) "Unable to parse error response"
    )
    rlang::abort(
      sprintf("API request failed [%s]: %s", httr2::resp_status(resp), error_body),
      class = "vgi_http_error",
      status = httr2::resp_status(resp),
      body = error_body
    )
  }

  content_text <- httr2::resp_body_string(resp)
  content_list <- jsonlite::fromJSON(content_text, flatten = TRUE)

  if (isTRUE(getOption("vgi.verbose", FALSE))) {
    dur <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    message(sprintf("[response] %s in %.2fs", label, dur))
  }
  content_list
}

# Make authenticated API request
#
# `version` selects the API generation: "v3" (per-Steam-game endpoints,
# offset/limit pagination) or "v4" (multi-platform endpoints, cursor
# pagination, `{nextCursor, results}` envelopes).
make_api_request <- function(endpoint,
                           query_params = list(),
                           auth_token = NULL,
                           method = "GET",
                           headers = list(),
                           version = "v3") {

  token <- get_auth_token(auth_token)
  version <- .vgi_api_version(version)
  req <- .vgi_build_request(endpoint, token, headers, version)

  # Add query parameters if provided
  if (length(query_params) > 0) {
    # Remove NULL values
    query_params <- query_params[!vapply(query_params, is.null, logical(1))]
    if (length(query_params) > 0) req <- req |> httr2::req_url_query(!!!query_params)
  }

  # Request-level caching (GET only)
  ttl <- getOption("vgi.request_cache_ttl", as.numeric(Sys.getenv("VGI_REQUEST_CACHE_TTL_SECONDS", "0")))
  cache_key <- .vgi_cache_key(method, endpoint, query_params, version)
  if (toupper(method) == "GET") {
    cached <- .vgi_cache_get(cache_key, ttl)
    if (!is.null(cached)) {
      if (isTRUE(getOption("vgi.verbose", FALSE))) message(sprintf("[cache hit] GET %s/%s", version, endpoint))
      return(cached)
    }
  }

  # Optional integrated rate limiting
  limiter <- .vgi_get_global_limiter()
  if (!is.null(limiter)) limiter$increment()

  content_list <- .vgi_perform(req, sprintf("%s %s/%s", toupper(method), version, endpoint))

  # Store in request cache (GET only)
  if (toupper(method) == "GET" && ttl > 0) {
    .vgi_cache_set(cache_key, content_list)
  }

  content_list
}

# Make authenticated API POST request
make_api_request_post <- function(endpoint,
                                 body = list(),
                                 auth_token = NULL,
                                 headers = list(),
                                 version = "v3") {

  token <- get_auth_token(auth_token)
  version <- .vgi_api_version(version)
  req <- .vgi_build_request(endpoint, token, headers, version) |>
    httr2::req_body_json(body)

  limiter <- .vgi_get_global_limiter()
  if (!is.null(limiter)) limiter$increment()

  .vgi_perform(req, sprintf("POST %s/%s", version, endpoint))
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
  
  if (is.character(date)) {
    parsed <- tryCatch(as.Date(date), error = function(e) NA)
    if (is.na(parsed)) {
      stop("Invalid date format. Please use YYYY-MM-DD format.")
    }
    return(format(parsed, "%Y-%m-%d"))
  }
  
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

# --- Tidyverse name cleaning ---

.vgi_clean_names <- function(df) {
  if (!is.data.frame(df) || ncol(df) == 0) return(df)
  names(df) <- gsub("([a-z0-9])([A-Z])", "\\1_\\2", names(df))
  names(df) <- tolower(names(df))
  tibble::as_tibble(df)
}

.vgi_clean_list <- function(x) {
  if (is.data.frame(x)) return(.vgi_clean_names(x))
  if (is.list(x)) {
    out <- lapply(x, function(el) {
      if (is.data.frame(el)) .vgi_clean_names(el) else el
    })
    if (!is.null(names(out))) {
      nms <- gsub("([a-z0-9])([A-Z])", "\\1_\\2", names(out))
      names(out) <- tolower(nms)
    }
    return(out)
  }
  x
}

# --- Identifier / query helpers ---

.vgi_to_csv_ids <- function(ids) {
  if (is.null(ids) || length(ids) == 0) return(NULL)
  ids <- unique(ids[!is.na(ids)])
  if (length(ids) == 0) return(NULL)
  paste(ids, collapse = ",")
}

.vgi_to_csv_chr <- function(x) {
  if (is.null(x) || length(x) == 0) return(NULL)
  x <- unique(as.character(x[!is.na(x)]))
  x <- x[nzchar(x)]
  if (length(x) == 0) return(NULL)
  paste(x, collapse = ",")
}

#' Build the shared v4 identifier/paging query parameters.
#' @noRd
.vgi_v4_query <- function(steam_app_ids = NULL, vgi_ids = NULL, slugs = NULL,
                          limit = NULL, cursor = NULL, countries = NULL, regions = NULL,
                          ...) {
  qp <- list(...)
  qp$steamAppIds <- .vgi_to_csv_ids(steam_app_ids)
  qp$vgiIds <- .vgi_to_csv_ids(vgi_ids)
  qp$slugs <- .vgi_to_csv_chr(slugs)
  qp$countries <- .vgi_to_csv_chr(countries)
  qp$regions <- .vgi_to_csv_chr(regions)
  if (!is.null(limit)) {
    validate_numeric(limit, "limit", min_val = 1, max_val = 1000)
    qp$limit <- as.integer(limit)
  }
  if (!is.null(cursor)) {
    validate_numeric(cursor, "cursor", min_val = 0)
    qp$cursor <- as.integer(cursor)
  }
  qp[!vapply(qp, is.null, logical(1))]
}

.vgi_parse_steam_app_id <- function(url_or_id) {
  if (is.null(url_or_id) || length(url_or_id) == 0) return(NA_integer_)
  if (is.numeric(url_or_id)) return(as.integer(url_or_id[[1]]))
  value <- as.character(url_or_id[[1]])
  if (is.na(value) || !nzchar(value)) return(NA_integer_)
  if (grepl("^[0-9]+$", value)) return(as.integer(value))

  # Steam URLs are usually .../app/<id>
  m <- regexpr("/app/([0-9]+)", value, perl = TRUE)
  if (m[1] == -1) return(NA_integer_)
  hit <- regmatches(value, m)
  as.integer(sub("/app/", "", hit))
}

# --- Response envelope / pagination helpers ---

#' Unwrap a v4 `{nextCursor, results}` envelope (pass-through for v3 arrays).
#' @noRd
.vgi_unwrap_results <- function(result) {
  if (is.list(result) && !is.data.frame(result) && "results" %in% names(result)) {
    return(result$results)
  }
  result
}

#' Steam App ID column from either API generation.
#'
#' v3 rows carry `steamAppId`; v4 rows carry `platform` + `externalId`.
#' Non-steam v4 rows yield NA.
#' @noRd
.vgi_steam_ids <- function(rows) {
  if (!is.data.frame(rows) || nrow(rows) == 0) return(integer())
  if ("steamAppId" %in% names(rows)) return(suppressWarnings(as.integer(rows$steamAppId)))
  if ("externalId" %in% names(rows)) {
    ids <- suppressWarnings(as.integer(rows$externalId))
    if ("platform" %in% names(rows)) ids[!is.na(rows$platform) & rows$platform != "steam"] <- NA_integer_
    return(ids)
  }
  rep(NA_integer_, nrow(rows))
}

#' Filter unwrapped multi-platform results to the steam row matching a
#' given Steam App ID. Returns a single-row data frame or NULL.
#' @noRd
.vgi_steam_row <- function(rows, steam_app_id) {
  if (!is.data.frame(rows) || nrow(rows) == 0) return(NULL)
  ids <- .vgi_steam_ids(rows)
  rows <- rows[!is.na(ids) & ids == as.integer(steam_app_id), , drop = FALSE]
  if (nrow(rows) == 0) return(NULL)
  rows[1, , drop = FALSE]
}

#' Fetch one or every page of a v4 cursor-paginated endpoint.
#'
#' Returns a list with `results` (data frame, possibly empty) and
#' `next_cursor` (NULL when exhausted). With `all_pages = TRUE` the pages are
#' row-bound; iteration stops when the API returns no rows or no cursor.
#'
#' The v4 API ignores `cursor` when identifier filters (`steamAppIds`,
#' `vgiIds`, `slugs`) are present and answers every request with the same
#' page and the same `nextCursor` (the last vgiId). A response whose
#' `nextCursor` repeats the previous one is therefore a replay of a page
#' already collected: it is discarded and paging stops.
#' @noRd
.vgi_fetch_v4_pages <- function(endpoint, query_params = list(), auth_token = NULL,
                                headers = list(), all_pages = FALSE,
                                max_pages = getOption("vgi.max_pages", 1000)) {
  pages <- list()
  cursor <- query_params$cursor
  next_cursor <- NULL
  n <- 0L
  repeat {
    qp <- query_params
    qp$cursor <- cursor
    raw <- make_api_request(endpoint = endpoint, query_params = qp,
                            auth_token = auth_token, method = "GET",
                            headers = headers, version = "v4")
    rows <- .vgi_unwrap_results(raw)
    prev_next <- next_cursor
    next_cursor <- if (is.list(raw) && !is.data.frame(raw)) raw$nextCursor else NULL
    if (length(next_cursor) == 0 || all(is.na(next_cursor))) next_cursor <- NULL
    if (n > 0L && !is.null(next_cursor) &&
        identical(as.numeric(next_cursor), as.numeric(prev_next))) {
      next_cursor <- NULL
      break
    }
    if (is.data.frame(rows) && nrow(rows) > 0) pages[[length(pages) + 1]] <- rows
    n <- n + 1L
    if (!isTRUE(all_pages)) break
    if (!is.data.frame(rows) || nrow(rows) == 0 || is.null(next_cursor)) break
    if (!is.null(cursor) && identical(as.numeric(next_cursor), as.numeric(cursor))) break
    if (n >= max_pages) break
    cursor <- next_cursor
  }
  results <- if (length(pages) == 0) data.frame() else dplyr::bind_rows(pages)
  list(results = results, next_cursor = next_cursor)
}

#' Full daily history of one Steam game from v4 `/historical-data`.
#'
#' With `steamAppIds` and no `date`, the endpoint returns every day since the
#' game was first tracked (one world-wide row per day) in a single response.
#' Rows are filtered to the requested Steam game and the `WW` slice,
#' de-duplicated by date and sorted. Column names are the raw API names, which
#' match the v3 per-game endpoints except for the renamed revenue and units
#' fields (`premiumRevenue*`, `unitsOwned*`, `paidUnits*`).
#' @noRd
.vgi_v4_game_history <- function(steam_app_id, auth_token = NULL, headers = list()) {
  rows <- .vgi_fetch_v4_pages(
    endpoint = "historical-data",
    query_params = list(steamAppIds = as.character(as.integer(steam_app_id))),
    auth_token = auth_token,
    headers = headers
  )$results
  if (!is.data.frame(rows) || nrow(rows) == 0 || !"date" %in% names(rows)) return(data.frame())
  ids <- .vgi_steam_ids(rows)
  rows <- rows[!is.na(ids) & ids == as.integer(steam_app_id), , drop = FALSE]
  if ("country" %in% names(rows)) rows <- rows[is.na(rows$country) | rows$country == "WW", , drop = FALSE]
  rows <- rows[!is.na(rows$date) & !duplicated(rows$date), , drop = FALSE]
  rows <- rows[, !grepl("^__", names(rows)), drop = FALSE]
  rows[order(rows$date), , drop = FALSE]
}

#' One Steam game's record from a v4 multi-game player-insights endpoint,
#' returned in the v3 per-game shape (a list whose nested breakdowns are data
#' frames), or NULL when the API has no steam row for the game.
#' @noRd
.vgi_v4_game_record <- function(endpoint, steam_app_id, auth_token = NULL, headers = list()) {
  raw <- make_api_request(
    endpoint = endpoint,
    query_params = list(steamAppIds = as.character(as.integer(steam_app_id))),
    auth_token = auth_token,
    method = "GET",
    headers = headers,
    version = "v4"
  )
  row <- .vgi_steam_row(.vgi_unwrap_results(raw), steam_app_id)
  if (is.null(row)) return(NULL)
  rec <- lapply(as.list(row), function(v) if (is.list(v)) v[[1]] else v)
  rec$steamAppId <- as.integer(steam_app_id)
  rec
}

#' Fetch one or every page of a v3 offset/limit endpoint (returns bare arrays).
#' @noRd
.vgi_fetch_v3_pages <- function(endpoint, query_params = list(), auth_token = NULL,
                                headers = list(), all_pages = FALSE, page_size = 1000,
                                max_pages = getOption("vgi.max_pages", 1000)) {
  pages <- list()
  offset <- as.integer(query_params$offset %||% 0L)
  n <- 0L
  repeat {
    qp <- query_params
    if (isTRUE(all_pages)) {
      qp$offset <- offset
      qp$limit <- as.integer(page_size)
    }
    rows <- make_api_request(endpoint = endpoint, query_params = qp,
                             auth_token = auth_token, method = "GET",
                             headers = headers, version = "v3")
    if (is.data.frame(rows) && nrow(rows) > 0) pages[[length(pages) + 1]] <- rows
    n <- n + 1L
    if (!isTRUE(all_pages)) break
    if (!is.data.frame(rows) || nrow(rows) < page_size || n >= max_pages) break
    offset <- offset + as.integer(page_size)
  }
  if (length(pages) == 0) data.frame() else dplyr::bind_rows(pages)
}

#' v4 `historical-data` snapshot rows for one date (steam rows only unless
#' `all_platforms = TRUE`).
#' @noRd
.vgi_historical_results <- function(date,
                                    steam_app_ids = NULL,
                                    vgi_ids = NULL,
                                    slugs = NULL,
                                    countries = NULL,
                                    regions = NULL,
                                    limit = NULL,
                                    cursor = NULL,
                                    all_pages = FALSE,
                                    auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                                    headers = list()) {
  qp <- .vgi_v4_query(steam_app_ids = steam_app_ids, vgi_ids = vgi_ids, slugs = slugs,
                      limit = limit, cursor = cursor, countries = countries, regions = regions)
  if (!is.null(date)) qp$date <- format_date(date)

  .vgi_fetch_v4_pages(
    endpoint = "historical-data",
    query_params = qp,
    auth_token = auth_token,
    headers = headers,
    all_pages = all_pages
  )$results
}

# --- Deprecation helper ---

.vgi_deprecate <- function(what, instead) {
  rlang::warn(
    sprintf("%s is deprecated and will be removed in a future release. Use %s instead.", what, instead),
    .frequency = getOption("vgi.deprecation_frequency", "once"),
    .frequency_id = paste0("vgi_deprecate_", what),
    class = "vgi_deprecated"
  )
  invisible(NULL)
}

# --- Column helpers ---

#' Pull a column by the first matching candidate name, else NA of length n.
#' @noRd
.vgi_col <- function(df, candidates, default = NA) {
  for (nm in candidates) if (nm %in% names(df)) return(df[[nm]])
  rep(default, NROW(df))
}

#' Daily rows for one game from v4 `/historical-data` (default) or from a v3
#' per-game time-series endpoint (`v3_endpoint` is a sprintf template taking
#' the Steam App ID; `v3_element` names the array inside an object response).
#' @noRd
.vgi_game_series <- function(steam_app_id, version, v3_endpoint, v3_element = NULL,
                             auth_token = NULL, headers = list()) {
  if (identical(version, "v4")) {
    return(.vgi_v4_game_history(steam_app_id, auth_token = auth_token, headers = headers))
  }
  result <- make_api_request(
    endpoint = sprintf(v3_endpoint, as.integer(steam_app_id)),
    auth_token = auth_token,
    method = "GET",
    headers = headers,
    version = "v3"
  )
  if (!is.null(v3_element) && is.list(result) && !is.data.frame(result)) result <- result[[v3_element]]
  if (is.data.frame(result)) result else data.frame()
}
