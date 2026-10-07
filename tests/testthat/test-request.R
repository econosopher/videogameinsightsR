# Request plumbing: base URL / version resolution, auth, query building,
# pagination and response helpers. These are the contracts every endpoint
# function relies on, tested once here rather than per endpoint.

test_that("get_base_url resolves the version segment from the configured root", {
  withr::local_options(vgi.base_url = NULL)
  withr::local_envvar(VGI_BASE_URL = "")
  expect_equal(get_base_url(), "https://vginsights.com/api/v3")
  expect_equal(get_base_url("v4"), "https://vginsights.com/api/v4")
  expect_equal(get_base_url(4), "https://vginsights.com/api/v4")

  # Pre-0.2.0 configs pointed at a versioned URL; the suffix is stripped.
  withr::local_options(vgi.base_url = "https://vginsights.com/api/v4")
  expect_equal(get_base_url("v3"), "https://vginsights.com/api/v3")

  withr::local_options(vgi.base_url = NULL)
  withr::local_envvar(VGI_BASE_URL = "https://app.sensortower.com/vgi/api/")
  expect_equal(get_base_url("v4"), "https://app.sensortower.com/vgi/api/v4")

  expect_error(get_base_url("v2"), "Unsupported VGI API version")
})

test_that("get_auth_token prefers the argument, then the environment, else errors", {
  expect_equal(get_auth_token("explicit"), "explicit")
  withr::with_envvar(c(VGI_AUTH_TOKEN = "env_token"), expect_equal(get_auth_token(), "env_token"))
  withr::with_envvar(c(VGI_AUTH_TOKEN = ""), {
    expect_error(get_auth_token(), "Authentication token is required")
    expect_error(get_auth_token(""), "Authentication token is required")
  })
})

test_that("make_api_request sends the api-key header to the versioned path", {
  seen <- NULL
  httr2::local_mocked_responses(function(req) {
    seen <<- req
    httr2::response_json(body = list(ok = TRUE))
  })
  res <- make_api_request("games/1/metadata", auth_token = "tok", version = "v4")
  expect_equal(res$ok, TRUE)
  expect_equal(seen$url, "https://vginsights.com/api/v4/games/1/metadata")
  expect_equal(seen$headers[["api-key"]], "tok")
})

test_that("make_api_request drops NULL query params and raises classed HTTP errors", {
  seen <- NULL
  httr2::local_mocked_responses(function(req) {
    seen <<- req
    if (grepl("missing", req$url)) {
      return(httr2::response_json(status_code = 404L, body = list(message = "Game not found")))
    }
    httr2::response_json(body = list())
  })
  make_api_request("x", query_params = list(a = 1, b = NULL, c = "z"), auth_token = "tok")
  expect_equal(seen$url, "https://vginsights.com/api/v3/x?a=1&c=z")

  err <- tryCatch(make_api_request("missing", auth_token = "tok"), vgi_http_error = function(e) e)
  expect_s3_class(err, "vgi_http_error")
  expect_equal(err$status, 404L)
  expect_match(conditionMessage(err), "API request failed \\[404\\]")
})

test_that(".vgi_v4_query builds comma-separated identifier filters and validates paging", {
  qp <- .vgi_v4_query(steam_app_ids = c(10, 20, 10, NA), slugs = c("a", "b"), limit = 50, cursor = 7,
                      countries = c("US", "DE"))
  expect_mapequal(qp, list(limit = 50L, cursor = 7L, steamAppIds = "10,20", slugs = "a,b", countries = "US,DE"))
  expect_equal(.vgi_v4_query(), list())
  expect_error(.vgi_v4_query(limit = 0), "limit must be at least 1")
  expect_error(.vgi_v4_query(limit = 1001), "limit must be at most 1000")
  expect_error(.vgi_v4_query(cursor = -1), "cursor must be at least 0")
})

test_that(".vgi_single_game_query requires exactly one identifier", {
  expect_equal(.vgi_single_game_query(steam_app_id = 4019220), list(steamAppId = "4019220"))
  expect_equal(.vgi_single_game_query(vgi_id = 2047657), list(vgiId = "2047657"))
  expect_equal(.vgi_single_game_query(slug = "dressmaker"), list(slug = "dressmaker"))
  expect_error(.vgi_single_game_query(), "exactly one")
  expect_error(.vgi_single_game_query(steam_app_id = 1, slug = "x"), "exactly one")
})

test_that(".vgi_steam_ids reads v3 steamAppId and v4 platform/externalId rows", {
  v3 <- data.frame(steamAppId = c(10, 20))
  v4 <- data.frame(platform = c("steam", "xbox", "steam"), externalId = c("4019220", "ABC123", "730"))
  expect_equal(.vgi_steam_ids(v3), c(10L, 20L))
  expect_equal(.vgi_steam_ids(v4), c(4019220L, NA, 730L))
  expect_equal(.vgi_steam_ids(data.frame()), integer())
  expect_equal(.vgi_steam_row(v4, 730)$externalId, "730")
  expect_null(.vgi_steam_row(v4, 999))
})

test_that(".vgi_parse_steam_app_id extracts ids from store URLs and numbers", {
  expect_equal(.vgi_parse_steam_app_id("https://store.steampowered.com/app/4019220"), 4019220L)
  expect_equal(.vgi_parse_steam_app_id("730"), 730L)
  expect_equal(.vgi_parse_steam_app_id(892970), 892970L)
  expect_equal(.vgi_parse_steam_app_id(NA_character_), NA_integer_)
  expect_equal(.vgi_parse_steam_app_id("https://example.com/no-id"), NA_integer_)
})

test_that(".vgi_fetch_v4_pages follows nextCursor until exhausted and stops on repeats", {
  calls <- list()
  pages <- list(
    list(nextCursor = 2, results = data.frame(vgiId = 1:2)),
    list(nextCursor = 4, results = data.frame(vgiId = 3:4)),
    list(nextCursor = NULL, results = data.frame(vgiId = 5L))
  )
  i <- 0L
  testthat::local_mocked_bindings(
    make_api_request = function(endpoint, query_params = list(), ...) {
      calls[[length(calls) + 1]] <<- query_params
      i <<- i + 1L
      pages[[i]]
    },
    .package = "VideoGameInsightsR"
  )
  out <- .vgi_fetch_v4_pages("games/metadata", list(limit = 2L), all_pages = TRUE)
  expect_equal(out$results$vgiId, 1:5)
  expect_null(out$next_cursor)
  expect_equal(vapply(calls, function(q) q$cursor %||% NA_integer_, numeric(1)), c(NA, 2, 4))

  # Single page by default, cursor passed through.
  i <- 0L
  one <- .vgi_fetch_v4_pages("games/metadata", list(limit = 2L), all_pages = FALSE)
  expect_equal(one$results$vgiId, 1:2)
  expect_equal(one$next_cursor, 2)

  # A server that keeps returning the same cursor must not loop forever.
  testthat::local_mocked_bindings(
    make_api_request = function(...) list(nextCursor = 9, results = data.frame(vgiId = 9L)),
    .package = "VideoGameInsightsR"
  )
  stuck <- .vgi_fetch_v4_pages("x", list(cursor = 9L), all_pages = TRUE)
  expect_equal(nrow(stuck$results), 1)
})

test_that(".vgi_fetch_v4_pages does not duplicate a page the API replays for id filters", {
  # Live behaviour: with steamAppIds the v4 API ignores `cursor` and returns
  # the same rows and the same nextCursor (the last vgiId) on every call.
  # 0.1.1 followed that cursor once and returned every history row twice.
  testthat::local_mocked_bindings(
    make_api_request = function(...) {
      list(nextCursor = 2047657L, results = data.frame(vgiId = 2047657L, date = c("2026-10-05", "2026-10-06")))
    },
    .package = "VideoGameInsightsR"
  )
  out <- .vgi_fetch_v4_pages("historical-data", list(steamAppIds = "4019220"), all_pages = TRUE)
  expect_equal(out$results$date, c("2026-10-05", "2026-10-06"))
  expect_null(out$next_cursor)
})

test_that(".vgi_fetch_v3_pages advances offset by page size until a short page", {
  offsets <- integer()
  testthat::local_mocked_bindings(
    make_api_request = function(endpoint, query_params = list(), ...) {
      offsets <<- c(offsets, query_params$offset)
      n <- if (query_params$offset >= 4) 1 else 2
      data.frame(steamAppId = seq_len(n) + query_params$offset)
    },
    .package = "VideoGameInsightsR"
  )
  out <- .vgi_fetch_v3_pages("games/rankings", list(), all_pages = TRUE, page_size = 2)
  expect_equal(out$steamAppId, c(1, 2, 3, 4, 5))
  expect_equal(offsets, c(0L, 2L, 4L))
})

test_that(".vgi_clean_list snake_cases list element names and nested tibbles", {
  out <- .vgi_clean_list(list(steamAppId = 1L, playerHistory = data.frame(ccuMax = 5)))
  expect_named(out, c("steam_app_id", "player_history"))
  expect_named(out$player_history, "ccu_max")
  expect_s3_class(out$player_history, "tbl_df")
})

test_that("deprecated aliases warn once per session and delegate", {
  testthat::local_mocked_bindings(
    vgi_units_sold_by_date = function(date, steam_app_ids = NULL, ...) {
      tibble::tibble(steam_app_id = steam_app_ids, date = date, units_sold = 5L, daily_units = 1L, sales_rank = 1L)
    },
    .package = "VideoGameInsightsR"
  )
  withr::local_options(vgi.deprecation_frequency = "always")
  expect_warning(out <- vgi_entitlements_by_date("2026-09-28", 1), class = "vgi_deprecated")
  expect_equal(out$units_sold, 5L)
})

test_that("format_date and validate_* enforce input contracts", {
  expect_equal(format_date("2023-01-15"), "2023-01-15")
  expect_equal(format_date(as.Date("2023-01-15")), "2023-01-15")
  expect_error(format_date("not-a-date"), "Invalid date format")
  expect_error(format_date(123), "Date must be a Date object")
  expect_equal(validate_date("2024-01-01"), "2024-01-01")
  expect_null(validate_date(NULL))
  expect_error(validate_date("not a date"), "must be a Date object")
  expect_silent(validate_numeric(5, "x", min_val = 1, max_val = 10))
  expect_error(validate_numeric("abc", "x"), "x must be numeric")
  expect_error(validate_numeric(0, "x", min_val = 1), "x must be at least 1")
  expect_error(validate_numeric(11, "x", max_val = 10), "x must be at most 10")
  expect_silent(validate_platform("steam"))
  expect_error(validate_platform("pc"), "Invalid platform")
})

test_that("process_api_response coerces responses to tibbles and pads expected fields", {
  expect_equal(nrow(process_api_response(data.frame(a = 1:3))), 3)
  expect_equal(nrow(process_api_response(list(data = data.frame(x = 1:5)))), 5)
  expect_equal(nrow(process_api_response(NULL)), 0)
  padded <- process_api_response(data.frame(a = 1:3), c("a", "b"))
  expect_true(all(is.na(padded$b)))
})
