#' Get Game Rankings
#'
#' Retrieve VGI rankings and percentiles across reviews, revenue, units sold,
#' followers and playtime from the v3 `/games/rankings` endpoint, or the
#' ranking of one game from `/games/{steamAppId}/rankings`.
#'
#' @param offset Integer. Number of records to skip (catalogue listing only).
#' @param limit Integer. Records to return (API default 5, maximum 1000).
#' @param date Deprecated and ignored. Rankings are always the API's current
#'   values. Supplying a value emits a one-time warning.
#' @param steam_app_id Integer. When supplied, return the single ranking row
#'   for that game instead of the catalogue listing.
#' @param all_pages Logical. Walk the whole ranking catalogue in pages of
#'   `limit` (catalogue listing only).
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A [tibble][tibble::tibble] with columns `steam_app_id`,
#'   `positive_reviews_rank`, `positive_reviews_prct`, `total_revenue_rank`,
#'   `total_revenue_prct`, `total_units_sold_rank`, `total_units_sold_prct`,
#'   `yesterday_units_sold_rank`, `yesterday_units_sold_prct`,
#'   `followers_rank`, `followers_prct`, `avg_playtime_rank`,
#'   `avg_playtime_prct`. Lower rank is better (1 = best); `*_prct` is the
#'   percentile position in the catalogue.
#'
#' @export
#' @examples
#' \dontrun{
#' vgi_game_rankings(limit = 100)
#' vgi_game_rankings(steam_app_id = 4019220)
#' }
vgi_game_rankings <- function(offset = NULL,
                              limit = NULL,
                              date = NULL,
                              auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                              headers = list(),
                              steam_app_id = NULL,
                              all_pages = FALSE) {

  if (!is.null(offset)) validate_numeric(offset, "offset", min_val = 0)
  if (!is.null(limit)) validate_numeric(limit, "limit", min_val = 1, max_val = 1000)
  if (!is.null(date)) {
    .vgi_deprecate("vgi_game_rankings(date=)", "the API's current rankings (argument is ignored)")
  }

  if (!is.null(steam_app_id)) {
    validate_numeric(steam_app_id, "steam_app_id")
    row <- make_api_request(
      endpoint = sprintf("games/%s/rankings", as.integer(steam_app_id)),
      auth_token = auth_token, method = "GET", headers = headers, version = "v3"
    )
    rows <- if (is.list(row) && !is.data.frame(row) && length(row) > 0) {
      as.data.frame(lapply(row, function(x) if (is.null(x)) NA else x), stringsAsFactors = FALSE)
    } else {
      row
    }
  } else {
    qp <- list()
    if (!is.null(offset)) qp$offset <- as.integer(offset)
    if (!is.null(limit)) qp$limit <- as.integer(limit)
    rows <- .vgi_fetch_v3_pages("games/rankings", qp, auth_token = auth_token, headers = headers,
                                all_pages = all_pages, page_size = as.integer(limit %||% 1000))
  }

  .vgi_rankings_rows(rows)
}

.vgi_rankings_rows <- function(rows) {
  cols <- c("positiveReviewsRank", "positiveReviewsPrct", "totalRevenueRank", "totalRevenuePrct",
            "totalUnitsSoldRank", "totalUnitsSoldPrct", "yesterdayUnitsSoldRank",
            "yesterdayUnitsSoldPrct", "followersRank", "followersPrct",
            "avgPlaytimeRank", "avgPlaytimePrct")
  if (!is.data.frame(rows) || nrow(rows) == 0) {
    empty <- tibble::tibble(steamAppId = integer())
    for (nm in cols) empty[[nm]] <- if (grepl("Rank$", nm)) integer() else numeric()
    return(.vgi_clean_names(empty))
  }
  out <- tibble::tibble(steamAppId = as.integer(rows$steamAppId))
  for (nm in cols) {
    v <- suppressWarnings(as.numeric(.vgi_col(rows, nm)))
    out[[nm]] <- if (grepl("Rank$", nm)) as.integer(v) else v
  }
  .vgi_clean_names(out)
}
