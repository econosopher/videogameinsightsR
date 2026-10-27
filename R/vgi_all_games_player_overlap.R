#' Get Player Overlap Across the Catalogue
#'
#' Walk `/player-overlap` game by game, starting after `cursor`. The endpoint
#' returns one game per request, so `limit` games cost `limit` requests and
#' each row can be large. For one game use [vgi_player_overlap()].
#'
#' @param limit Integer. Number of games to return.
#' @param cursor Integer. VGI ID after which to start (the `next_cursor`
#'   attribute of a previous call).
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A [tibble][tibble::tibble] with one row per game: `vgi_id`,
#'   `steam_app_id`, `top_overlaps` (list-column of overlap tibbles, see
#'   [vgi_player_overlap()]), `overlap_count`, `top_overlap_game` (VGI ID) and
#'   `top_overlap_pct`, sorted by `top_overlap_pct`. The attribute
#'   `next_cursor` carries the cursor for the next game.
#'
#' @export
#' @examples
#' \dontrun{
#' vgi_all_games_player_overlap(limit = 5)
#' }
vgi_all_games_player_overlap <- function(limit = 1,
                                         cursor = NULL,
                                         auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                                         headers = list()) {
  validate_numeric(limit, "limit", min_val = 1)
  if (!is.null(cursor)) validate_numeric(cursor, "cursor", min_val = 0)

  pages <- list()
  for (i in seq_len(as.integer(limit))) {
    raw <- make_api_request(
      endpoint = "player-overlap",
      query_params = list(limit = 1L, cursor = if (!is.null(cursor)) as.integer(cursor)),
      auth_token = auth_token, method = "GET", headers = headers
    )
    rows <- .vgi_unwrap_results(raw)
    if (is.data.frame(rows) && nrow(rows) > 0) pages[[length(pages) + 1]] <- rows[1, , drop = FALSE]
    next_cursor <- if (is.list(raw) && !is.data.frame(raw)) raw$nextCursor else NULL
    if (length(next_cursor) == 0 || identical(as.numeric(next_cursor), as.numeric(cursor))) {
      cursor <- NULL
      break
    }
    cursor <- next_cursor
  }

  if (length(pages) == 0) {
    out <- .vgi_clean_names(tibble::tibble(
      vgiId = integer(), steamAppId = integer(), topOverlaps = I(list()), overlapCount = integer(),
      topOverlapGame = integer(), topOverlapPct = numeric()
    ))
    attr(out, "next_cursor") <- cursor
    return(out)
  }
  rows <- dplyr::bind_rows(pages)
  nested <- if ("playerOverlaps" %in% names(rows)) rows$playerOverlaps else replicate(nrow(rows), NULL, simplify = FALSE)
  tbls <- lapply(nested, .vgi_overlap_rows)
  out <- tibble::tibble(
    vgiId = as.integer(.vgi_col(rows, "vgiId", NA_integer_)),
    steamAppId = .vgi_steam_ids(rows),
    topOverlaps = I(tbls),
    overlapCount = vapply(tbls, nrow, integer(1)),
    topOverlapGame = vapply(tbls, function(t) if (nrow(t) > 0) t$vgi_id[1] else NA_integer_, integer(1)),
    topOverlapPct = vapply(tbls, function(t) if (nrow(t) > 0) t$units_sold_overlap_percentage[1] else NA_real_, numeric(1))
  )
  out <- .vgi_clean_names(out[order(-out$topOverlapPct, na.last = TRUE), , drop = FALSE])
  attr(out, "next_cursor") <- cursor
  out
}
