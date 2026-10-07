#' Get Player Overlap Across the Catalogue (v3)
#'
#' Retrieve player-overlap lists for many games from the v3
#' `/player-insights/games/player-overlap` endpoint (offset paging; the API
#' returns one game per call by default and each row can be large). For one
#' game use [vgi_player_overlap()].
#'
#' @param offset Integer. Games to skip.
#' @param limit Integer. Games to return (API default 1).
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A [tibble][tibble::tibble] with one row per game: `steam_app_id`,
#'   `top_overlaps` (list-column of overlap tibbles, see
#'   [vgi_player_overlap()]), `overlap_count`, `top_overlap_game`,
#'   `top_overlap_pct`.
#'
#' @export
#' @examples
#' \dontrun{
#' vgi_all_games_player_overlap(limit = 5)
#' }
vgi_all_games_player_overlap <- function(offset = NULL, limit = NULL,
                                        auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                                        headers = list()) {
  if (!is.null(offset)) validate_numeric(offset, "offset", min_val = 0)
  if (!is.null(limit)) validate_numeric(limit, "limit", min_val = 1)
  qp <- list()
  if (!is.null(offset)) qp$offset <- as.integer(offset)
  if (!is.null(limit)) qp$limit <- as.integer(limit)

  rows <- make_api_request(
    endpoint = "player-insights/games/player-overlap", query_params = qp,
    auth_token = auth_token, method = "GET", headers = headers, version = "v3"
  )
  if (!is.data.frame(rows) || nrow(rows) == 0) {
    return(.vgi_clean_names(tibble::tibble(
      steamAppId = integer(), topOverlaps = I(list()), overlapCount = integer(),
      topOverlapGame = integer(), topOverlapPct = numeric()
    )))
  }
  nested <- if ("playerOverlaps" %in% names(rows)) rows$playerOverlaps else replicate(nrow(rows), NULL, simplify = FALSE)
  tbls <- lapply(nested, .vgi_overlap_rows)
  out <- tibble::tibble(
    steamAppId = .vgi_steam_ids(rows),
    topOverlaps = I(tbls),
    overlapCount = vapply(tbls, nrow, integer(1)),
    topOverlapGame = vapply(tbls, function(t) if (nrow(t) > 0) t$steam_app_id[1] else NA_integer_, integer(1)),
    topOverlapPct = vapply(tbls, function(t) if (nrow(t) > 0) t$units_sold_overlap_percentage[1] else NA_real_, numeric(1))
  )
  out <- out[!is.na(out$steamAppId), , drop = FALSE]
  .vgi_clean_names(out[order(-out$topOverlapPct, na.last = TRUE), , drop = FALSE])
}
