#' Get Player Overlap for a Game
#'
#' Retrieve the games whose audiences overlap most with a given game from
#' `/player-overlap`. Identify the game by Steam App ID, VGI ID or slug.
#'
#' @param steam_app_id Integer. The Steam App ID of the game.
#' @param limit Integer. Maximum number of overlapping games to return, largest
#'   units overlap first.
#' @param vgi_id Integer. VGI internal game ID.
#' @param slug Character. VGI game slug.
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A list containing:
#' \describe{
#'   \item{steam_app_id}{Integer. The requested game (NA for non-Steam games)}
#'   \item{vgi_id}{Integer. VGI ID of the requested game}
#'   \item{player_overlaps}{Tibble with one row per overlapping game:
#'     `steam_app_id`, `vgi_id`, `median_playtime`, `units_sold_overlap`,
#'     `units_sold_overlap_percentage`, `units_sold_overlap_index`,
#'     `mau_overlap`, `mau_overlap_percentage`, `mau_overlap_index`,
#'     `wishlist_overlap`, `wishlist_overlap_percentage`,
#'     `wishlist_overlap_index`}
#' }
#'
#' @export
#' @examples
#' \dontrun{
#' ov <- vgi_player_overlap(4019220, limit = 20)
#' head(ov$player_overlaps)
#'
#' vgi_player_overlap(slug = "dressmaker")
#' }
vgi_player_overlap <- function(steam_app_id = NULL,
                               limit = 10,
                               auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                               headers = list(),
                               vgi_id = NULL,
                               slug = NULL) {
  validate_numeric(limit, "limit", min_val = 1)

  qp <- .vgi_single_game_query(steam_app_id, vgi_id, slug)
  qp$limit <- 1L
  result <- make_api_request(
    endpoint = "player-overlap", query_params = qp,
    auth_token = auth_token, method = "GET", headers = headers
  )
  rows <- .vgi_unwrap_results(result)
  overlaps <- NULL
  requested_vgi <- NA_integer_
  requested_steam <- if (!is.null(steam_app_id)) as.integer(steam_app_id) else NA_integer_
  if (is.data.frame(rows) && nrow(rows) > 0) {
    row <- rows[1, , drop = FALSE]
    requested_vgi <- as.integer(.vgi_col(row, "vgiId", NA_integer_))
    if (is.na(requested_steam)) requested_steam <- .vgi_steam_ids(row)
    if ("playerOverlaps" %in% names(row)) overlaps <- row$playerOverlaps[[1]]
  }

  overlaps_df <- .vgi_overlap_rows(overlaps)
  if (nrow(overlaps_df) > limit) overlaps_df <- overlaps_df[seq_len(limit), , drop = FALSE]

  .vgi_clean_list(list(
    steamAppId = requested_steam,
    vgiId = requested_vgi,
    playerOverlaps = overlaps_df
  ))
}

.vgi_overlap_rows <- function(overlaps) {
  metric_cols <- c("medianPlaytime", "unitsSoldOverlap", "unitsSoldOverlapPercentage",
                   "unitsSoldOverlapIndex", "mauOverlap", "mauOverlapPercentage",
                   "mauOverlapIndex", "wishlistOverlap", "wishlistOverlapPercentage",
                   "wishlistOverlapIndex")
  if (!is.data.frame(overlaps) || nrow(overlaps) == 0) {
    out <- tibble::tibble(steamAppId = integer(), vgiId = integer())
    for (nm in metric_cols) out[[nm]] <- numeric()
    return(.vgi_clean_names(out))
  }
  out <- tibble::tibble(
    steamAppId = .vgi_steam_ids(overlaps),
    vgiId = as.integer(.vgi_col(overlaps, "vgiId", NA_integer_))
  )
  for (nm in metric_cols) out[[nm]] <- suppressWarnings(as.numeric(.vgi_col(overlaps, nm)))
  # Drop games the API lists without any overlap figures.
  informative <- rowSums(!is.na(as.matrix(out[, metric_cols]))) > 0
  out <- out[informative, , drop = FALSE]
  out <- out[order(-out$unitsSoldOverlapPercentage, na.last = TRUE), , drop = FALSE]
  .vgi_clean_names(out)
}
