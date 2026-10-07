#' Get Player Overlap for a Game
#'
#' Retrieve the games whose audiences overlap most with a given game. The v3
#' endpoint `/player-insights/games/{steamAppId}/player-overlap` is used when
#' a `steam_app_id` is given and `version = "v3"` (the default). Supplying
#' `vgi_id` or `slug`, or `version = "v4"`, uses the multi-platform v4
#' `/player-overlap` endpoint.
#'
#' v3 stays the default here, unlike the other per-game functions: as of
#' October 2026 the v4 endpoint lists the overlapping games but returns only
#' `null` overlap percentages and no counts or indices (checked for Steam
#' 4019220, 730 and 892970), while v3 returns the full figures.
#'
#' @param steam_app_id Integer. The Steam App ID of the game.
#' @param limit Integer. Maximum number of overlapping games to return. In
#'   v3 this is the API page size: the API lists overlapping games in Steam
#'   App ID order, so to find the largest overlaps request a large `limit`
#'   (Dressmaker has about 3,900 rows); returned rows are sorted by units
#'   overlap.
#' @param offset Integer. Records to skip (v3 only).
#' @param vgi_id Integer. VGI internal game ID (v4 only).
#' @param slug Character. VGI game slug (v4 only).
#' @param version `"v3"` or `"v4"`. Inferred as `"v4"` when `vgi_id` or
#'   `slug` is supplied.
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A list containing:
#' \describe{
#'   \item{steam_app_id}{Integer. The requested game (NA for non-Steam v4 lookups)}
#'   \item{vgi_id}{Integer. VGI ID of the requested game (v4 only, else NA)}
#'   \item{player_overlaps}{Tibble with one row per overlapping game:
#'     `steam_app_id`, `vgi_id` (v4), `median_playtime`, `units_sold_overlap`,
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
                             offset = 0,
                             vgi_id = NULL,
                             slug = NULL,
                             version = c("v3", "v4"),
                             auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                             headers = list()) {
  if (!is.null(vgi_id) || !is.null(slug)) version <- "v4"
  version <- .vgi_api_version(match.arg(version))
  validate_numeric(limit, "limit", min_val = 1)
  validate_numeric(offset, "offset", min_val = 0)

  if (version == "v3") {
    if (is.null(steam_app_id)) stop("steam_app_id is required for the v3 player-overlap endpoint")
    validate_numeric(steam_app_id, "steam_app_id")
    result <- make_api_request(
      endpoint = sprintf("player-insights/games/%s/player-overlap", as.integer(steam_app_id)),
      query_params = list(limit = as.integer(limit), offset = if (offset > 0) as.integer(offset) else NULL),
      auth_token = auth_token, method = "GET", headers = headers, version = "v3"
    )
    overlaps <- result$playerOverlaps
    requested_vgi <- NA_integer_
    requested_steam <- as.integer(steam_app_id)
  } else {
    qp <- .vgi_single_game_query(steam_app_id, vgi_id, slug)
    qp$limit <- 1L
    result <- make_api_request(
      endpoint = "player-overlap", query_params = qp,
      auth_token = auth_token, method = "GET", headers = headers, version = "v4"
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
