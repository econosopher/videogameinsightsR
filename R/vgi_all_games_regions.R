#' Get Player Regions for Many Games (v4)
#'
#' Retrieve the player-share-by-region breakdown for many games from the v4
#' `/player-insights/games/top-regions` endpoint, widened to one column per
#' region. For one Steam game use [vgi_top_regions()] (v3).
#'
#' @inheritParams vgi_all_games_top_countries
#' @return A [tibble][tibble::tibble] with one row per game: `vgi_id`,
#'   `platform`, `steam_app_id`, `north_america`, `europe`, `asia`,
#'   `south_america`, `oceania`, `africa`, `middle_east` (percentages) and
#'   `dominant_region`. The attribute `next_cursor` carries the cursor for
#'   the next page.
#' @export
#' @examples
#' \dontrun{
#' vgi_all_games_regions(steam_app_ids = 4019220)
#' }
vgi_all_games_regions <- function(auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                                  headers = list(),
                                  steam_app_ids = NULL,
                                  vgi_ids = NULL,
                                  slugs = NULL,
                                  limit = NULL,
                                  cursor = NULL,
                                  all_pages = FALSE) {
  page <- .vgi_player_insights_pages("top-regions", steam_app_ids, vgi_ids, slugs, limit, cursor,
                                     all_pages, auth_token, headers)
  rows <- page$results
  region_keys <- c("north america" = "northAmerica", "europe" = "europe", "asia" = "asia",
                   "south america" = "southAmerica", "oceania" = "oceania", "africa" = "africa",
                   "middle east" = "middleEast")

  if (!is.data.frame(rows) || nrow(rows) == 0) {
    out <- tibble::tibble(vgiId = integer(), platform = character(), steamAppId = integer())
    for (k in region_keys) out[[k]] <- numeric()
    out$dominantRegion <- character()
    out <- .vgi_clean_names(out)
    attr(out, "next_cursor") <- page$next_cursor
    return(out)
  }

  nested <- if ("topRegions" %in% names(rows)) rows$topRegions else replicate(nrow(rows), NULL, simplify = FALSE)
  shares <- t(vapply(nested, function(reg_df) {
    vals <- stats::setNames(rep(0, length(region_keys)), region_keys)
    if (is.data.frame(reg_df) && nrow(reg_df) > 0) {
      keys <- region_keys[tolower(reg_df$regionName)]
      ok <- !is.na(keys)
      vals[keys[ok]] <- as.numeric(reg_df$percentage[ok])
    }
    vals
  }, numeric(length(region_keys))))

  out <- .vgi_game_identity(rows)
  for (k in region_keys) out[[k]] <- as.numeric(shares[, k])
  out$dominantRegion <- region_keys[apply(shares, 1, which.max)]
  attr_cursor <- page$next_cursor
  out <- .vgi_clean_names(out)
  attr(out, "next_cursor") <- attr_cursor
  out
}
