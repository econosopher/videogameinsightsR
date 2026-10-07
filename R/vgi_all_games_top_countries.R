#' Get Top Countries for Many Games (v4)
#'
#' Retrieve the player-share-by-country breakdown for many games from the v4
#' `/player-insights/games/top-countries` endpoint. Games can be selected by
#' Steam App ID, VGI ID or slug; without identifiers the catalogue is paged
#' from the cursor. For one Steam game use [vgi_top_countries()] (v3).
#'
#' @param steam_app_ids Integer vector. Steam App IDs to select. Optional.
#' @param vgi_ids Integer vector. VGI internal game IDs to select. Optional.
#' @param slugs Character vector. VGI game slugs to select. Optional.
#' @param limit Integer. Games per page (API default 200, maximum 1000).
#' @param cursor Integer. Cursor from a previous page.
#' @param all_pages Logical. Follow the cursor through every page.
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A [tibble][tibble::tibble] with one row per game: `vgi_id`,
#'   `platform`, `steam_app_id`, `top_countries` (list-column of tibbles with
#'   `country`, `country_name`, `percentage`, `rank`), `country_count`,
#'   `top_country`, `top_country_pct`. The attribute `next_cursor` carries
#'   the cursor for the next page.
#'
#' @export
#' @examples
#' \dontrun{
#' tc <- vgi_all_games_top_countries(steam_app_ids = c(4019220, 730))
#' tc$top_countries[[1]]
#' }
vgi_all_games_top_countries <- function(auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                                        headers = list(),
                                        steam_app_ids = NULL,
                                        vgi_ids = NULL,
                                        slugs = NULL,
                                        limit = NULL,
                                        cursor = NULL,
                                        all_pages = FALSE) {
  page <- .vgi_player_insights_pages("top-countries", steam_app_ids, vgi_ids, slugs, limit, cursor,
                                     all_pages, auth_token, headers)
  out <- .vgi_nested_country_summary(page$results, "topCountries", "topCountries",
                                     c("countryCount", "topCountry", "topCountryPct"))
  attr(out, "next_cursor") <- page$next_cursor
  out
}

# Shared v4 player-insights fetch.
.vgi_player_insights_pages <- function(resource, steam_app_ids, vgi_ids, slugs, limit, cursor,
                                       all_pages, auth_token, headers, countries = NULL, regions = NULL) {
  qp <- .vgi_v4_query(steam_app_ids = steam_app_ids, vgi_ids = vgi_ids, slugs = slugs,
                      limit = limit, cursor = cursor, countries = countries, regions = regions)
  .vgi_fetch_v4_pages(sprintf("player-insights/games/%s", resource), qp,
                      auth_token = auth_token, headers = headers, all_pages = all_pages)
}

# Per-game identity columns shared by the v4 player-insights outputs.
.vgi_game_identity <- function(rows) {
  tibble::tibble(
    vgiId = as.integer(.vgi_col(rows, "vgiId", NA_integer_)),
    platform = as.character(.vgi_col(rows, "platform", NA_character_)),
    steamAppId = .vgi_steam_ids(rows)
  )
}

# Summarise a nested country list-column (players or wishlists).
.vgi_nested_country_summary <- function(rows, src_col, out_col, summary_cols) {
  if (!is.data.frame(rows) || nrow(rows) == 0) {
    out <- tibble::tibble(vgiId = integer(), platform = character(), steamAppId = integer())
    out[[out_col]] <- I(list())
    out[[summary_cols[1]]] <- integer()
    out[[summary_cols[2]]] <- character()
    out[[summary_cols[3]]] <- numeric()
    return(.vgi_clean_names(out))
  }
  nested <- if (src_col %in% names(rows)) rows[[src_col]] else replicate(nrow(rows), NULL, simplify = FALSE)
  tbls <- lapply(nested, .vgi_country_rows)
  out <- .vgi_game_identity(rows)
  out[[out_col]] <- I(tbls)
  out[[summary_cols[1]]] <- vapply(tbls, nrow, integer(1))
  out[[summary_cols[2]]] <- vapply(tbls, function(t) if (nrow(t) > 0) t$country[1] else NA_character_, character(1))
  out[[summary_cols[3]]] <- vapply(tbls, function(t) if (nrow(t) > 0) t$percentage[1] else NA_real_, numeric(1))
  .vgi_clean_names(out[order(-out[[summary_cols[3]]], na.last = TRUE), , drop = FALSE])
}
