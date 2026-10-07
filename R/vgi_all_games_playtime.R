#' Get Playtime for Many Games (v4)
#'
#' Retrieve playtime statistics for many games from the v4
#' `/player-insights/games/playtime` endpoint, optionally restricted to
#' players in given countries or regions. For one Steam game use
#' [vgi_insights_playtime()] (v3).
#'
#' @inheritParams vgi_all_games_top_countries
#' @param countries Character vector of 2-letter ISO country codes. Limits the
#'   numbers to players from those countries.
#' @param regions Character vector of VGI region slugs. Limits the numbers to
#'   players from those regions.
#' @return A [tibble][tibble::tibble] with one row per game: `vgi_id`,
#'   `platform`, `steam_app_id`, `avg_playtime`, `median_playtime` (minutes),
#'   `avg_playtime_rank`, `avg_playtime_prct`, `playtime_ranges` (list-column
#'   of tibbles with `range`, `percentage`) and `playtime_rank` (rank by
#'   average playtime within the returned rows). The attribute `next_cursor`
#'   carries the cursor for the next page.
#' @export
#' @examples
#' \dontrun{
#' vgi_all_games_playtime(steam_app_ids = 4019220, countries = "US")
#' }
vgi_all_games_playtime <- function(auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                                   headers = list(),
                                   steam_app_ids = NULL,
                                   vgi_ids = NULL,
                                   slugs = NULL,
                                   countries = NULL,
                                   regions = NULL,
                                   limit = NULL,
                                   cursor = NULL,
                                   all_pages = FALSE) {
  page <- .vgi_player_insights_pages("playtime", steam_app_ids, vgi_ids, slugs, limit, cursor,
                                     all_pages, auth_token, headers, countries = countries, regions = regions)
  rows <- page$results
  if (!is.data.frame(rows) || nrow(rows) == 0) {
    out <- .vgi_clean_names(tibble::tibble(
      vgiId = integer(), platform = character(), steamAppId = integer(),
      avgPlaytime = numeric(), medianPlaytime = numeric(), avgPlaytimeRank = integer(),
      avgPlaytimePrct = numeric(), playtimeRanges = I(list()), playtimeRank = integer()
    ))
    attr(out, "next_cursor") <- page$next_cursor
    return(out)
  }
  nested <- if ("playtime" %in% names(rows)) rows$playtime else replicate(nrow(rows), NULL, simplify = FALSE)
  ranges <- lapply(nested, function(df) {
    if (is.data.frame(df) && nrow(df) > 0) {
      tibble::tibble(range = as.character(df$range), percentage = as.numeric(df$percentage))
    } else {
      tibble::tibble(range = character(), percentage = numeric())
    }
  })
  out <- .vgi_game_identity(rows)
  out$avgPlaytime <- as.numeric(.vgi_col(rows, "avgPlaytime"))
  out$medianPlaytime <- as.numeric(.vgi_col(rows, "medianPlaytime"))
  out$avgPlaytimeRank <- as.integer(.vgi_col(rows, "avgPlaytimeRank", NA_integer_))
  out$avgPlaytimePrct <- as.numeric(.vgi_col(rows, "avgPlaytimePrct"))
  out$playtimeRanges <- I(ranges)
  out <- out[order(-out$avgPlaytime, na.last = TRUE), , drop = FALSE]
  out$playtimeRank <- seq_len(nrow(out))
  out <- .vgi_clean_names(out)
  attr(out, "next_cursor") <- page$next_cursor
  out
}
