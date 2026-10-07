#' Get Playtime Insights for a Game
#'
#' Retrieve playtime statistics for a single Steam game. For many games at
#' once, or for country / region filtered playtime, use
#' [vgi_all_games_playtime()] (v4).
#'
#' @param steam_app_id Integer. The Steam App ID of the game.
#' @param version API generation. `"v4"` (default) reads the Steam row of
#'   the multi-game v4 `/player-insights/games/playtime` endpoint; `"v3"`
#'   reads `/player-insights/games/{steamAppId}/playtime`. Both return the same
#'   figures.
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A list containing `steam_app_id`, `avg_playtime` and
#'   `median_playtime` (minutes), `avg_playtime_rank`, `avg_playtime_prct`,
#'   and `playtime_ranges`, a tibble with columns `range` and `percentage`.
#'
#' @export
#' @examples
#' \dontrun{
#' pt <- vgi_insights_playtime(steam_app_id = 4019220)
#' pt$median_playtime
#' pt$playtime_ranges
#' }
vgi_insights_playtime <- function(steam_app_id,
                                  version = c("v4", "v3"),
                                  auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                                  headers = list()) {

  validate_numeric(steam_app_id, "steam_app_id")

  version <- .vgi_api_version(match.arg(version))
  result <- if (version == "v4") {
    .vgi_v4_game_record("player-insights/games/playtime", steam_app_id,
                        auth_token = auth_token, headers = headers)
  } else {
    make_api_request(
      endpoint = sprintf("player-insights/games/%s/playtime", as.integer(steam_app_id)),
      auth_token = auth_token,
      method = "GET",
      headers = headers,
      version = "v3"
    )
  }

  ranges <- if (is.list(result)) result$playtimeRanges %||% result$playtime else NULL
  ranges_df <- if (is.data.frame(ranges) && nrow(ranges) > 0) {
    tibble::tibble(range = as.character(ranges$range), percentage = as.numeric(ranges$percentage))
  } else {
    tibble::tibble(range = character(), percentage = numeric())
  }

  .vgi_clean_list(list(
    steamAppId = as.integer(result$steamAppId %||% steam_app_id),
    avgPlaytime = as.numeric(result$avgPlaytime %||% NA_real_),
    medianPlaytime = as.numeric(result$medianPlaytime %||% NA_real_),
    avgPlaytimeRank = as.integer(result$avgPlaytimeRank %||% NA_integer_),
    avgPlaytimePrct = as.numeric(result$avgPlaytimePrct %||% NA_real_),
    playtimeRanges = ranges_df
  ))
}
