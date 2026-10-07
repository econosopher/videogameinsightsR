#' Get Player Region Distribution for a Game
#'
#' Retrieve the share of players by world region for a single Steam game.
#'
#' @param steam_app_id Integer. The Steam App ID of the game.
#' @param version API generation. `"v4"` (default) reads the Steam row of
#'   the multi-game v4 `/player-insights/games/top-regions` endpoint; `"v3"`
#'   reads `/player-insights/games/{steamAppId}/regions`. Both return the same
#'   figures.
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A list containing `steam_app_id` and `regions`, a tibble with
#'   columns `region_name`, `rank` and `percentage`, sorted by rank.
#'
#' @seealso [vgi_top_regions()] returns just the regions tibble.
#' @export
#' @examples
#' \dontrun{
#' vgi_insights_player_regions(steam_app_id = 4019220)$regions
#' }
vgi_insights_player_regions <- function(steam_app_id,
                                        version = c("v4", "v3"),
                                        auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                                        headers = list()) {

  validate_numeric(steam_app_id, "steam_app_id")

  version <- .vgi_api_version(match.arg(version))
  result <- if (version == "v4") {
    .vgi_v4_game_record("player-insights/games/top-regions", steam_app_id,
                        auth_token = auth_token, headers = headers)
  } else {
    make_api_request(
      endpoint = sprintf("player-insights/games/%s/regions", as.integer(steam_app_id)),
      auth_token = auth_token,
      method = "GET",
      headers = headers,
      version = "v3"
    )
  }

  regions <- if (is.list(result)) result$regions %||% result$topRegions else NULL
  regions_df <- if (is.data.frame(regions) && nrow(regions) > 0) {
    df <- tibble::tibble(
      regionName = as.character(regions$regionName),
      rank = as.integer(.vgi_col(regions, "rank", NA_integer_)),
      percentage = as.numeric(regions$percentage)
    )
    df[order(df$rank), , drop = FALSE]
  } else {
    tibble::tibble(regionName = character(), rank = integer(), percentage = numeric())
  }

  .vgi_clean_list(list(steamAppId = as.integer(steam_app_id), regions = regions_df))
}
