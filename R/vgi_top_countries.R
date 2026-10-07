#' Get Top Countries for a Game
#'
#' Retrieve the countries with the largest share of a Steam game's players.
#' For many games at once use [vgi_all_games_top_countries()].
#'
#' @param steam_app_id Integer. The Steam App ID of the game.
#' @param version API generation. `"v4"` (default) reads the Steam row of
#'   the multi-game v4 `/player-insights/games/top-countries` endpoint; `"v3"`
#'   reads `/player-insights/games/{steamAppId}/top-countries`. Both return the same
#'   figures.
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A [tibble][tibble::tibble] with columns `country` (ISO code),
#'   `country_name`, `percentage` and `rank`, sorted by rank.
#'
#' @export
#' @examples
#' \dontrun{
#' vgi_top_countries(steam_app_id = 4019220)
#' }
vgi_top_countries <- function(steam_app_id,
                              auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                              headers = list(),
                              version = c("v4", "v3")) {

  validate_numeric(steam_app_id, "steam_app_id")

  version <- .vgi_api_version(match.arg(version))
  result <- if (version == "v4") {
    .vgi_v4_game_record("player-insights/games/top-countries", steam_app_id,
                        auth_token = auth_token, headers = headers)
  } else {
    make_api_request(
      endpoint = sprintf("player-insights/games/%s/top-countries", as.integer(steam_app_id)),
      auth_token = auth_token,
      method = "GET",
      headers = headers,
      version = "v3"
    )
  }

  .vgi_country_rows(if (is.list(result)) result$topCountries else NULL)
}

# Shared shaping for the country breakdown endpoints (players and wishlists).
.vgi_country_rows <- function(df) {
  if (!is.data.frame(df) || nrow(df) == 0) {
    return(.vgi_clean_names(tibble::tibble(
      country = character(), countryName = character(), percentage = numeric(), rank = integer()
    )))
  }
  out <- tibble::tibble(
    country = as.character(.vgi_col(df, "countryCode", NA_character_)),
    countryName = as.character(.vgi_col(df, "countryName", NA_character_)),
    percentage = as.numeric(.vgi_col(df, "percentage")),
    rank = as.integer(.vgi_col(df, "rank", NA_integer_))
  )
  .vgi_clean_names(out[order(out$rank), , drop = FALSE])
}
