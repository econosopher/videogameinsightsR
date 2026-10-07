#' Get Top Wishlist Countries for a Game
#'
#' Retrieve the countries contributing the largest share of a Steam game's
#' wishlists (v4 `/player-insights/games/top-wishlist-countries` by default,
#' v3 `/player-insights/games/{steamAppId}/top-wishlist-countries` with
#' `version = "v3"`). For many games at once use
#' [vgi_all_games_wishlist_countries()].
#'
#' @inheritParams vgi_top_countries
#' @param version API generation, `"v4"` (default) or `"v3"`; both return
#'   the same figures.
#'
#' @return A [tibble][tibble::tibble] with columns `country` (ISO code),
#'   `country_name`, `percentage` and `rank`, sorted by rank.
#'
#' @export
#' @examples
#' \dontrun{
#' vgi_top_wishlist_countries(steam_app_id = 4019220)
#' }
vgi_top_wishlist_countries <- function(steam_app_id,
                                       version = c("v4", "v3"),
                                       auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                                       headers = list()) {

  validate_numeric(steam_app_id, "steam_app_id")

  version <- .vgi_api_version(match.arg(version))
  result <- if (version == "v4") {
    .vgi_v4_game_record("player-insights/games/top-wishlist-countries", steam_app_id,
                        auth_token = auth_token, headers = headers)
  } else {
    make_api_request(
      endpoint = sprintf("player-insights/games/%s/top-wishlist-countries", as.integer(steam_app_id)),
      auth_token = auth_token,
      method = "GET",
      headers = headers,
      version = "v3"
    )
  }

  .vgi_country_rows(if (is.list(result)) result$wishlists else NULL)
}
