#' Get Top Wishlist Countries for a Game
#'
#' Retrieve the countries contributing the largest share of a Steam game's
#' wishlists, from v4 `/player-insights/games/top-wishlist-countries`. For many
#' games at once use [vgi_all_games_wishlist_countries()].
#'
#' @inheritParams vgi_top_countries
#' @return A [tibble][tibble::tibble] with columns `country` (ISO code),
#'   `country_name`, `percentage` and `rank`, sorted by rank.
#'
#' @export
#' @examples
#' \dontrun{
#' vgi_top_wishlist_countries(steam_app_id = 4019220)
#' }
vgi_top_wishlist_countries <- function(steam_app_id,
                                       auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                                       headers = list()) {

  validate_numeric(steam_app_id, "steam_app_id")
  result <- .vgi_v4_game_record("player-insights/games/top-wishlist-countries", steam_app_id,
                        auth_token = auth_token, headers = headers)

  .vgi_country_rows(if (is.list(result)) result$wishlists else NULL)
}
