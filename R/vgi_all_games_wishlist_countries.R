#' Get Top Wishlist Countries for Many Games (v4)
#'
#' Retrieve the wishlist-share-by-country breakdown for many games from the v4
#' `/player-insights/games/top-wishlist-countries` endpoint. For one Steam
#' game use [vgi_top_wishlist_countries()] (v3).
#'
#' @inheritParams vgi_all_games_top_countries
#' @return A [tibble][tibble::tibble] with one row per game: `vgi_id`,
#'   `platform`, `steam_app_id`, `top_wishlist_countries` (list-column of
#'   tibbles with `country`, `country_name`, `percentage`, `rank`),
#'   `wishlist_country_count`, `top_wishlist_country`,
#'   `top_wishlist_country_pct`. The attribute `next_cursor` carries the
#'   cursor for the next page.
#' @export
#' @examples
#' \dontrun{
#' vgi_all_games_wishlist_countries(slugs = "dressmaker")
#' }
vgi_all_games_wishlist_countries <- function(auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                                             headers = list(),
                                             steam_app_ids = NULL,
                                             vgi_ids = NULL,
                                             slugs = NULL,
                                             limit = NULL,
                                             cursor = NULL,
                                             all_pages = FALSE) {
  page <- .vgi_player_insights_pages("top-wishlist-countries", steam_app_ids, vgi_ids, slugs, limit,
                                     cursor, all_pages, auth_token, headers)
  out <- .vgi_nested_country_summary(page$results, "wishlists", "topWishlistCountries",
                                     c("wishlistCountryCount", "topWishlistCountry", "topWishlistCountryPct"))
  attr(out, "next_cursor") <- page$next_cursor
  out
}
