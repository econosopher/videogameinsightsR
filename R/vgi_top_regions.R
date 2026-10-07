#' Get Top Regions for a Game
#'
#' Convenience wrapper around [vgi_insights_player_regions()] that returns
#' only the regions tibble.
#'
#' @inheritParams vgi_insights_player_regions
#'
#' @return A [tibble][tibble::tibble] with columns `region_name`, `rank` and
#'   `percentage`.
#'
#' @export
#' @examples
#' \dontrun{
#' vgi_top_regions(steam_app_id = 4019220)
#' }
vgi_top_regions <- function(steam_app_id,
                            auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                            headers = list(),
                            version = c("v4", "v3")) {
  out <- vgi_insights_player_regions(steam_app_id, version = version,
                                     auth_token = auth_token, headers = headers)
  .vgi_clean_names(out$regions)
}
