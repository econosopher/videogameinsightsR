#' Get Top Regions for a Game (v4)
#'
#' Convenience wrapper around the v4 `player-insights/games/top-regions` endpoint.
#'
#' @param steam_app_id Integer Steam app ID.
#' @param auth_token Character string. Your VGI API authentication token.
#' @param headers List. Optional custom headers.
#'
#' @return A data frame with `regionName`, `rank`, and `percentage`.
#' @export
vgi_top_regions <- function(steam_app_id,
                            auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                            headers = list()) {
  out <- vgi_insights_player_regions(
    steam_app_id = steam_app_id,
    auth_token = auth_token,
    headers = headers
  )
  if (is.list(out) && "regions" %in% names(out) && is.data.frame(out$regions)) {
    return(.vgi_clean_names(out$regions))
  }
  .vgi_clean_names(data.frame(
    regionName = character(),
    rank = integer(),
    percentage = numeric(),
    stringsAsFactors = FALSE
  ))
}
