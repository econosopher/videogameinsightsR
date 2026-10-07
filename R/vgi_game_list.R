#' Get the Full Game List
#'
#' Retrieve the catalogue of games known to VGI from `/games/game-list`. The
#' v4 endpoint (default) returns the VGI ID alongside the Steam App ID; the v3
#' endpoint returns Steam App IDs and names only. Both responses are large
#' (several MB) and unpaginated; consider caching the result.
#'
#' @param version `"v4"` (default) or `"v3"`.
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A [tibble][tibble::tibble] with columns `steam_app_id`, `name`,
#'   `id` (alias of `steam_app_id`, kept for backwards compatibility) and
#'   (v4) `vgi_id`, sorted by Steam App ID.
#'
#' @export
#' @examples
#' \dontrun{
#' games <- vgi_game_list()
#' nrow(games)
#' }
vgi_game_list <- function(version = c("v4", "v3"),
                          auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                          headers = list()) {
  version <- match.arg(version)
  if (interactive() || isTRUE(getOption("vgi.verbose", FALSE))) {
    message("Note: This endpoint returns ALL games and may take some time. Consider caching the results.")
  }

  result <- make_api_request(
    endpoint = "games/game-list",
    auth_token = auth_token, method = "GET", headers = headers, version = version
  )

  if (!is.data.frame(result) || nrow(result) == 0) {
    return(.vgi_clean_names(tibble::tibble(steamAppId = integer(), name = character(), id = integer())))
  }
  df <- tibble::tibble(
    steamAppId = as.integer(.vgi_col(result, c("steamAppId", "id"), NA_integer_)),
    name = as.character(.vgi_col(result, "name", NA_character_))
  )
  if ("vgiId" %in% names(result)) df$vgiId <- as.integer(result$vgiId)
  df$id <- df$steamAppId
  df <- df[order(df$steamAppId), , drop = FALSE]
  warn_if_stale_ids(df$steamAppId)
  .vgi_clean_names(df)
}
