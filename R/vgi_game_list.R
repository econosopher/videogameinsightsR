#' Get the Full Game List
#'
#' Retrieve the catalogue of games known to VGI, on every platform, from
#' `/games/game-list`. The response is large (several MB) and unpaginated;
#' consider caching the result.
#'
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A [tibble][tibble::tibble] with columns `vgi_id`, `steam_app_id`
#'   (NA for games that are not on Steam) and `name`, sorted by VGI ID.
#'
#' @export
#' @examples
#' \dontrun{
#' games <- vgi_game_list()
#' nrow(games)
#' }
vgi_game_list <- function(auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                          headers = list()) {
  if (interactive() || isTRUE(getOption("vgi.verbose", FALSE))) {
    message("Note: This endpoint returns ALL games and may take some time. Consider caching the results.")
  }

  result <- make_api_request(
    endpoint = "games/game-list",
    auth_token = auth_token, method = "GET", headers = headers
  )

  if (!is.data.frame(result) || nrow(result) == 0) {
    return(.vgi_clean_names(tibble::tibble(vgiId = integer(), steamAppId = integer(), name = character())))
  }
  df <- tibble::tibble(
    vgiId = as.integer(.vgi_col(result, "vgiId", NA_integer_)),
    steamAppId = as.integer(.vgi_col(result, "steamAppId", NA_integer_)),
    name = as.character(.vgi_col(result, "name", NA_character_))
  )
  df <- df[order(df$vgiId), , drop = FALSE]
  .vgi_clean_names(df)
}
