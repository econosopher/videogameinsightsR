#' Get Game Metadata for a Steam Game
#'
#' Retrieves the metadata of one Steam game from `/games/metadata`. The row has
#' the multi-platform shape of [vgi_games_metadata()]: VGI's own `genre` and
#' `subgenre`, Steam's `steam_genres` and `steam_subgenres`, per-platform
#' prices, release dates and store URLs, plus list-columns for tags, modes,
#' themes, developers and publishers.
#'
#' @param steam_app_id Character or numeric. The Steam App ID of the game.
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A one-row [tibble][tibble::tibble] with the columns of
#'   [vgi_games_metadata()]. An empty tibble is returned when the API has no
#'   such game.
#'
#' @examples
#' \dontrun{
#' vgi_game_metadata(4019220)$name
#' }
#'
#' @export
vgi_game_metadata <- function(steam_app_id,
                             auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                             headers = list()) {

  if (is.null(steam_app_id) || identical(steam_app_id, "")) stop("steam_app_id is required")
  steam_app_id <- suppressWarnings(as.integer(as.character(steam_app_id)))
  if (is.na(steam_app_id)) stop("steam_app_id must be numeric")

  out <- tryCatch(
    vgi_games_metadata(steam_app_ids = steam_app_id, auth_token = auth_token, headers = headers),
    vgi_http_error = function(e) {
      # The API answers 404 when it does not know the game.
      if (identical(e$status, 404L)) return(.vgi_metadata_rows_v4(data.frame()))
      rlang::cnd_signal(e)
    }
  )
  out <- out[!is.na(out$steam_app_id) & out$steam_app_id == steam_app_id, , drop = FALSE]
  attr(out, "next_cursor") <- NULL
  out[seq_len(min(1L, nrow(out))), , drop = FALSE]
}
