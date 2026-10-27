#' Get Metadata for Several Steam Games
#'
#' Retrieves metadata for multiple Steam games from `/games/metadata`, up to
#' 500 IDs per request. When the API rejects a request because one ID is
#' unknown, that chunk is fetched one game at a time.
#'
#' @param steam_app_ids Numeric vector. The Steam App IDs of the games.
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A [tibble][tibble::tibble] with one row per game found, in the shape
#'   of [vgi_games_metadata()]. IDs the API does not know are reported in a
#'   warning.
#'
#' @examples
#' \dontrun{
#' vgi_game_metadata_batch(c(892970, 1245620, 105600))
#' }
#'
#' @export
vgi_game_metadata_batch <- function(steam_app_ids,
                                   auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                                   headers = list()) {
  if (is.null(steam_app_ids) || length(steam_app_ids) == 0) {
    stop("steam_app_ids must be a non-empty vector")
  }
  steam_app_ids <- suppressWarnings(as.integer(as.character(steam_app_ids)))
  if (any(is.na(steam_app_ids))) stop("steam_app_ids contains invalid values")
  steam_app_ids <- unique(steam_app_ids)

  chunks <- split(steam_app_ids, ceiling(seq_along(steam_app_ids) / 500))
  out <- dplyr::bind_rows(lapply(chunks, function(ids) {
    tryCatch(
      vgi_games_metadata(steam_app_ids = ids, limit = 1000, auth_token = auth_token, headers = headers),
      vgi_http_error = function(e) {
        # One unknown ID makes the API reject the whole request with a 404;
        # fall back to one request per game.
        if (!identical(e$status, 404L)) rlang::cnd_signal(e)
        dplyr::bind_rows(lapply(ids, vgi_game_metadata, auth_token = auth_token, headers = headers))
      }
    )
  }))
  if (nrow(out) == 0) out <- .vgi_metadata_rows_v4(data.frame())
  out <- out[!is.na(out$steam_app_id) & out$steam_app_id %in% steam_app_ids, , drop = FALSE]
  missing <- setdiff(steam_app_ids, out$steam_app_id)
  if (length(missing) > 0) warning(sprintf("No metadata found for Steam App ID(s): %s", paste(missing, collapse = ", ")))
  attr(out, "next_cursor") <- NULL
  out
}
