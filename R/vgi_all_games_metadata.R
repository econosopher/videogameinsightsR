#' Get Metadata for Many Games (v3 catalogue pages)
#'
#' Page through the v3 `/games/metadata` catalogue with `offset` / `limit`.
#' The rows have the same shape as [vgi_game_metadata()]. For identifier-based
#' selection or multi-platform fields use [vgi_games_metadata()] (v4).
#'
#' @param limit Integer. Number of games to return (API default 5, maximum 1000).
#' @param offset Integer. Number of games to skip.
#' @param all_pages Logical. Walk the whole catalogue in pages of `limit`.
#'   This is a large download (tens of thousands of games).
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A [tibble][tibble::tibble] with one row per game; see
#'   [vgi_game_metadata()] for the columns.
#'
#' @export
#' @examples
#' \dontrun{
#' vgi_all_games_metadata(limit = 100)
#' vgi_all_games_metadata(limit = 100, offset = 100)
#' }
vgi_all_games_metadata <- function(limit = 1000,
                                  offset = 0,
                                  all_pages = FALSE,
                                  auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                                  headers = list()) {
  validate_numeric(limit, "limit", min_val = 1, max_val = 1000)
  validate_numeric(offset, "offset", min_val = 0)

  rows <- .vgi_fetch_v3_pages(
    endpoint = "games/metadata",
    query_params = list(offset = as.integer(offset), limit = as.integer(limit)),
    auth_token = auth_token, headers = headers,
    all_pages = all_pages, page_size = as.integer(limit)
  )
  if (!is.data.frame(rows) || nrow(rows) == 0) {
    return(.vgi_clean_names(tibble::tibble(steamAppId = integer(), name = character())))
  }
  dplyr::bind_rows(lapply(seq_len(nrow(rows)), function(i) {
    .vgi_metadata_row_v3(as.list(rows[i, , drop = FALSE]) |> lapply(function(x) if (is.list(x) && !is.data.frame(x)) x[[1]] else x))
  }))
}
