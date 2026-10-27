#' Get Game IDs for Publishers
#'
#' Retrieve the VGI game IDs attached to each publisher from
#' `/companies/publishers/game-ids`. Games on every platform are included; use
#' [vgi_publisher_games()] for one publisher's Steam App IDs.
#'
#' @param vgi_ids Integer vector. VGI company IDs to select.
#' @param slugs Character vector. VGI company slugs to select.
#' @param limit Integer. Records per page (maximum 1000).
#' @param cursor Integer. Cursor from a previous page.
#' @param all_pages Logical. Fetch every page.
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A [tibble][tibble::tibble] with columns `publisher_id`, `name`,
#'   `game_ids` (list-column of VGI game IDs) and `game_count`, sorted by game
#'   count. The attribute `next_cursor` carries the cursor for the next page.
#'
#' @export
#' @examples
#' \dontrun{
#' vgi_all_publisher_games(vgi_ids = 28663)
#' }
vgi_all_publisher_games <- function(auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                                    headers = list(),
                                    vgi_ids = NULL,
                                    slugs = NULL,
                                    limit = NULL,
                                    cursor = NULL,
                                    all_pages = FALSE) {
  .vgi_company_game_ids("publishers", "publisherId", vgi_ids, slugs, limit, cursor,
                        all_pages, auth_token, headers)
}

.vgi_company_game_ids <- function(kind, id_col, vgi_ids, slugs, limit, cursor,
                                  all_pages, auth_token, headers) {
  qp <- .vgi_v4_query(vgi_ids = vgi_ids, slugs = slugs, limit = limit, cursor = cursor)
  page <- .vgi_fetch_v4_pages(sprintf("companies/%s/game-ids", kind), qp, auth_token = auth_token,
                              headers = headers, all_pages = all_pages)
  rows <- page$results
  if (!is.data.frame(rows) || nrow(rows) == 0) {
    out <- tibble::tibble(id = integer(), name = character(), gameIds = I(list()),
                          gameCount = integer())
    names(out)[1] <- id_col
    out <- .vgi_clean_names(out)
    attr(out, "next_cursor") <- page$next_cursor
    return(out)
  }
  game_ids <- lapply(rows$vgiGameIds, function(x) as.integer(unlist(x)))
  out <- tibble::tibble(
    id = as.integer(rows$vgiCompanyId),
    name = as.character(.vgi_col(rows, "name", NA_character_)),
    gameIds = I(game_ids),
    gameCount = vapply(game_ids, length, integer(1))
  )
  names(out)[1] <- id_col
  out <- .vgi_clean_names(out[order(-out$gameCount), , drop = FALSE])
  attr(out, "next_cursor") <- page$next_cursor
  out
}
