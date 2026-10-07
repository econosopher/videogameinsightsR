#' Get Game IDs for Publishers
#'
#' Retrieve the game IDs attached to each publisher. The v4
#' `/companies/publishers/game-ids` endpoint (default) returns VGI game IDs
#' and supports selection by company ID or slug; the v3 `/publishers/game-ids`
#' endpoint returns Steam App IDs with offset paging.
#'
#' @param vgi_ids Integer vector. VGI company IDs to select (v4 only).
#' @param slugs Character vector. VGI company slugs to select (v4 only).
#' @param limit Integer. Records per page (maximum 1000).
#' @param cursor Integer. Cursor from a previous page (v4 only).
#' @param offset Integer. Records to skip (v3 only).
#' @param all_pages Logical. Fetch every page.
#' @param version `"v4"` (default) or `"v3"`.
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A [tibble][tibble::tibble] with columns `publisher_id`, `name`
#'   (v4), `game_ids` (list-column of VGI IDs for v4, Steam App IDs for v3),
#'   `id_type` ("vgi_id" or "steam_app_id") and `game_count`, sorted by game
#'   count. The attribute `next_cursor` carries the v4 cursor.
#'
#' @export
#' @examples
#' \dontrun{
#' vgi_all_publisher_games(vgi_ids = 28663)
#' vgi_all_publisher_games(version = "v3", limit = 50)
#' }
vgi_all_publisher_games <- function(auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                                    headers = list(),
                                    vgi_ids = NULL,
                                    slugs = NULL,
                                    limit = NULL,
                                    cursor = NULL,
                                    offset = NULL,
                                    all_pages = FALSE,
                                    version = c("v4", "v3")) {
  .vgi_company_game_ids("publishers", "publisherId", vgi_ids, slugs, limit, cursor, offset,
                        all_pages, match.arg(version), auth_token, headers)
}

.vgi_company_game_ids <- function(kind, id_col, vgi_ids, slugs, limit, cursor, offset,
                                  all_pages, version, auth_token, headers) {
  next_cursor <- NULL
  if (version == "v3") {
    if (!is.null(offset)) validate_numeric(offset, "offset", min_val = 0)
    if (!is.null(limit)) validate_numeric(limit, "limit", min_val = 1, max_val = 1000)
    qp <- list()
    if (!is.null(offset)) qp$offset <- as.integer(offset)
    if (!is.null(limit)) qp$limit <- as.integer(limit)
    rows <- .vgi_fetch_v3_pages(sprintf("%s/game-ids", kind), qp, auth_token = auth_token,
                                headers = headers, all_pages = all_pages,
                                page_size = as.integer(limit %||% 1000))
    company_ids <- if (is.data.frame(rows)) rows$companyId else NULL
    names_col <- rep(NA_character_, NROW(rows))
    id_lists <- if (is.data.frame(rows)) rows$steamAppIds else list()
    id_type <- "steam_app_id"
  } else {
    qp <- .vgi_v4_query(vgi_ids = vgi_ids, slugs = slugs, limit = limit, cursor = cursor)
    page <- .vgi_fetch_v4_pages(sprintf("companies/%s/game-ids", kind), qp, auth_token = auth_token,
                                headers = headers, all_pages = all_pages)
    rows <- page$results
    next_cursor <- page$next_cursor
    company_ids <- if (is.data.frame(rows)) rows$vgiCompanyId else NULL
    names_col <- as.character(.vgi_col(rows, "name", NA_character_))
    id_lists <- if (is.data.frame(rows)) rows$vgiGameIds else list()
    id_type <- "vgi_id"
  }

  if (is.null(company_ids) || length(company_ids) == 0) {
    out <- tibble::tibble(id = integer(), name = character(), gameIds = I(list()),
                          idType = character(), gameCount = integer())
    names(out)[1] <- id_col
    out <- .vgi_clean_names(out)
    attr(out, "next_cursor") <- next_cursor
    return(out)
  }
  game_ids <- lapply(id_lists, function(x) as.integer(unlist(x)))
  out <- tibble::tibble(
    id = as.integer(company_ids),
    name = names_col,
    gameIds = I(game_ids),
    idType = id_type,
    gameCount = vapply(game_ids, length, integer(1))
  )
  names(out)[1] <- id_col
  out <- .vgi_clean_names(out[order(-out$gameCount), , drop = FALSE])
  attr(out, "next_cursor") <- next_cursor
  out
}
