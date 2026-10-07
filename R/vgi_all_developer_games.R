#' Get Game IDs for Developers
#'
#' Retrieve the game IDs attached to each developer. The v4
#' `/companies/developers/game-ids` endpoint (default) returns VGI game IDs
#' and supports selection by company ID or slug; the v3 `/developers/game-ids`
#' endpoint returns Steam App IDs with offset paging.
#'
#' @inheritParams vgi_all_publisher_games
#' @return A [tibble][tibble::tibble] with columns `developer_id`, `name`
#'   (v4), `game_ids` (list-column), `id_type` and `game_count`; see
#'   [vgi_all_publisher_games()].
#' @export
#' @examples
#' \dontrun{
#' vgi_all_developer_games(vgi_ids = 28663)
#' }
vgi_all_developer_games <- function(auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                                    headers = list(),
                                    vgi_ids = NULL,
                                    slugs = NULL,
                                    limit = NULL,
                                    cursor = NULL,
                                    offset = NULL,
                                    all_pages = FALSE,
                                    version = c("v4", "v3")) {
  .vgi_company_game_ids("developers", "developerId", vgi_ids, slugs, limit, cursor, offset,
                        all_pages, match.arg(version), auth_token, headers)
}
