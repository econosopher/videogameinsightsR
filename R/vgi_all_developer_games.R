#' Get Game IDs for Developers
#'
#' Retrieve the VGI game IDs attached to each developer from
#' `/companies/developers/game-ids`. Games on every platform are included; use
#' [vgi_developer_games()] for one developer's Steam App IDs.
#'
#' @inheritParams vgi_all_publisher_games
#' @return A [tibble][tibble::tibble] with columns `developer_id`, `name`,
#'   `game_ids` (list-column of VGI game IDs) and `game_count`; see
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
                                    all_pages = FALSE) {
  .vgi_company_game_ids("developers", "developerId", vgi_ids, slugs, limit, cursor,
                        all_pages, auth_token, headers)
}
