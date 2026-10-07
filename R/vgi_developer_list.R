#' Get Developer Names and IDs
#'
#' Retrieve the developer directory. The v4 `/companies/developers/list`
#' endpoint (default) pages with a cursor and includes slugs; the v3
#' `/developers/developer-list` endpoint returns the whole directory in one
#' (large) response.
#'
#' @inheritParams vgi_publisher_list
#' @inherit vgi_publisher_list return
#' @export
#' @examples
#' \dontrun{
#' vgi_developer_list(search = "free lives")
#' }
vgi_developer_list <- function(search = NULL,
                               limit = NULL,
                               min_games = NULL,
                               auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                               headers = list(),
                               vgi_ids = NULL,
                               slugs = NULL,
                               cursor = NULL,
                               all_pages = FALSE,
                               version = c("v4", "v3")) {
  .vgi_company_directory("developers", search, limit, min_games, vgi_ids, slugs, cursor,
                         all_pages, match.arg(version), auth_token, headers)
}
