#' Get Developer Names and IDs
#'
#' Retrieve the developer directory from `/companies/developers/list`, which
#' pages with a cursor and includes slugs.
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
                               auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                               headers = list(),
                               vgi_ids = NULL,
                               slugs = NULL,
                               cursor = NULL,
                               all_pages = FALSE) {
  .vgi_company_directory("developers", search, limit, vgi_ids, slugs, cursor,
                         all_pages, auth_token, headers)
}
