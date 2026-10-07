#' Get Developer Overview (v4, multi-platform)
#'
#' Retrieve developer summaries from the v4 `/companies/developers` endpoint,
#' including per-platform revenue, release counts and genre breakdowns.
#'
#' @inheritParams vgi_publishers_overview
#' @inherit vgi_publishers_overview return
#' @export
#' @examples
#' \dontrun{
#' vgi_developers_overview(slugs = "free-lives")
#' }
vgi_developers_overview <- function(vgi_ids = NULL,
                                    slugs = NULL,
                                    cursor = NULL,
                                    limit = 100,
                                    all_pages = FALSE,
                                    auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                                    headers = list()) {
  .vgi_company_overview("developers", vgi_ids, slugs, cursor, limit, all_pages, auth_token, headers)
}
