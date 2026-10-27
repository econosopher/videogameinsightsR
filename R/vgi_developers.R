#' Get Developer Information
#'
#' Retrieve a developer's summary from `/companies/developers`. Revenue and
#' game counts are VGI's Steam figures; release counts are given per platform.
#' For many developers or the full per-platform breakdown use
#' [vgi_developers_overview()].
#'
#' @param company_id Integer. The VGI company ID of the developer.
#' @inheritParams vgi_publisher_info
#'
#' @return A one-row [tibble][tibble::tibble] with columns `company_id`,
#'   `name`, `slug`, `classification`, `country`, `games_developed`,
#'   `games_in_development`, `revenue_total`, `revenue_avg_per_game`,
#'   `revenue_median_per_game`, `games_released_steam`,
#'   `games_released_playstation`, `games_released_xbox` and `vgi_url`.
#'   Empty when the developer is unknown.
#'
#' @export
#' @examples
#' \dontrun{
#' vgi_developer_info(28663)
#' }
vgi_developer_info <- function(company_id,
                              auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                              headers = list()) {
  validate_numeric(company_id, "company_id")
  .vgi_company_info("developers", "gamesDeveloped", company_id, auth_token, headers)
}

#' Get a Developer's Steam Games
#'
#' Retrieve the Steam App IDs of the games a company develops. The VGI game
#' IDs from `/companies/developers/game-ids` are resolved to Steam App IDs
#' through `/games/metadata`; games that are not on Steam are dropped. For the
#' VGI IDs of games on every platform use [vgi_all_developer_games()].
#'
#' @inheritParams vgi_developer_info
#' @return An integer vector of Steam App IDs, sorted (empty when none).
#' @export
#' @examples
#' \dontrun{
#' vgi_developer_games(28663)
#' }
vgi_developer_games <- function(company_id,
                               auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                               headers = list()) {
  validate_numeric(company_id, "company_id")
  .vgi_company_steam_games("developers", company_id, auth_token, headers)
}
