#' Get Publisher Information
#'
#' Retrieve a publisher's summary from `/companies/publishers`. Revenue and
#' game counts are VGI's Steam figures; release counts are given per platform.
#' For many publishers or the full per-platform breakdown use
#' [vgi_publishers_overview()].
#'
#' @param company_id Integer. The VGI company ID of the publisher.
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A one-row [tibble][tibble::tibble] with columns `company_id`,
#'   `name`, `slug`, `classification`, `country`, `games_published`,
#'   `games_in_development`, `revenue_total`, `revenue_avg_per_game`,
#'   `revenue_median_per_game`, `games_released_steam`,
#'   `games_released_playstation`, `games_released_xbox` and `vgi_url`.
#'   Empty when the publisher is unknown.
#'
#' @export
#' @examples
#' \dontrun{
#' vgi_publisher_info(28663)
#' }
vgi_publisher_info <- function(company_id,
                              auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                              headers = list()) {
  validate_numeric(company_id, "company_id")
  .vgi_company_info("publishers", "gamesPublished", company_id, auth_token, headers)
}

#' Get a Publisher's Steam Games
#'
#' Retrieve the Steam App IDs of the games a company publishes. The VGI game
#' IDs from `/companies/publishers/game-ids` are resolved to Steam App IDs
#' through `/games/metadata`; games that are not on Steam are dropped. For the
#' VGI IDs of games on every platform use [vgi_all_publisher_games()].
#'
#' @inheritParams vgi_publisher_info
#' @return An integer vector of Steam App IDs, sorted (empty when none).
#' @export
#' @examples
#' \dontrun{
#' vgi_publisher_games(28663)
#' }
vgi_publisher_games <- function(company_id,
                               auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                               headers = list()) {
  validate_numeric(company_id, "company_id")
  .vgi_company_steam_games("publishers", company_id, auth_token, headers)
}

# --- shared company helpers ---

.vgi_company_info <- function(kind, games_col, company_id, auth_token, headers) {
  rows <- .vgi_fetch_v4_pages(sprintf("companies/%s", kind),
                              .vgi_v4_query(vgi_ids = as.integer(company_id)),
                              auth_token = auth_token, headers = headers)$results
  if (is.data.frame(rows) && nrow(rows) > 0) {
    rows <- rows[as.integer(rows$vgiCompanyId) == as.integer(company_id), , drop = FALSE]
  }
  steam <- function(metric, type = as.numeric) {
    type(.vgi_col(rows, paste0("platformData.steam.", metric)))
  }
  released <- function(platform) {
    as.integer(.vgi_col(rows, paste0("platformData.", platform, ".gamesReleased"), NA_integer_))
  }
  if (!is.data.frame(rows) || nrow(rows) == 0) {
    out <- tibble::tibble(companyId = integer(), name = character(), slug = character(),
                          classification = character(), country = character())
    out[[games_col]] <- integer()
    for (nm in c("gamesInDevelopment", "gamesReleasedSteam", "gamesReleasedPlaystation", "gamesReleasedXbox")) out[[nm]] <- integer()
    for (nm in c("revenueTotal", "revenueAvgPerGame", "revenueMedianPerGame")) out[[nm]] <- numeric()
    out$vgiUrl <- character()
    return(.vgi_clean_names(out[, c("companyId", "name", "slug", "classification", "country", games_col,
                                    "gamesInDevelopment", "revenueTotal", "revenueAvgPerGame",
                                    "revenueMedianPerGame", "gamesReleasedSteam",
                                    "gamesReleasedPlaystation", "gamesReleasedXbox", "vgiUrl")]))
  }
  out <- tibble::tibble(
    companyId = as.integer(rows$vgiCompanyId),
    name = as.character(.vgi_col(rows, "name", NA_character_)),
    slug = as.character(.vgi_col(rows, "slug", NA_character_)),
    classification = as.character(.vgi_col(rows, "classification", NA_character_)),
    country = as.character(.vgi_col(rows, "country", NA_character_))
  )
  # Publishers carry only gamesReleased; developers also carry gamesDeveloped.
  out[[games_col]] <- as.integer(.vgi_col(rows, paste0("platformData.steam.", c(games_col, "gamesReleased"))))
  out$gamesInDevelopment <- steam("gamesInDevelopment", as.integer)
  out$revenueTotal <- steam("revenueTotal")
  out$revenueAvgPerGame <- steam("revenueAvgPerGame")
  out$revenueMedianPerGame <- steam("revenueMedianPerGame")
  out$gamesReleasedSteam <- released("steam")
  out$gamesReleasedPlaystation <- released("playstation")
  out$gamesReleasedXbox <- released("xbox")
  out$vgiUrl <- as.character(.vgi_col(rows, "vgiUrl", NA_character_))
  .vgi_clean_names(out)
}

.vgi_company_steam_games <- function(kind, company_id, auth_token, headers) {
  ids <- .vgi_company_game_ids(kind, "companyId", as.integer(company_id), NULL, NULL, NULL,
                               FALSE, auth_token, headers)
  ids <- ids[ids$company_id == as.integer(company_id), , drop = FALSE]
  vgi_ids <- unique(unlist(ids$game_ids))
  if (length(vgi_ids) == 0) return(integer())
  steam <- unlist(lapply(split(vgi_ids, ceiling(seq_along(vgi_ids) / 500)), function(chunk) {
    meta <- vgi_games_metadata(vgi_ids = chunk, limit = 1000, auth_token = auth_token, headers = headers)
    meta$steam_app_id
  }))
  sort(unique(as.integer(steam[!is.na(steam)])))
}
