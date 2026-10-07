#' Get Game Metadata for Many Games (v4, multi-platform)
#'
#' Retrieve metadata from the v4 `/games/metadata` endpoint. Games can be
#' selected by Steam App ID, VGI ID or slug, and the endpoint pages with a
#' cursor. Without any identifiers it lists the catalogue from the cursor.
#'
#' @param steam_app_ids Integer vector. Steam App IDs to select. Optional.
#' @param vgi_ids Integer vector. VGI internal game IDs to select. Optional.
#' @param slugs Character vector. VGI game slugs to select. Optional.
#' @param limit Integer. Games per page (API default 200, maximum 1000).
#' @param cursor Integer. VGI ID after which results start (from the
#'   `next_cursor` attribute of a previous page).
#' @param all_pages Logical. Follow `nextCursor` until every page is fetched.
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A [tibble][tibble::tibble] with one row per game: `vgi_id`, `slug`,
#'   `name`, `steam_app_id` (parsed from the Steam store URL), per-platform
#'   columns `price_steam`, `price_xbox`, `price_playstation`,
#'   `release_date_steam`, `release_date_xbox`, `release_date_playstation`,
#'   `store_url_steam`, `store_url_xbox`, `store_url_playstation`, plus
#'   `steam_full_release_date`, `genre`, `subgenre`, `steam_genres`,
#'   `steam_subgenres`, `languages`, `publisher_classification`,
#'   `publishing_type`, `vgi_url`, `publisher_id`, `publisher_name`,
#'   `developer_id`, `developer_name`, and list-columns `platforms`,
#'   `game_engines`, `steam_tags`, `themes`, `art_styles`, `character`,
#'   `game_modes`, `developers`, `publishers`. The attribute `next_cursor`
#'   carries the cursor for the next page (NULL when exhausted).
#'
#' @export
#' @examples
#' \dontrun{
#' vgi_games_metadata(steam_app_ids = 4019220)
#' vgi_games_metadata(slugs = "dressmaker")$vgi_id
#'
#' # Page through the catalogue 500 games at a time
#' page1 <- vgi_games_metadata(limit = 500)
#' page2 <- vgi_games_metadata(limit = 500, cursor = attr(page1, "next_cursor"))
#' }
vgi_games_metadata <- function(steam_app_ids = NULL,
                               vgi_ids = NULL,
                               slugs = NULL,
                               limit = NULL,
                               cursor = NULL,
                               all_pages = FALSE,
                               auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                               headers = list()) {
  qp <- .vgi_v4_query(steam_app_ids = steam_app_ids, vgi_ids = vgi_ids, slugs = slugs,
                      limit = limit, cursor = cursor)
  page <- .vgi_fetch_v4_pages("games/metadata", qp, auth_token = auth_token,
                              headers = headers, all_pages = all_pages)
  out <- .vgi_metadata_rows_v4(page$results)
  attr(out, "next_cursor") <- page$next_cursor
  out
}

# Shape flattened v4 metadata rows into a tidy tibble.
.vgi_metadata_rows_v4 <- function(rows) {
  if (!is.data.frame(rows) || nrow(rows) == 0) {
    return(.vgi_clean_names(tibble::tibble(
      vgiId = integer(), slug = character(), name = character(), steamAppId = integer()
    )))
  }
  n <- nrow(rows)
  chr <- function(nm) as.character(.vgi_col(rows, nm, NA_character_))
  num <- function(nm) as.numeric(.vgi_col(rows, nm, NA_real_))
  list_col <- function(nm) {
    if (!nm %in% names(rows)) return(I(replicate(n, character(0), simplify = FALSE)))
    I(lapply(rows[[nm]], function(x) as.character(unlist(x))))
  }
  first_company <- function(nm, field, type) {
    if (!nm %in% names(rows)) return(rep(type(NA), n))
    vapply(rows[[nm]], function(df) {
      if (is.data.frame(df) && nrow(df) > 0) type(df[[field]][1]) else type(NA)
    }, type(NA))
  }
  company_tbls <- function(nm) {
    if (!nm %in% names(rows)) return(I(replicate(n, tibble::tibble(), simplify = FALSE)))
    I(lapply(rows[[nm]], function(df) if (is.data.frame(df)) .vgi_clean_names(df) else tibble::tibble()))
  }

  out <- tibble::tibble(
    vgiId = as.integer(num("vgiId")),
    slug = chr("slug"),
    name = chr("name"),
    steamAppId = vapply(chr("storeUrl.steam"), .vgi_parse_steam_app_id, integer(1), USE.NAMES = FALSE),
    priceSteam = num("price.steam"),
    priceXbox = num("price.xbox"),
    pricePlaystation = num("price.playstation"),
    releaseDateSteam = chr("releaseDate.steam"),
    releaseDateXbox = chr("releaseDate.xbox"),
    releaseDatePlaystation = chr("releaseDate.playstation"),
    steamFullReleaseDate = chr("steamFullReleaseDate"),
    genre = chr("genre"),
    subgenre = chr("subgenre"),
    steamGenres = chr("steamGenres"),
    steamSubgenres = chr("steamSubgenres"),
    languages = chr("languages"),
    publisherClassification = chr("publisherClassification"),
    publishingType = chr("publishingType"),
    vgiUrl = chr("vgiUrl"),
    storeUrlSteam = chr("storeUrl.steam"),
    storeUrlXbox = chr("storeUrl.xbox"),
    storeUrlPlaystation = chr("storeUrl.playstation"),
    publisherId = first_company("publishers", "companyId", as.integer),
    publisherName = first_company("publishers", "companyName", as.character),
    developerId = first_company("developers", "companyId", as.integer),
    developerName = first_company("developers", "companyName", as.character),
    platforms = list_col("platforms"),
    gameEngines = list_col("gameEngines"),
    steamTags = list_col("steamTags"),
    themes = list_col("themes"),
    artStyles = list_col("artStyles"),
    character = list_col("character"),
    gameModes = list_col("gameModes"),
    developers = company_tbls("developers"),
    publishers = company_tbls("publishers")
  )
  .vgi_clean_names(out)
}
