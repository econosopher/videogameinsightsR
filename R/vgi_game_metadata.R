#' Get Game Metadata (v3, single Steam game)
#'
#' Retrieves metadata for one game from the v3 `/games/{steamAppId}/metadata`
#' endpoint. For multi-platform metadata, VGI IDs or slugs, or many games in
#' one call, use [vgi_games_metadata()] (v4).
#'
#' @param steam_app_id Character or numeric. The Steam App ID of the game.
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A one-row [tibble][tibble::tibble] with columns `steam_app_id`, `id`
#'   (alias), `name`, `price`, `release_date`, `full_release_date`, `genres`,
#'   `subgenres`, `languages`, `publisher_classification`, `publishing_type`,
#'   `vgi_url`, `steam_url`, `publisher_id`, `publisher_name`, `developer_id`,
#'   `developer_name`, and list-columns `steam_tags`, `game_modes`, `themes`,
#'   `art_styles`, `character`, `developers`, `publishers`. An empty tibble is
#'   returned when the API has no such game.
#'
#' @examples
#' \dontrun{
#' vgi_game_metadata(4019220)$name
#' }
#'
#' @export
vgi_game_metadata <- function(steam_app_id,
                             auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                             headers = list()) {

  if (is.null(steam_app_id) || identical(steam_app_id, "")) stop("steam_app_id is required")
  steam_app_id <- suppressWarnings(as.integer(as.character(steam_app_id)))
  if (is.na(steam_app_id)) stop("steam_app_id must be numeric")

  row <- tryCatch(
    make_api_request(
      endpoint = sprintf("games/%s/metadata", steam_app_id),
      auth_token = auth_token,
      headers = headers,
      version = "v3"
    ),
    vgi_http_error = function(e) {
      if (identical(e$status, 404L)) return(NULL)
      rlang::cnd_signal(e)
    }
  )

  if (!is.list(row) || length(row) == 0 || is.null(row$steamAppId)) {
    return(.vgi_clean_names(tibble::tibble()))
  }
  .vgi_metadata_row_v3(row)
}

# Shape one v3 metadata object (a named list from jsonlite) into a tibble row.
.vgi_metadata_row_v3 <- function(row) {
  scalar <- function(nm, type = as.character) {
    v <- row[[nm]]
    if (is.null(v) || length(v) == 0) return(type(NA))
    type(v[[1]])
  }
  company <- function(nm) {
    df <- row[[nm]]
    if (is.data.frame(df) && nrow(df) > 0) {
      list(id = as.integer(df$companyId[1]), name = as.character(df$companyName[1]), df = tibble::as_tibble(df))
    } else {
      list(id = NA_integer_, name = NA_character_, df = tibble::tibble(companyId = integer(), companyName = character(), vgiUrl = character()))
    }
  }
  pub <- company("publishers")
  dev <- company("developers")
  list_col <- function(nm) I(list(as.character(unlist(row[[nm]]))))

  result <- tibble::tibble(
    steamAppId = scalar("steamAppId", as.integer),
    id = scalar("steamAppId", as.integer),
    name = scalar("name"),
    price = scalar("price", as.numeric),
    releaseDate = scalar("releaseDate"),
    fullReleaseDate = scalar("fullReleaseDate"),
    genres = scalar("genres"),
    subgenres = scalar("subgenres"),
    languages = scalar("languages"),
    publisherClassification = scalar("publisherClassification"),
    publishingType = scalar("publishingType"),
    vgiUrl = scalar("vgiUrl"),
    steamUrl = scalar("steamUrl"),
    publisherId = pub$id,
    publisherName = pub$name,
    developerId = dev$id,
    developerName = dev$name,
    steamTags = list_col("steamTags"),
    gameModes = list_col("gameModes"),
    themes = list_col("themes"),
    artStyles = list_col("artStyles"),
    character = list_col("character"),
    developers = I(list(.vgi_clean_names(dev$df))),
    publishers = I(list(.vgi_clean_names(pub$df)))
  )
  .vgi_clean_names(result)
}
