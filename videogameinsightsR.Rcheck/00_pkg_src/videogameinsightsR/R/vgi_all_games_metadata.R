#' Get Metadata for All Games
#'
#' Retrieve comprehensive metadata for all games in the database, providing
#' essential information for game identification and categorization.
#'
#' @param limit Integer. Maximum number of games to return (default 1000).
#'   Use NULL to return all games (may be very large).
#' @param offset Integer. Number of games to skip for pagination (default 0).
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{steamAppId}{Integer. The Steam App ID}
#'   \item{name}{Character. Game title}
#'   \item{releaseDate}{Character. Release date}
#'   \item{developer}{Character. Primary developer name}
#'   \item{publisher}{Character. Primary publisher name}
#'   \item{genres}{List. Game genres}
#'   \item{tags}{List. Steam tags}
#'   \item{price}{Numeric. Current price in USD}
#'   \item{description}{Character. Game description}
#' }
#'
#' @details
#' This endpoint provides the foundation for:
#' \itemize{
#'   \item Building game catalogs and databases
#'   \item Genre and tag analysis
#'   \item Release pattern studies
#'   \item Price point analysis
#'   \item Developer/publisher relationships
#' }
#' 
#' Note: This endpoint may return a very large dataset. Consider using
#' pagination or caching the results for repeated use.
#'
#' @export
#' @examples
#' \dontrun{
#' # Get first 1000 games
#' games_metadata <- vgi_all_games_metadata(limit = 1000)
#' 
#' # Basic statistics
#' nrow(games_metadata)
#' range(as.Date(games_metadata$releaseDate), na.rm = TRUE)
#' 
#' # Price analysis
#' summary(games_metadata$price)
#' 
#' # Free vs paid games
#' free_games <- sum(games_metadata$price == 0, na.rm = TRUE)
#' free_share <- free_games / nrow(games_metadata)
#' c(free_games = free_games, free_share = free_share)
#' 
#' # Genre analysis
#' all_genres <- unlist(games_metadata$genres)
#' genre_counts <- sort(table(all_genres), decreasing = TRUE)
#' head(genre_counts, 10)
#' 
#' # Tag analysis for trends
#' all_tags <- unlist(games_metadata$tags)
#' tag_counts <- sort(table(all_tags), decreasing = TRUE)
#' head(tag_counts, 20)
#' 
#' # Release patterns by year
#' games_metadata$year <- format(as.Date(games_metadata$releaseDate), "%Y")
#' yearly_releases <- table(games_metadata$year)
#' 
#' barplot(yearly_releases[names(yearly_releases) >= "2015"],
#'         main = "Games Released per Year",
#'         xlab = "Year",
#'         ylab = "Number of Games",
#'         las = 2,
#'         col = "steelblue")
#' }
vgi_all_games_metadata <- function(limit = 1000,
                                  offset = 0,
                                  auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                                  headers = list()) {
  
  # Validate inputs
  if (!is.null(limit)) {
    validate_numeric(limit, "limit", min_val = 1)
  }
  validate_numeric(offset, "offset", min_val = 0)
  
  # Build query parameters
  query_params <- list(cursor = offset)
  if (!is.null(limit)) {
    query_params$limit <- limit
  }
  
  # Make API request
  result <- make_api_request(
    endpoint = "games/metadata",
    query_params = query_params,
    auth_token = auth_token,
    method = "GET",
    headers = headers
  )
  
  rows <- .vgi_unwrap_results(result)
  if (!is.data.frame(rows) || nrow(rows) == 0) {
    return(.vgi_clean_names(data.frame(
      steamAppId = integer(),
      name = character(),
      releaseDate = character(),
      developer = character(),
      publisher = character(),
      genres = I(list()),
      tags = I(list()),
      price = numeric(),
      description = character(),
      stringsAsFactors = FALSE
    )))
  }

  df <- do.call(rbind, lapply(seq_len(nrow(rows)), function(i) {
    row <- rows[i, , drop = FALSE]
    steam_id <- .vgi_parse_steam_app_id(row$storeUrl.steam)
    dev_name <- NA_character_
    pub_name <- NA_character_
    if ("developers" %in% names(row) && is.data.frame(row$developers[[1]]) && nrow(row$developers[[1]]) > 0) {
      dev_name <- as.character(row$developers[[1]]$companyName[1] %||% NA_character_)
    }
    if ("publishers" %in% names(row) && is.data.frame(row$publishers[[1]]) && nrow(row$publishers[[1]]) > 0) {
      pub_name <- as.character(row$publishers[[1]]$companyName[1] %||% NA_character_)
    }
    data.frame(
      steamAppId = as.integer(steam_id),
      name = as.character(row$name %||% NA_character_),
      releaseDate = as.character(row$releaseDate.steam %||% row$steamFullReleaseDate %||% NA_character_),
      developer = dev_name,
      publisher = pub_name,
      genres = I(list(unlist(row$genre[[1]] %||% character(0)))),
      tags = I(list(unlist(row$steamTags[[1]] %||% character(0)))),
      price = as.numeric(row$price.steam %||% NA),
      description = as.character(NA),
      stringsAsFactors = FALSE
    )
  }))

  df <- df[!is.na(df$steamAppId), , drop = FALSE]
  .vgi_clean_names(df)
}
