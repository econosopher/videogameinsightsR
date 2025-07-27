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
#' cat("Total games:", nrow(games_metadata), "\n")
#' cat("Date range:", min(games_metadata$releaseDate), "to", 
#'     max(games_metadata$releaseDate), "\n")
#' 
#' # Price analysis
#' price_stats <- summary(games_metadata$price)
#' print(price_stats)
#' 
#' # Free vs paid games
#' free_games <- sum(games_metadata$price == 0)
#' cat("Free games:", free_games, 
#'     "(", round(free_games/nrow(games_metadata)*100, 1), "%)\n")
#' 
#' # Genre analysis
#' all_genres <- unlist(games_metadata$genres)
#' genre_counts <- sort(table(all_genres), decreasing = TRUE)
#' cat("Top 10 genres:\n")
#' print(head(genre_counts, 10))
#' 
#' # Tag analysis for trends
#' all_tags <- unlist(games_metadata$tags)
#' tag_counts <- sort(table(all_tags), decreasing = TRUE)
#' cat("Top 20 tags:\n")
#' print(head(tag_counts, 20))
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
#' 
#' # Developer analysis
#' dev_counts <- sort(table(games_metadata$developer), decreasing = TRUE)
#' cat("Most prolific developers:\n")
#' print(head(dev_counts, 10))
#' 
#' # Price tier analysis
#' games_metadata$price_tier <- cut(games_metadata$price,
#'                                  breaks = c(-0.01, 0, 9.99, 19.99, 
#'                                            39.99, 59.99, Inf),
#'                                  labels = c("Free", "<$10", "$10-20", 
#'                                            "$20-40", "$40-60", ">$60"))
#' 
#' tier_dist <- table(games_metadata$price_tier)
#' pie(tier_dist,
#'     main = "Games by Price Tier",
#'     col = rainbow(length(tier_dist)))
#' 
#' # Find games by specific criteria
#' # Recent indie games
#' recent_indie <- games_metadata[
#'   games_metadata$year >= "2023" & 
#'   sapply(games_metadata$tags, function(t) "Indie" %in% t) &
#'   games_metadata$price < 30,
#' ]
#' cat("Recent indie games under $30:", nrow(recent_indie), "\n")
#' 
#' # Export for external analysis
#' # write.csv(games_metadata, "all_games_metadata.csv", row.names = FALSE)
#' 
#' # Paginated retrieval for large datasets
#' # all_games <- list()
#' # offset <- 0
#' # repeat {
#' #   batch <- vgi_all_games_metadata(limit = 1000, offset = offset)
#' #   if (nrow(batch) == 0) break
#' #   all_games[[length(all_games) + 1]] <- batch
#' #   offset <- offset + 1000
#' #   cat("Retrieved", offset, "games...\n")
#' # }
#' # all_games_df <- do.call(rbind, all_games)
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
  query_params <- list(offset = offset)
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
  
  # Convert to data frame
  if (is.list(result) && length(result) > 0) {
    df <- do.call(rbind, lapply(result, function(x) {
      data.frame(
        steamAppId = as.integer(x$steamAppId %||% NA),
        name = as.character(x$name %||% NA),
        releaseDate = as.character(x$releaseDate %||% NA),
        developer = as.character(x$developer %||% NA),
        publisher = as.character(x$publisher %||% NA),
        genres = I(list(unlist(x$genres))),
        tags = I(list(unlist(x$tags))),
        price = as.numeric(x$price %||% 0),
        description = as.character(x$description %||% NA),
        stringsAsFactors = FALSE
      )
    }))
    
    return(df)
  } else {
    # Return empty data frame with correct structure
    return(data.frame(
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
    ))
  }
}