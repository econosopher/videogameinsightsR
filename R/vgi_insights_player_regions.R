#' Get Player Geographic Distribution Data
#'
#' Retrieve the geographic distribution of players for a specific game by region.
#'
#' @param steam_app_id Integer. The Steam App ID of the game.
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A list containing:
#' \describe{
#'   \item{steamAppId}{Integer. The Steam App ID}
#'   \item{regions}{Data frame with columns:
#'     \itemize{
#'       \item regionName: Geographic region (e.g., "Europe", "North America", "Asia")
#'       \item rank: Rank of the region by player percentage (1 = highest)
#'       \item percentage: Percentage of total player base from this region
#'     }
#'   }
#' }
#'
#' @details
#' Understanding player geographic distribution is crucial for:
#' \itemize{
#'   \item Localization decisions - Which languages to prioritize
#'   \item Server placement - Where to host game servers for optimal latency
#'   \item Marketing focus - Which regions to target with advertising
#'   \item Content scheduling - When to release updates based on player timezones
#'   \item Regional pricing - Understanding purchasing power by region
#' }
#'
#' @export
#' @examples
#' \dontrun{
#' # Get player regions for a game
#' regions <- vgi_insights_player_regions(steam_app_id = 730)
#' 
#' # Display regions sorted by rank
#' print(regions$regions)
#' 
#' # Find the top region
#' top_region <- regions$regions[regions$regions$rank == 1, ]
#' print(paste("Top region:", top_region$regionName, 
#'             "with", round(top_region$percentage, 1), "% of players"))
#' 
#' # Create a horizontal bar chart of regions
#' old_par <- par(no.readonly = TRUE)
#' par(mar = c(5, 8, 4, 2))  # Increase left margin for region names
#' barplot(regions$regions$percentage,
#'         names.arg = regions$regions$regionName,
#'         horiz = TRUE,
#'         las = 1,
#'         main = "Player Distribution by Region",
#'         xlab = "Percentage of Players",
#'         col = rainbow(nrow(regions$regions), alpha = 0.8))
#' 
#' # Check geographic diversity
#' top_3_percentage <- sum(head(regions$regions$percentage, 3))
#' print(paste("Top 3 regions account for", 
#'             round(top_3_percentage, 1), "% of players"))
#'             
#' # Identify potential server locations
#' major_regions <- regions$regions[regions$regions$percentage > 10, ]
#' print(paste("Regions with >10% of players:", 
#'             paste(major_regions$regionName, collapse = ", ")))
#' par(old_par)
#' }
vgi_insights_player_regions <- function(steam_app_id,
                                      auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                                      headers = list()) {
  
  # Validate inputs
  validate_numeric(steam_app_id, "steam_app_id")
  
  # v4 top-regions endpoint is query-based.
  result <- make_api_request(
    endpoint = "player-insights/games/top-regions",
    query_params = list(steamAppIds = as.character(steam_app_id), limit = 1),
    auth_token = auth_token,
    method = "GET",
    headers = headers
  )

  rows <- .vgi_unwrap_results(result)
  empty_regions <- .vgi_clean_list(list(
    steamAppId = as.integer(steam_app_id),
    regions = tibble::tibble(
      regionName = character(), rank = integer(), percentage = numeric(),

    )
  ))
  if (!is.data.frame(rows) || nrow(rows) == 0 || !"topRegions" %in% names(rows)) {
    return(empty_regions)
  }

  row <- .vgi_steam_row(rows, steam_app_id)
  if (is.null(row) || !"topRegions" %in% names(row)) return(empty_regions)

  regions_df <- row$topRegions[[1]]
  if (!is.data.frame(regions_df) || nrow(regions_df) == 0) {
    regions_df <- tibble::tibble(
      regionName = character(),
      rank = integer(),
      percentage = numeric(),

    )
  } else {
    regions_df <- tibble::tibble(
      regionName = as.character(regions_df$regionName %||% NA_character_),
      rank = as.integer(regions_df$rank %||% seq_len(nrow(regions_df))),
      percentage = as.numeric(regions_df$percentage %||% NA),

    )
    regions_df <- regions_df[order(regions_df$rank), , drop = FALSE]
  }

  .vgi_clean_list(list(
    steamAppId = as.integer(steam_app_id),
    regions = regions_df
  ))
}
