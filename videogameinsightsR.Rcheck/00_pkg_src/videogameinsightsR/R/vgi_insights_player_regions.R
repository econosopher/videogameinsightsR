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
#' }
vgi_insights_player_regions <- function(steam_app_id,
                                      auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                                      headers = list()) {
  
  # Validate inputs
  validate_numeric(steam_app_id, "steam_app_id")
  
  # Make API request to the new endpoint
  result <- make_api_request(
    endpoint = paste0("player-insights/games/", steam_app_id, "/regions"),
    auth_token = auth_token,
    method = "GET",
    headers = headers
  )
  
  # Process the regions array if it exists
  if (!is.null(result$regions) && length(result$regions) > 0) {
    # Convert to data frame
    regions_df <- do.call(rbind, lapply(result$regions, function(x) {
      data.frame(
        regionName = as.character(x$regionName),
        rank = as.integer(x$rank),
        percentage = as.numeric(x$percentage),
        stringsAsFactors = FALSE
      )
    }))
    
    # Sort by rank
    regions_df <- regions_df[order(regions_df$rank), ]
    
    result$regions <- regions_df
  } else {
    # Return empty data frame with correct structure
    result$regions <- data.frame(
      regionName = character(),
      rank = integer(),
      percentage = numeric(),
      stringsAsFactors = FALSE
    )
  }
  
  return(result)
}