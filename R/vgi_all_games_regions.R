#' Get Regional Distribution Data for All Games
#'
#' Retrieve regional player distribution for all games, showing how players
#' are distributed across major world regions.
#'
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{steamAppId}{Integer. The Steam App ID}
#'   \item{northAmerica}{Numeric. Percentage of players from North America}
#'   \item{europe}{Numeric. Percentage of players from Europe}
#'   \item{asia}{Numeric. Percentage of players from Asia}
#'   \item{southAmerica}{Numeric. Percentage of players from South America}
#'   \item{oceania}{Numeric. Percentage of players from Oceania}
#'   \item{africa}{Numeric. Percentage of players from Africa}
#'   \item{middleEast}{Numeric. Percentage of players from Middle East}
#'   \item{dominantRegion}{Character. Region with highest player percentage}
#' }
#'
#' @details
#' Regional data provides high-level insights for:
#' \itemize{
#'   \item Global market strategy
#'   \item Server infrastructure planning
#'   \item Marketing budget allocation
#'   \item Content scheduling (time zones)
#'   \item Localization priorities
#' }
#' 
#' Regions are aggregated from individual country data using
#' standard geographic classifications.
#'
#' @export
#' @examples
#' \dontrun{
#' # Get regional data for all games
#' regions_data <- vgi_all_games_regions()
#' 
#' # Find games by dominant region
#' dominant_regions <- table(regions_data$dominantRegion)
#' print(dominant_regions)
#' 
#' # Visualize regional distribution
#' pie(dominant_regions,
#'     main = "Games by Dominant Region",
#'     col = rainbow(length(dominant_regions)))
#' 
#' # Find globally balanced games
#' regions_data$max_region_pct <- pmax(regions_data$northAmerica,
#'                                     regions_data$europe,
#'                                     regions_data$asia,
#'                                     regions_data$southAmerica,
#'                                     regions_data$oceania,
#'                                     regions_data$africa,
#'                                     regions_data$middleEast)
#' 
#' balanced_games <- regions_data[regions_data$max_region_pct < 40, ]
#' cat("Games with no region >40%:", nrow(balanced_games), "\n")
#' 
#' # Compare Western vs Eastern games
#' regions_data$western_pct <- regions_data$northAmerica + 
#'                            regions_data$europe + 
#'                            regions_data$oceania
#' regions_data$eastern_pct <- regions_data$asia + 
#'                            regions_data$middleEast
#' 
#' western_games <- regions_data[regions_data$western_pct > 70, ]
#' eastern_games <- regions_data[regions_data$eastern_pct > 70, ]
#' 
#' cat("Western-dominated games (>70%):", nrow(western_games), "\n")
#' cat("Eastern-dominated games (>70%):", nrow(eastern_games), "\n")
#' 
#' # Analyze emerging markets
#' emerging_markets <- regions_data$southAmerica + 
#'                    regions_data$africa + 
#'                    regions_data$middleEast
#' 
#' emerging_focused <- regions_data[emerging_markets > 30, ]
#' cat("Games with >30% from emerging markets:", nrow(emerging_focused), "\n")
#' 
#' # Create regional profile heatmap (requires additional packages)
#' # library(ggplot2)
#' # library(reshape2)
#' # 
#' # top_100 <- head(regions_data, 100)
#' # regions_matrix <- top_100[, c("steamAppId", "northAmerica", "europe", 
#' #                              "asia", "southAmerica", "oceania", 
#' #                              "africa", "middleEast")]
#' # regions_long <- melt(regions_matrix, id.vars = "steamAppId")
#' # 
#' # ggplot(regions_long, aes(x = variable, y = steamAppId, fill = value)) +
#' #   geom_tile() +
#' #   scale_fill_gradient(low = "white", high = "darkblue") +
#' #   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#' #   labs(title = "Regional Distribution Heatmap (Top 100 Games)",
#' #        x = "Region", y = "Game", fill = "Player %")
#' 
#' # Find region-specific genres (would need genre data)
#' # Asia-focused games might be more likely to be MMOs or mobile ports
#' asia_focused <- regions_data[regions_data$asia > 50, ]
#' cat("Asia-focused games (>50% Asian players):", nrow(asia_focused), "\n")
#' }
vgi_all_games_regions <- function(auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                                 headers = list()) {
  
  # Make API request
  result <- make_api_request(
    endpoint = "player-insights/games/top-regions",
    auth_token = auth_token,
    method = "GET",
    headers = headers
  )

  rows <- .vgi_unwrap_results(result)
  if (!is.data.frame(rows) || nrow(rows) == 0) {
    return(data.frame(
      steamAppId = integer(),
      northAmerica = numeric(),
      europe = numeric(),
      asia = numeric(),
      southAmerica = numeric(),
      oceania = numeric(),
      africa = numeric(),
      middleEast = numeric(),
      dominantRegion = character(),
      stringsAsFactors = FALSE
    ))
  }

  map_region <- function(region_name) {
    key <- tolower(region_name %||% "")
    if (key == "north america") return("northAmerica")
    if (key == "south america") return("southAmerica")
    if (key == "middle east") return("middleEast")
    if (key %in% c("europe", "asia", "oceania", "africa")) return(key)
    NULL
  }

  df <- do.call(rbind, lapply(seq_len(nrow(rows)), function(i) {
    reg_df <- if ("topRegions" %in% names(rows)) rows$topRegions[[i]] else NULL
    region_vals <- c(
      northAmerica = 0, europe = 0, asia = 0, southAmerica = 0,
      oceania = 0, africa = 0, middleEast = 0
    )

    if (is.data.frame(reg_df) && nrow(reg_df) > 0) {
      for (j in seq_len(nrow(reg_df))) {
        key <- map_region(reg_df$regionName[j])
        if (!is.null(key)) {
          region_vals[key] <- as.numeric(reg_df$percentage[j] %||% 0)
        }
      }
    }

    dominant_region <- names(region_vals)[which.max(region_vals)]
    data.frame(
      steamAppId = as.integer(rows$externalId[i] %||% NA),
      northAmerica = as.numeric(region_vals["northAmerica"]),
      europe = as.numeric(region_vals["europe"]),
      asia = as.numeric(region_vals["asia"]),
      southAmerica = as.numeric(region_vals["southAmerica"]),
      oceania = as.numeric(region_vals["oceania"]),
      africa = as.numeric(region_vals["africa"]),
      middleEast = as.numeric(region_vals["middleEast"]),
      dominantRegion = as.character(dominant_region),
      stringsAsFactors = FALSE
    )
  }))

  df <- df[!is.na(df$steamAppId), , drop = FALSE]
  df
}