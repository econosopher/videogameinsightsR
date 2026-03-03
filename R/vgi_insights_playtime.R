#' Get Playtime Statistics for a Game
#'
#' Retrieve detailed playtime statistics for a specific game, including average and median
#' playtime, ranking information, and distribution across playtime ranges.
#'
#' @param steam_app_id Integer. The Steam App ID of the game.
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A list containing:
#' \describe{
#'   \item{steamAppId}{Integer. The Steam App ID}
#'   \item{avgPlaytime}{Integer. Average lifetime playtime across all users (in minutes)}
#'   \item{medianPlaytime}{Integer. Median lifetime playtime across all users (in minutes)}
#'   \item{avgPlaytimeRank}{Integer. Rank of the game by average playtime}
#'   \item{avgPlaytimePrct}{Numeric. Percentile of average playtime}
#'   \item{playtimeRanges}{Data frame with columns:
#'     \itemize{
#'       \item range: Character string describing the playtime range (e.g., "0", "<2", "2-5", "5-10", etc.)
#'       \item percentage: Percentage of players in this range
#'     }
#'   }
#' }
#'
#' @details
#' The playtime data is returned in minutes. To convert to hours, divide by 60.
#' 
#' Playtime ranges help understand player engagement patterns:
#' \itemize{
#'   \item "0": Players who own but never played
#'   \item "<2": Players who tried briefly (under 2 hours)
#'   \item "2-5": Short engagement
#'   \item "5-10": Moderate engagement
#'   \item "10-20": Good engagement
#'   \item "20-50": Strong engagement
#'   \item "50-100": Very engaged players
#'   \item "100-200": Highly engaged players
#'   \item "200-500": Dedicated players
#'   \item "500+": Extremely dedicated players
#' }
#'
#' @export
#' @examples
#' \dontrun{
#' # Get playtime statistics for a game
#' playtime <- vgi_insights_playtime(steam_app_id = 730)
#' 
#' # Display overall statistics
#' print(paste("Average playtime:", round(playtime$avgPlaytime / 60, 1), "hours"))
#' print(paste("Median playtime:", round(playtime$medianPlaytime / 60, 1), "hours"))
#' print(paste("Playtime rank:", playtime$avgPlaytimeRank))
#' print(paste("Better than", round(100 - playtime$avgPlaytimePrct, 1), "% of games"))
#' 
#' # Visualize playtime distribution
#' if (nrow(playtime$playtimeRanges) > 0) {
#'   barplot(playtime$playtimeRanges$percentage,
#'           names.arg = playtime$playtimeRanges$range,
#'           main = "Player Playtime Distribution",
#'           xlab = "Hours Played",
#'           ylab = "Percentage of Players",
#'           col = "steelblue",
#'           las = 2)
#' }
#' 
#' # Calculate engaged players (20+ hours)
#' engaged_ranges <- c("20-50", "50-100", "100-200", "200-500", "500+")
#' engaged_players <- sum(playtime$playtimeRanges$percentage[
#'   playtime$playtimeRanges$range %in% engaged_ranges
#' ], na.rm = TRUE)
#' print(paste("Engaged players (20+ hours):", round(engaged_players, 1), "%"))
#' }
vgi_insights_playtime <- function(steam_app_id, 
                                auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                                headers = list()) {
  
  # Validate inputs
  validate_numeric(steam_app_id, "steam_app_id")
  
  # v4 playtime endpoint is query-based.
  result <- make_api_request(
    endpoint = "player-insights/games/playtime",
    query_params = list(steamAppIds = as.character(steam_app_id), limit = 1),
    auth_token = auth_token,
    method = "GET",
    headers = headers
  )

  rows <- .vgi_unwrap_results(result)
  empty_result <- .vgi_clean_list(list(
    steamAppId = as.integer(steam_app_id),
    avgPlaytime = NA_real_,
    medianPlaytime = NA_real_,
    avgPlaytimeRank = NA_integer_,
    avgPlaytimePrct = NA_real_,
    playtimeRanges = tibble::tibble(range = character(), percentage = numeric())
  ))
  if (!is.data.frame(rows) || nrow(rows) == 0) return(empty_result)

  row <- .vgi_steam_row(rows, steam_app_id)
  if (is.null(row)) return(empty_result)
  ranges <- if ("playtime" %in% names(row)) row$playtime[[1]] else NULL
  if (!is.data.frame(ranges) || nrow(ranges) == 0) {
    ranges_df <- tibble::tibble(range = character(), percentage = numeric())
  } else {
    ranges_df <- tibble::tibble(
      range = as.character(ranges$range %||% NA_character_),
      percentage = as.numeric(ranges$percentage %||% NA_real_),

    )
  }

  .vgi_clean_list(list(
    steamAppId = as.integer(row$externalId %||% steam_app_id),
    avgPlaytime = as.numeric(row$avgPlaytime %||% NA_real_),
    medianPlaytime = as.numeric(row$medianPlaytime %||% NA_real_),
    avgPlaytimeRank = as.integer(row$avgPlaytimeRank %||% NA_integer_),
    avgPlaytimePrct = as.numeric(row$avgPlaytimePrct %||% NA_real_),
    playtimeRanges = ranges_df
  ))
}
