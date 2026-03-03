#' Get Follower Data for a Game
#'
#' Retrieve historical follower data for a specific game on Steam, showing how many users
#' follow the game over time.
#'
#' @param steam_app_id Integer. The Steam App ID of the game.
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A list containing:
#' \describe{
#'   \item{steamAppId}{Integer. The Steam App ID}
#'   \item{followersChange}{Data frame with columns:
#'     \itemize{
#'       \item date: Date of the data point
#'       \item followersTotal: Total number of followers on that date
#'       \item followersChange: Change in followers from previous period
#'     }
#'   }
#' }
#'
#' @details
#' Follower data indicates community engagement and interest:
#' \itemize{
#'   \item Followers receive updates about the game in their Steam activity feed
#'   \item High follower counts suggest strong community interest
#'   \item Follower growth often correlates with marketing effectiveness
#'   \item Pre-launch follower counts can predict initial sales
#' }
#'
#' @export
#' @examples
#' \dontrun{
#' # Get follower data for a game
#' followers <- vgi_insights_followers(steam_app_id = 892970)
#' 
#' # Display current followers
#' current_followers <- tail(followers$followersChange, 1)$followersTotal
#' print(paste("Current followers:", format(current_followers, big.mark = ",")))
#' 
#' # Calculate growth rate
#' if (nrow(followers$followersChange) >= 7) {
#'   week_ago <- followers$followersChange[nrow(followers$followersChange) - 6, ]
#'   weekly_growth <- current_followers - week_ago$followersTotal
#'   print(paste("Weekly growth:", format(weekly_growth, big.mark = ",")))
#' }
#' 
#' # Plot follower totals and daily changes
#' old_par <- par(no.readonly = TRUE)
#' par(mfrow = c(2, 1))
#' plot(followers$followersChange$date, followers$followersChange$followersTotal,
#'      type = "l", col = "darkgreen", lwd = 2,
#'      main = "Follower Growth Over Time",
#'      xlab = "Date", ylab = "Total Followers")
#' 
#' # Daily changes as bars
#' barplot(followers$followersChange$followersChange,
#'         col = ifelse(followers$followersChange$followersChange > 0, 
#'                      "lightgreen", "lightcoral"),
#'         border = NA, xlab = "Observation", ylab = "Daily Change")
#' par(old_par)
#' }
vgi_insights_followers <- function(steam_app_id,
                                 auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                                 headers = list()) {
  
  validate_numeric(steam_app_id, "steam_app_id")
  
  hist <- vgi_historical_data(steam_app_id, auth_token = auth_token, headers = headers)
  
  fol <- hist$followers
  if (is.null(fol) || nrow(fol) == 0) {
    changes_df <- data.frame(
      date = as.Date(character()), followersTotal = integer(),
      followersChange = integer(), stringsAsFactors = FALSE
    )
  } else {
    changes_df <- data.frame(
      date = as.Date(fol$date),
      followersTotal = as.integer(fol$followers),
      followersChange = c(NA_integer_, diff(as.integer(fol$followers))),
      stringsAsFactors = FALSE
    )
  }
  
  .vgi_clean_list(list(steamAppId = as.integer(steam_app_id), followersChange = changes_df))
}
