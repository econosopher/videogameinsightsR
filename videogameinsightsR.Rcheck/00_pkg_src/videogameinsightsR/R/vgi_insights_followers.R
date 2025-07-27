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
#' # Plot follower growth
#' plot(followers$followersChange$date, followers$followersChange$followersTotal,
#'      type = "l", col = "darkgreen", lwd = 2,
#'      main = "Follower Growth Over Time",
#'      xlab = "Date", ylab = "Total Followers")
#' 
#' # Add daily changes as bars
#' par(new = TRUE)
#' barplot(followers$followersChange$followersChange,
#'         col = ifelse(followers$followersChange$followersChange > 0, 
#'                      "lightgreen", "lightcoral"),
#'         border = NA, axes = FALSE, xlab = "", ylab = "")
#' axis(4)
#' mtext("Daily Change", side = 4, line = 3)
#' }
vgi_insights_followers <- function(steam_app_id,
                                 auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                                 headers = list()) {
  
  # Validate inputs
  validate_numeric(steam_app_id, "steam_app_id")
  
  # Make API request
  result <- make_api_request(
    endpoint = paste0("interest-level/followers/games/", steam_app_id),
    auth_token = auth_token,
    method = "GET",
    headers = headers
  )
  
  # Process the followersChange array if it exists
  if (!is.null(result$followersChange) && length(result$followersChange) > 0) {
    # Convert to data frame
    changes_df <- do.call(rbind, lapply(result$followersChange, function(x) {
      data.frame(
        date = as.Date(x$date),
        followersTotal = as.integer(x$followersTotal %||% NA),
        followersChange = as.integer(x$followersChange %||% NA),
        stringsAsFactors = FALSE
      )
    }))
    
    # Sort by date
    changes_df <- changes_df[order(changes_df$date), ]
    
    result$followersChange <- changes_df
  } else {
    # Return empty data frame with correct structure
    result$followersChange <- data.frame(
      date = as.Date(character()),
      followersTotal = integer(),
      followersChange = integer(),
      stringsAsFactors = FALSE
    )
  }
  
  return(result)
}