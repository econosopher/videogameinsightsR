#' Get Publisher Information
#'
#' Retrieve detailed information about a specific video game publisher.
#'
#' @param company_id Integer. The publisher's company ID.
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A list containing publisher information with fields:
#' \describe{
#'   \item{companyId}{Integer. The company ID}
#'   \item{name}{Character. Publisher name}
#'   \item{foundedDate}{Character. Date the company was founded}
#'   \item{headquarters}{Character. Company headquarters location}
#'   \item{employeeCount}{Integer. Number of employees}
#'   \item{website}{Character. Company website URL}
#'   \item{description}{Character. Company description}
#'   \item{totalGames}{Integer. Total number of games published}
#'   \item{activeGames}{Integer. Number of currently active games}
#' }
#'
#' @details
#' Publisher information can be used to:
#' \itemize{
#'   \item Research company backgrounds and portfolios
#'   \item Analyze publisher market share
#'   \item Identify publishers specializing in certain genres
#'   \item Track publisher growth and success metrics
#' }
#'
#' @export
#' @examples
#' \dontrun{
#' # Get information about a publisher
#' pub_info <- vgi_publisher_info(company_id = 13190)
#' 
#' # Display publisher details
#' cat("Publisher:", pub_info$name, "\n")
#' cat("Founded:", pub_info$foundedDate, "\n")
#' cat("Total Games Published:", pub_info$totalGames, "\n")
#' cat("Currently Active Games:", pub_info$activeGames, "\n")
#' 
#' # Analyze publisher size
#' if (!is.null(pub_info$employeeCount)) {
#'   if (pub_info$employeeCount > 1000) {
#'     cat("This is a major publisher\n")
#'   } else if (pub_info$employeeCount > 100) {
#'     cat("This is a mid-sized publisher\n")
#'   } else {
#'     cat("This is a boutique publisher\n")
#'   }
#' }
#' }
vgi_publisher_info <- function(company_id,
                              auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                              headers = list()) {
  
  # Validate inputs
  validate_numeric(company_id, "company_id")
  
  # Make API request
  result <- make_api_request(
    endpoint = paste0("publishers/", company_id),
    auth_token = auth_token,
    method = "GET",
    headers = headers
  )
  
  return(result)
}

#' Get Game IDs by Publisher
#'
#' Retrieve a list of Steam App IDs for all games by a specific publisher.
#'
#' @param company_id Integer. The publisher's company ID.
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A numeric vector of Steam App IDs for games published by this company.
#'
#' @details
#' This endpoint now returns only game IDs instead of full game metadata.
#' To get detailed information about each game, use \code{vgi_game_metadata()}
#' with the returned IDs.
#' 
#' This approach is more efficient when you only need to:
#' \itemize{
#'   \item Count the number of games by a publisher
#'   \item Check if a publisher released a specific game
#'   \item Get a subset of games for detailed analysis
#' }
#'
#' @export
#' @examples
#' \dontrun{
#' # Get all game IDs for a publisher
#' game_ids <- vgi_publisher_games(company_id = 13190)
#' 
#' # Count games by this publisher
#' cat("Total games published:", length(game_ids), "\n")
#' 
#' # Get detailed info for the first 5 games
#' if (length(game_ids) > 0) {
#'   first_five <- head(game_ids, 5)
#'   game_details <- lapply(first_five, vgi_game_metadata)
#'   
#'   # Display game names
#'   for (game in game_details) {
#'     cat(game$name, "(", game$steamAppId, ")\n")
#'   }
#' }
#' 
#' # Analyze publisher's portfolio
#' if (length(game_ids) > 0) {
#'   # Get metadata for all games
#'   all_games <- vgi_game_metadata_batch(game_ids)
#'   
#'   # Group by genre
#'   genre_counts <- table(unlist(lapply(all_games$genres, function(x) x)))
#'   print("Publisher's genre distribution:")
#'   print(sort(genre_counts, decreasing = TRUE))
#' }
#' 
#' # Find publisher's most successful games
#' if (length(game_ids) > 10) {
#'   # Get revenue data for top games
#'   revenues <- lapply(head(game_ids, 10), function(id) {
#'     tryCatch({
#'       rev_data <- vgi_insights_revenue(id)
#'       list(id = id, revenue = rev_data$revenueTotal)
#'     }, error = function(e) NULL)
#'   })
#'   
#'   # Filter out NULLs and sort by revenue
#'   revenues <- revenues[!sapply(revenues, is.null)]
#'   revenues <- revenues[order(sapply(revenues, function(x) x$revenue), 
#'                             decreasing = TRUE)]
#' }
#' }
vgi_publisher_games <- function(company_id,
                               auth_token = Sys.getenv("VGI_AUTH_TOKEN"),
                               headers = list()) {
  
  # Validate inputs
  validate_numeric(company_id, "company_id")
  
  # Make API request
  result <- make_api_request(
    endpoint = paste0("publishers/", company_id, "/game-ids"),
    auth_token = auth_token,
    method = "GET",
    headers = headers
  )
  
  # The result should be an array of game IDs
  if (is.list(result) && length(result) > 0) {
    # Convert to numeric vector
    game_ids <- unlist(result)
    return(as.numeric(game_ids))
  } else {
    # Return empty numeric vector
    return(numeric(0))
  }
}