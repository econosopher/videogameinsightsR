#' Get Complete Publisher List
#'
#' Retrieve a list of all game publishers in the Video Game Insights database.
#'
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{id}{Integer. The company ID}
#'   \item{name}{Character. The publisher name}
#' }
#'
#' @details
#' This endpoint is useful for:
#' \itemize{
#'   \item Discovering all tracked publishers
#'   \item Building publisher selection interfaces
#'   \item Finding publisher IDs for further queries
#'   \item Analyzing the publishing landscape
#' }
#'
#' @export
#' @examples
#' \dontrun{
#' # Get all publishers
#' all_pubs <- vgi_publisher_list()
#' cat("Total publishers:", nrow(all_pubs), "\n")
#' 
#' # Search for major publishers
#' ea_pubs <- all_pubs[grep("Electronic Arts", all_pubs$name, ignore.case = TRUE), ]
#' print(ea_pubs)
#' 
#' # Find publisher ID by exact name
#' pub_id <- all_pubs$id[all_pubs$name == "Ubisoft"]
#' if (length(pub_id) > 0) {
#'   cat("Ubisoft ID:", pub_id, "\n")
#'   
#'   # Get more info about this publisher
#'   ubi_info <- vgi_publisher_info(pub_id)
#'   print(ubi_info)
#' }
#' 
#' # Analyze publisher ecosystem
#' # Find self-published developers
#' all_devs <- vgi_developer_list()
#' self_published <- intersect(all_pubs$name, all_devs$name)
#' cat("Companies that both develop and publish:", length(self_published), "\n")
#' 
#' # Find publishers with "Games" in name
#' games_pubs <- all_pubs[grep("Games", all_pubs$name), ]
#' cat("Publishers with 'Games' in name:", nrow(games_pubs), "\n")
#' 
#' # Export for analysis
#' write.csv(all_pubs, "vgi_publishers.csv", row.names = FALSE)
#' }
vgi_publisher_list <- function(auth_token = Sys.getenv("VGI_AUTH_TOKEN"), headers = list()) {
  
  # Make API request
  result <- make_api_request(
    endpoint = "publishers/publisher-list",
    auth_token = auth_token,
    method = "GET",
    headers = headers
  )
  
  # Convert to data frame
  if (is.list(result) && length(result) > 0) {
    df <- do.call(rbind, lapply(result, function(x) {
      data.frame(
        id = as.integer(x$id),
        name = as.character(x$name),
        stringsAsFactors = FALSE
      )
    }))
    
    # Sort by name for easier browsing
    df <- df[order(df$name), ]
    
    return(df)
  } else {
    # Return empty data frame with correct structure
    return(data.frame(
      id = integer(),
      name = character(),
      stringsAsFactors = FALSE
    ))
  }
}