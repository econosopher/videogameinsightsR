#' Get Complete Developer List
#'
#' Retrieve a list of all game developers in the Video Game Insights database.
#'
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @param headers List. Optional custom headers to include in the API request.
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{id}{Integer. The company ID}
#'   \item{name}{Character. The developer name}
#' }
#'
#' @details
#' This endpoint is useful for:
#' \itemize{
#'   \item Discovering all tracked developers
#'   \item Building developer selection interfaces
#'   \item Finding developer IDs for further queries
#'   \item Analyzing the developer ecosystem
#' }
#'
#' @export
#' @examples
#' \dontrun{
#' # Get all developers
#' all_devs <- vgi_developer_list()
#' cat("Total developers:", nrow(all_devs), "\n")
#' 
#' # Search for developers by name
#' valve_devs <- all_devs[grep("Valve", all_devs$name, ignore.case = TRUE), ]
#' print(valve_devs)
#' 
#' # Find developer ID by exact name
#' dev_id <- all_devs$id[all_devs$name == "Valve Corporation"]
#' if (length(dev_id) > 0) {
#'   cat("Valve Corporation ID:", dev_id, "\n")
#'   
#'   # Get more info about this developer
#'   valve_info <- vgi_developer_info(dev_id)
#'   print(valve_info)
#' }
#' 
#' # Analyze developer names
#' # Find developers with "Studios" in name
#' studios <- all_devs[grep("Studios", all_devs$name), ]
#' cat("Developers with 'Studios':", nrow(studios), "\n")
#' 
#' # Find indie developers (often individual names or small teams)
#' short_names <- all_devs[nchar(all_devs$name) < 15, ]
#' cat("Developers with short names:", nrow(short_names), "\n")
#' }
vgi_developer_list <- function(auth_token = Sys.getenv("VGI_AUTH_TOKEN"), headers = list()) {
  
  # Make API request
  result <- make_api_request(
    endpoint = "developers/developer-list",
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