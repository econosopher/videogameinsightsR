#' Check VGI API Authentication
#'
#' Performs a lightweight authenticated request to verify that the configured
#' credentials can access the API. Returns TRUE on success; otherwise FALSE
#' with an informative message.
#'
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#' @return Logical indicating whether authentication appears valid.
#' @export
vgi_auth_check <- function(auth_token = Sys.getenv("VGI_AUTH_TOKEN")) {
  ok <- TRUE
  msg <- NULL
  tryCatch({
    res <- make_api_request(
      endpoint = "games/game-list",
      query_params = list(limit = 1),
      auth_token = auth_token,
      method = "GET"
    )
    # Basic sanity check: expect data frame or list with at least one row/item
    if ((is.data.frame(res) && nrow(res) >= 0) || (is.list(res) && length(res) >= 0)) {
      ok <- TRUE
    } else {
      ok <- FALSE
      msg <- "Unexpected response structure from authentication check."
    }
  }, error = function(e) {
    ok <<- FALSE
    msg <<- e$message
  })
  if (!ok && isTRUE(getOption("vgi.verbose", FALSE))) {
    message("Authentication check failed: ", msg)
  }
  ok
}


