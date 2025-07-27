# Basic test script to verify package loading and function availability

# Load the package
devtools::load_all()

# Check if functions exist
cat("Checking if functions are available...\n")
cat("vgi_game_metadata exists:", exists("vgi_game_metadata"), "\n")
cat("make_api_request exists:", exists("make_api_request"), "\n")
cat("get_auth_token exists:", exists("get_auth_token"), "\n")

# Test auth token validation
cat("\nTesting auth token validation...\n")
tryCatch({
  token <- get_auth_token()
  cat("Token found in environment\n")
}, error = function(e) {
  cat("Expected error when token not set:\n")
  cat(e$message, "\n")
})

# Test function without token to verify error handling
cat("\nTesting vgi_game_metadata without token...\n")
Sys.unsetenv("VGI_AUTH_TOKEN")
tryCatch({
  result <- vgi_game_metadata(892970)
}, error = function(e) {
  cat("Expected error:\n")
  cat(e$message, "\n")
})