# Test script for vgi_game_metadata function

# Load the package
devtools::load_all()

# Check if auth token is set
token_check <- Sys.getenv("VGI_AUTH_TOKEN")
if (token_check == "") {
  stop("Please set VGI_AUTH_TOKEN environment variable first")
}

cat("Testing vgi_game_metadata function...\n")

# Test with Valheim (Steam App ID: 892970)
tryCatch({
  result <- vgi_game_metadata(892970)
  cat("Success! Retrieved metadata for game:\n")
  print(result)
}, error = function(e) {
  cat("Error occurred:\n")
  cat(e$message, "\n")
})

# Test with invalid ID to check error handling
cat("\nTesting error handling with invalid ID...\n")
tryCatch({
  result <- vgi_game_metadata(999999999)
  cat("Unexpected success with invalid ID\n")
}, error = function(e) {
  cat("Expected error caught:\n")
  cat(e$message, "\n")
})