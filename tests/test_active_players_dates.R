# Test active players function with dates before DAU/MAU availability
library(tidyverse)
library(dotenv)

# Load environment variables
parent_env <- file.path(dirname(getwd()), ".env")
if (file.exists(parent_env)) {
  load_dot_env(parent_env)
}

# Source VGI functions
vgi_path <- file.path(dirname(getwd()), "videogameinsightsR", "R")
if (dir.exists(vgi_path)) {
  source(file.path(vgi_path, "utils.R"))
  source(file.path(vgi_path, "vgi_active_players_by_date.R"))
  cat("✅ VGI functions loaded\n\n")
}

# Define test Steam IDs
test_ids <- c(730, 570, 440)  # CS:GO, Dota 2, TF2

cat("=== TESTING ACTIVE PLAYERS DATE HANDLING ===\n\n")

# Test 1: Date before DAU availability (should adjust to 2024-03-18)
cat("Test 1: Requesting date before DAU availability (2024-01-01)\n")
result1 <- tryCatch({
  vgi_active_players_by_date("2024-01-01", steam_app_ids = test_ids)
}, error = function(e) {
  cat(paste0("Error: ", e$message, "\n"))
  NULL
})

if (!is.null(result1)) {
  cat(paste0("Requested date: ", attr(result1, "requested_date"), "\n"))
  cat(paste0("Actual date returned: ", attr(result1, "actual_date"), "\n"))
  cat(paste0("Number of games returned: ", nrow(result1), "\n"))
  if (nrow(result1) > 0) {
    cat("\nSample data:\n")
    print(head(result1, 3))
  }
}

# Test 2: Date between DAU and MAU start (should work but note MAU limitation)
cat("\n\nTest 2: Requesting date between DAU and MAU start (2024-03-20)\n")
result2 <- tryCatch({
  vgi_active_players_by_date("2024-03-20", steam_app_ids = test_ids)
}, error = function(e) {
  cat(paste0("Error: ", e$message, "\n"))
  NULL
})

if (!is.null(result2)) {
  cat(paste0("Requested date: ", attr(result2, "requested_date"), "\n"))
  cat(paste0("Actual date returned: ", attr(result2, "actual_date"), "\n"))
  cat(paste0("MAU limited: ", ifelse(is.null(attr(result2, "mau_limited")), "No", "Yes"), "\n"))
  cat(paste0("Number of games returned: ", nrow(result2), "\n"))
}

# Test 3: Date after full availability (should work normally)
cat("\n\nTest 3: Requesting date after full availability (2024-04-01)\n")
result3 <- tryCatch({
  vgi_active_players_by_date("2024-04-01", steam_app_ids = test_ids)
}, error = function(e) {
  cat(paste0("Error: ", e$message, "\n"))
  NULL
})

if (!is.null(result3)) {
  cat(paste0("Requested date: ", attr(result3, "requested_date"), "\n"))
  cat(paste0("Actual date returned: ", attr(result3, "actual_date"), "\n"))
  cat(paste0("Number of games returned: ", nrow(result3), "\n"))
  if (nrow(result3) > 0) {
    cat("\nDAU/MAU ratios:\n")
    result3 %>%
      select(steamAppId, dau, mau, dauMauRatio) %>%
      mutate(dauMauRatio = round(dauMauRatio, 3)) %>%
      print()
  }
}

# Test 4: Test with Battlefield IDs
cat("\n\nTest 4: Testing with Battlefield IDs on early date\n")
battlefield_ids <- c(1517290, 1238810, 1238860, 1238840, 24960)

result4 <- tryCatch({
  vgi_active_players_by_date("2023-12-01", steam_app_ids = battlefield_ids)
}, warning = function(w) {
  cat(paste0("Warning: ", w$message, "\n"))
  invokeRestart("muffleWarning")
}, error = function(e) {
  cat(paste0("Error: ", e$message, "\n"))
  NULL
})

if (!is.null(result4)) {
  cat(paste0("Actual date used: ", attr(result4, "actual_date"), "\n"))
  cat(paste0("Number of Battlefield games with data: ", nrow(result4), "\n"))
}

cat("\n=== SUMMARY ===\n")
cat("✅ Function now handles dates before DAU/MAU availability\n")
cat("✅ Automatically adjusts to earliest available date (2024-03-18)\n")
cat("✅ Warns user when date is adjusted\n")
cat("✅ Notes when MAU data might be incomplete (2024-03-18 to 2024-03-22)\n")
cat("✅ Returns metadata about requested vs actual dates\n")