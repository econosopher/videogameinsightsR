# Endpoint Validation Testing Framework
# Validates API responses against expected data structures

library(testthat)
library(httr2)
library(jsonlite)
library(videogameinsightsR)

# Test configuration
readRenviron("../../.env")
auth_token <- Sys.getenv("VGI_AUTH_TOKEN")

context("Endpoint Response Validation")

# Helper to make direct API calls
make_test_request <- function(endpoint, query_params = list()) {
  base_url <- "https://vginsights.com/api/v3"
  
  request(base_url) %>%
    req_url_path_append(endpoint) %>%
    req_headers("api-key" = auth_token) %>%
    req_url_query(!!!query_params) %>%
    req_perform()
}

# Test data quality checks
test_that("Game rankings returns modern games", {
  response <- make_test_request("games/rankings", list(limit = 20))
  content <- resp_body_json(response)
  
  # Extract game IDs
  game_ids <- sapply(content, function(x) x$steamAppId)
  
  # Check for old games (ID < 1000)
  old_games <- game_ids[game_ids < 1000]
  
  expect_equal(length(old_games), 0,
               info = paste("API returns old games with IDs:", 
                           paste(old_games, collapse = ", "),
                           "\nExpected modern games (IDs > 1000)"))
})

test_that("Game metadata returns complete information", {
  # Test with known modern game IDs
  test_games <- list(
    "Counter-Strike 2" = 730,
    "PUBG" = 578080,
    "Apex Legends" = 1172470
  )
  
  for (game_name in names(test_games)) {
    game_id <- test_games[[game_name]]
    
    response <- make_test_request(paste0("games/", game_id, "/metadata"))
    
    if (resp_status(response) == 200) {
      content <- resp_body_json(response)
      
      # Check required fields
      expect_true("name" %in% names(content),
                  info = paste("Missing 'name' field for", game_name))
      expect_true("steamAppId" %in% names(content),
                  info = paste("Missing 'steamAppId' field for", game_name))
      
      # Verify it's the correct game
      if (!is.null(content$steamAppId)) {
        expect_equal(content$steamAppId, game_id,
                     info = paste("Wrong game ID returned for", game_name))
      }
    }
  }
})

test_that("Top games endpoints return valid data", {
  metrics <- c("revenue", "units", "ccu", "dau", "followers")
  
  for (metric in metrics) {
    endpoint <- paste0("analytics/top-games-", metric)
    
    tryCatch({
      response <- make_test_request(endpoint, list(limit = 10))
      
      if (resp_status(response) == 200) {
        content <- resp_body_json(response)
        
        # Check response structure
        expect_true(length(content) > 0,
                    info = paste("Empty response for metric:", metric))
        
        # Check for required fields in first item
        if (length(content) > 0) {
          first_item <- content[[1]]
          
          expect_true("steamAppId" %in% names(first_item),
                      info = paste("Missing steamAppId in", metric, "response"))
          
          # Check for modern games
          if (!is.null(first_item$steamAppId)) {
            expect_true(first_item$steamAppId > 100,
                        info = paste("Old game ID in", metric, "response:", 
                                    first_item$steamAppId))
          }
        }
      }
    }, error = function(e) {
      fail(paste("Error testing", metric, "endpoint:", e$message))
    })
  }
})

# Date range validation
test_that("Date filtering works correctly", {
  # Test with recent date range
  end_date <- Sys.Date()
  start_date <- end_date - 7
  
  response <- make_test_request("games/rankings", 
                               list(start_date = format(start_date, "%Y-%m-%d"),
                                    end_date = format(end_date, "%Y-%m-%d"),
                                    limit = 10))
  
  if (resp_status(response) == 200) {
    content <- resp_body_json(response)
    
    # The response should reflect the date range somehow
    # Check if we get different results for different dates
    response_old <- make_test_request("games/rankings",
                                     list(start_date = "2024-01-01",
                                          end_date = "2024-01-07",
                                          limit = 10))
    
    if (resp_status(response_old) == 200) {
      content_old <- resp_body_json(response_old)
      
      # Extract game IDs from both responses
      ids_recent <- sapply(content, function(x) x$steamAppId)
      ids_old <- sapply(content_old, function(x) x$steamAppId)
      
      # Log findings
      cat("Recent date range games:", paste(ids_recent, collapse = ", "), "\n")
      cat("Old date range games:", paste(ids_old, collapse = ", "), "\n")
      
      # They should potentially be different if date filtering works
      # But if API returns same data, that's a problem
      if (identical(ids_recent, ids_old)) {
        warning("Date filtering may not be working - same games returned for different dates")
      }
    }
  }
})

# Genre filtering validation
test_that("Genre filtering returns appropriate games", {
  genres <- c("Shooter", "RPG", "Strategy")
  
  for (genre in genres) {
    response <- make_test_request("games/game-list",
                                 list(genre = genre, limit = 10))
    
    if (resp_status(response) == 200) {
      content <- resp_body_json(response)
      
      # Check if we get games
      expect_true(length(content) > 0,
                  info = paste("No games returned for genre:", genre))
      
      # Ideally check if games actually belong to the genre
      # but this requires game metadata
    }
  }
})

# Response time monitoring
test_that("API responds within reasonable time", {
  start_time <- Sys.time()
  
  response <- make_test_request("games/rankings", list(limit = 100))
  
  end_time <- Sys.time()
  response_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  expect_lt(response_time, 5,
            info = paste("API took", round(response_time, 2), "seconds to respond"))
})

# Generate validation report
generate_validation_report <- function() {
  report <- list(
    timestamp = Sys.time(),
    issues = list(),
    warnings = list(),
    api_health = "Unknown"
  )
  
  # Check game rankings
  tryCatch({
    response <- make_test_request("games/rankings", list(limit = 50))
    if (resp_status(response) == 200) {
      content <- resp_body_json(response)
      game_ids <- sapply(content, function(x) x$steamAppId)
      
      # Check for old games
      old_games <- game_ids[game_ids < 1000]
      if (length(old_games) > 0) {
        report$issues$old_games <- paste("Rankings returns old games:", 
                                        paste(unique(old_games), collapse = ", "))
      }
      
      # Check for missing modern games
      modern_games <- game_ids[game_ids > 100000]
      if (length(modern_games) == 0) {
        report$issues$no_modern_games <- "No modern games (ID > 100000) in rankings"
      }
    }
  }, error = function(e) {
    report$issues$rankings_error <- e$message
  })
  
  # Check metadata endpoint
  test_game_ids <- c(730, 578080, 1172470)
  metadata_works <- 0
  
  for (game_id in test_game_ids) {
    tryCatch({
      response <- make_test_request(paste0("games/", game_id, "/metadata"))
      if (resp_status(response) == 200) {
        metadata_works <- metadata_works + 1
      }
    }, error = function(e) {})
  }
  
  if (metadata_works < length(test_game_ids)) {
    report$warnings$metadata <- paste("Metadata endpoint failed for",
                                     length(test_game_ids) - metadata_works,
                                     "out of", length(test_game_ids), "games")
  }
  
  # Overall health assessment
  if (length(report$issues) == 0) {
    report$api_health <- "Good"
  } else if (length(report$issues) <= 2) {
    report$api_health <- "Warning"
  } else {
    report$api_health <- "Critical"
  }
  
  return(report)
}

# Run validation report
cat("\n=== API ENDPOINT VALIDATION REPORT ===\n\n")
validation_report <- generate_validation_report()

cat("Timestamp:", format(validation_report$timestamp), "\n")
cat("API Health Status:", validation_report$api_health, "\n\n")

if (length(validation_report$issues) > 0) {
  cat("CRITICAL ISSUES:\n")
  for (issue_name in names(validation_report$issues)) {
    cat(sprintf("- %s\n", validation_report$issues[[issue_name]]))
  }
  cat("\n")
}

if (length(validation_report$warnings) > 0) {
  cat("WARNINGS:\n")
  for (warning_name in names(validation_report$warnings)) {
    cat(sprintf("- %s\n", validation_report$warnings[[warning_name]]))
  }
  cat("\n")
}

# Save report
saveRDS(validation_report, file.path(tempdir(), "endpoint_validation_report.rds"))
cat("Detailed report saved to: endpoint_validation_report.rds\n")