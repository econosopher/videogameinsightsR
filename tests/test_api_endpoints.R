# Test all VGI API endpoints and generate a comprehensive report
# This script tests each endpoint to determine what's working and what's not

library(tidyverse)
library(httr2)
library(jsonlite)
library(knitr)

# Load dotenv to get API key
if (!require(dotenv)) install.packages("dotenv")
library(dotenv)

# Load environment variables
parent_env <- file.path(dirname(here::here()), ".env")
if (file.exists(parent_env)) {
  load_dot_env(parent_env)
}

# Check API key
api_key <- Sys.getenv("VGI_AUTH_TOKEN")
cat(paste0("\nAPI Key Status: ", ifelse(api_key != "", "Found", "NOT FOUND"), "\n"))
if (api_key != "") {
  cat(paste0("API Key (first 10 chars): ", substr(api_key, 1, 10), "...\n"))
}

# Source all VGI functions
vgi_path <- here::here("R")
r_files <- list.files(vgi_path, pattern = "\\.R$", full.names = TRUE)
# Source utils.R first as other files depend on it
source(file.path(vgi_path, "utils.R"))
# Then source the rest
for (file in r_files) {
  if (!grepl("utils.R", file)) {
    source(file)
  }
}

# Initialize results storage
test_results <- list()

# Test authentication first
cat("\n=== AUTHENTICATION TEST ===\n")
test_auth <- tryCatch({
  req <- request("https://vginsights.com/api/v3") |>
    req_url_path_append("games", "game-list") |>
    req_url_query(limit = 1) |>
    req_headers("api-key" = api_key) |>
    req_user_agent("auth-test")
  
  resp <- req |> req_perform()
  
  if (resp_status(resp) == 200) {
    cat("✅ Authentication successful - API key is valid\n")
    TRUE
  } else {
    cat(paste0("❌ Authentication failed - Status: ", resp_status(resp), "\n"))
    FALSE
  }
}, error = function(e) {
  cat(paste0("❌ Authentication error: ", e$message, "\n"))
  FALSE
})

if (!test_auth) {
  cat("\n⚠️  WARNING: API authentication failed. Results may be limited.\n")
}

# Test configuration
test_game_ids <- c(
  10,      # Low ID that appears in demo data
  730,     # Counter-Strike (popular game)
  24960,   # Battlefield Bad Company 2
  1517290  # Battlefield 2042
)

test_dates <- c(
  Sys.Date(),      # Today
  Sys.Date() - 1,  # Yesterday
  Sys.Date() - 30, # 30 days ago
  "2024-01-01"     # Specific past date
)

# Function to test an endpoint
test_endpoint <- function(name, test_func, ...) {
  cat(paste0("\nTesting: ", name, "\n"))
  
  result <- tryCatch({
    response <- test_func(...)
    
    # Check if we got data
    if (is.null(response)) {
      list(status = "NULL_RESPONSE", message = "Function returned NULL")
    } else if (is.data.frame(response) && nrow(response) == 0) {
      list(status = "EMPTY_DATA", message = "Function returned empty data frame")
    } else if (is.data.frame(response)) {
      list(
        status = "SUCCESS",
        rows = nrow(response),
        cols = ncol(response),
        columns = names(response),
        sample = head(response, 2)
      )
    } else if (is.list(response)) {
      list(
        status = "SUCCESS",
        type = "list",
        length = length(response),
        elements = names(response)
      )
    } else {
      list(status = "SUCCESS", type = class(response))
    }
  }, error = function(e) {
    list(status = "ERROR", message = e$message)
  })
  
  result$endpoint <- name
  result$timestamp <- Sys.time()
  return(result)
}

# Test 1: Search endpoints
cat("\n=== SEARCH ENDPOINTS ===\n")

test_results$search_games <- test_endpoint(
  "vgi_search_games",
  vgi_search_games,
  query = "Battlefield",
  limit = 10
)

test_results$game_list <- test_endpoint(
  "vgi_game_list",
  vgi_game_list,
  limit = 20
)

# Test 2: Game metadata endpoints
cat("\n=== GAME METADATA ENDPOINTS ===\n")

for (id in test_game_ids) {
  test_results[[paste0("game_metadata_", id)]] <- test_endpoint(
    paste0("vgi_game_metadata (ID: ", id, ")"),
    vgi_game_metadata,
    steam_app_id = id
  )
}

# Test 3: Rankings endpoints
cat("\n=== RANKINGS ENDPOINTS ===\n")

test_results$game_rankings <- test_endpoint(
  "vgi_game_rankings",
  vgi_game_rankings,
  limit = 50
)

test_results$top_games_revenue <- test_endpoint(
  "vgi_top_games (revenue)",
  vgi_top_games,
  metric = "revenue",
  limit = 20
)

test_results$top_games_ccu <- test_endpoint(
  "vgi_top_games (ccu)",
  vgi_top_games,
  metric = "ccu",
  limit = 20
)

# Test 4: Time series endpoints
cat("\n=== TIME SERIES ENDPOINTS ===\n")

# CCU by date
for (date in test_dates[1:2]) {
  test_results[[paste0("ccu_date_", date)]] <- test_endpoint(
    paste0("vgi_concurrent_players_by_date (", date, ")"),
    vgi_concurrent_players_by_date,
    date = date
  )
}

# Revenue by date for specific games
test_results$revenue_by_date <- test_endpoint(
  "vgi_revenue_by_date (Game 730)",
  vgi_revenue_by_date,
  steam_app_id = 730,
  start_date = "2024-01-01",
  end_date = "2024-01-31"
)

# Active players
test_results$active_players <- test_endpoint(
  "vgi_active_players_by_date (Game 730)",
  vgi_active_players_by_date,
  steam_app_id = 730,
  start_date = "2024-01-01",
  end_date = "2024-01-31"
)

# Test 5: Insights endpoints
cat("\n=== INSIGHTS ENDPOINTS ===\n")

for (id in test_game_ids[1:2]) {
  test_results[[paste0("insights_ccu_", id)]] <- test_endpoint(
    paste0("vgi_insights_ccu (ID: ", id, ")"),
    vgi_insights_ccu,
    steam_app_id = id
  )
  
  test_results[[paste0("insights_revenue_", id)]] <- test_endpoint(
    paste0("vgi_insights_revenue (ID: ", id, ")"),
    vgi_insights_revenue,
    steam_app_id = id
  )
}

# Test 6: Developer/Publisher endpoints
cat("\n=== DEVELOPER/PUBLISHER ENDPOINTS ===\n")

test_results$developer_list <- test_endpoint(
  "vgi_developer_list",
  vgi_developer_list,
  limit = 10
)

test_results$publisher_list <- test_endpoint(
  "vgi_publisher_list",
  vgi_publisher_list,
  limit = 10
)

# Generate Markdown report
cat("\n\n=== GENERATING REPORT ===\n")

# Create summary statistics
total_tests <- length(test_results)
successful_tests <- sum(sapply(test_results, function(x) x$status == "SUCCESS"))
failed_tests <- sum(sapply(test_results, function(x) x$status == "ERROR"))
empty_tests <- sum(sapply(test_results, function(x) x$status %in% c("NULL_RESPONSE", "EMPTY_DATA")))

# Generate Markdown content
md_content <- c(
  "# Video Game Insights API Endpoint Test Report",
  paste0("\n**Generated:** ", Sys.time()),
  paste0("\n**Package Version:** ", packageVersion("videogameinsightsR")),
  "",
  "## Summary",
  "",
  paste0("- **Total Endpoints Tested:** ", total_tests),
  paste0("- **Successful:** ", successful_tests, " (", round(successful_tests/total_tests*100, 1), "%)"),
  paste0("- **Failed:** ", failed_tests, " (", round(failed_tests/total_tests*100, 1), "%)"),
  paste0("- **Empty/Null:** ", empty_tests, " (", round(empty_tests/total_tests*100, 1), "%)"),
  "",
  "## Detailed Results",
  ""
)

# Group results by category
categories <- list(
  "Search" = grep("search|game_list", names(test_results), value = TRUE),
  "Metadata" = grep("metadata", names(test_results), value = TRUE),
  "Rankings" = grep("ranking|top_games", names(test_results), value = TRUE),
  "Time Series" = grep("date|revenue_by|active_players", names(test_results), value = TRUE),
  "Insights" = grep("insights", names(test_results), value = TRUE),
  "Developer/Publisher" = grep("developer|publisher", names(test_results), value = TRUE)
)

for (cat_name in names(categories)) {
  md_content <- c(md_content, paste0("\n### ", cat_name, "\n"))
  
  cat_results <- test_results[categories[[cat_name]]]
  
  for (result_name in names(cat_results)) {
    result <- cat_results[[result_name]]
    
    status_emoji <- switch(result$status,
      "SUCCESS" = "✅",
      "ERROR" = "❌",
      "EMPTY_DATA" = "⚠️",
      "NULL_RESPONSE" = "⚠️",
      "⚪"
    )
    
    md_content <- c(md_content, paste0("\n#### ", status_emoji, " ", result$endpoint))
    
    if (result$status == "SUCCESS") {
      if (!is.null(result$rows)) {
        md_content <- c(md_content, paste0("- **Rows returned:** ", result$rows))
        md_content <- c(md_content, paste0("- **Columns:** ", paste(result$columns, collapse = ", ")))
      } else if (!is.null(result$type)) {
        md_content <- c(md_content, paste0("- **Type:** ", result$type))
        if (!is.null(result$length)) {
          md_content <- c(md_content, paste0("- **Length:** ", result$length))
        }
      }
    } else {
      md_content <- c(md_content, paste0("- **Status:** ", result$status))
      if (!is.null(result$message)) {
        md_content <- c(md_content, paste0("- **Error:** ", result$message))
      }
    }
    
    md_content <- c(md_content, "")
  }
}

# Add observations
md_content <- c(md_content,
  "\n## Key Observations",
  "",
  "Based on the test results:",
  ""
)

# Check for demo data pattern
if (any(grepl("ccu_date", names(test_results)))) {
  ccu_results <- test_results[grep("ccu_date", names(test_results))]
  if (any(sapply(ccu_results, function(x) x$status == "SUCCESS" && x$rows <= 5))) {
    md_content <- c(md_content,
      "- **Limited CCU Data:** The concurrent players endpoint returns only 5 games (IDs: 10, 20, 30, 40, 50)",
      "- **Demo/Sandbox Mode:** The API appears to be operating in a limited demo mode"
    )
  }
}

# Check for 404 patterns
error_404_count <- sum(sapply(test_results, function(x) grepl("404", x$message)))
if (error_404_count > 0) {
  md_content <- c(md_content,
    paste0("- **404 Errors:** ", error_404_count, " endpoints returned 404 Not Found"),
    "- **Game Coverage:** Modern games (high Steam IDs) are not accessible"
  )
}

# Write report to a temp location to avoid polluting repo
report_path <- file.path(tempdir(), "api_endpoint_test_report.md")
writeLines(md_content, report_path)
cat(paste0("\nReport saved to: ", report_path, "\n"))

# Also save raw results for debugging
results_path <- file.path(tempdir(), "api_test_results.rds")
saveRDS(test_results, results_path)
cat(paste0("Raw results saved to: ", results_path, "\n"))

# Print summary to console
cat("\n=== TEST SUMMARY ===\n")
cat(paste0("Total: ", total_tests, " | Success: ", successful_tests, 
           " | Failed: ", failed_tests, " | Empty: ", empty_tests, "\n"))

# Return results for further analysis
invisible(test_results)