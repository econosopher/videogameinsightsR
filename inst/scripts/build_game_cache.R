#!/usr/bin/env Rscript

# BATCH DOWNLOAD CACHE FOR VideoGameInsightsR
# 
# PURPOSE: This creates a cache for BATCH OPERATIONS, not for searching!
# 
# If you want to search for games, use vgi_smart_game_search() which:
#   1. Finds one game matching your query
#   2. Gets its publisher/developer
#   3. Returns ALL related games
# 
# This cache is ONLY useful for:
#   - Batch processing operations where you need game names
#   - Avoiding API calls when you already know the Steam ID
#   - Development/testing purposes
# 
# DO NOT use this cache as a "search database" - that's not what it's for!

suppressPackageStartupMessages({
  library(pacman)
  p_load(devtools, dplyr, purrr, tidyr, tibble)
})

# Check for VGI API authentication
if (!nzchar(Sys.getenv("VGI_AUTH_TOKEN"))) {
  stop("VGI_AUTH_TOKEN environment variable is required. Set it with Sys.setenv(VGI_AUTH_TOKEN='your_token')")
}

# Load the VideoGameInsightsR package
devtools::load_all("/Users/phillip/Documents/vibe_coding_projects/VideoGameInsightsR")

message("=== Building Game Cache for VideoGameInsightsR ===\n")

# Configuration
TARGET_GAMES <- 2000  # Reasonable number for initial cache
BATCH_SIZE <- 1000  # Maximum limit per API call
CACHE_FILE <- "/Users/phillip/Documents/vibe_coding_projects/VideoGameInsightsR/inst/extdata/game_cache.rds"

# Ensure directory exists
dir.create(dirname(CACHE_FILE), recursive = TRUE, showWarnings = FALSE)

# Initialize storage for unique games
all_games <- tibble::tibble(
  steamAppId = integer(),
  name = character()
)

# Track unique Steam IDs
unique_ids <- c()

message("Step 1: Fetching Steam IDs from rankings endpoint...")
message(sprintf("Target: %d unique games, batch size: %d\n", TARGET_GAMES, BATCH_SIZE))

# Calculate how many batches we might need
max_batches <- ceiling(TARGET_GAMES / BATCH_SIZE) * 2  # Double to account for potential duplicates
current_offset <- 0
batch_num <- 1

while (length(unique_ids) < TARGET_GAMES && batch_num <= max_batches) {
  message(sprintf("Batch %d: Fetching games with offset=%d, limit=%d", 
                  batch_num, current_offset, BATCH_SIZE))
  
  # Fetch batch of games from rankings
  rankings <- tryCatch({
    vgi_game_rankings(
      offset = current_offset,
      limit = BATCH_SIZE
    )
  }, error = function(e) {
    message(sprintf("  Error: %s", e$message))
    NULL
  })
  
  if (is.null(rankings) || nrow(rankings) == 0) {
    message("  No more games returned from API")
    break
  }
  
  # Extract Steam IDs
  batch_ids <- unique(rankings$steamAppId)
  new_ids <- setdiff(batch_ids, unique_ids)
  
  message(sprintf("  Received %d games, %d new unique IDs", 
                  nrow(rankings), length(new_ids)))
  
  # Add new unique IDs
  unique_ids <- c(unique_ids, new_ids)
  
  message(sprintf("  Total unique IDs so far: %d", length(unique_ids)))
  
  # If we got fewer results than requested, we've likely hit the end
  if (nrow(rankings) < BATCH_SIZE) {
    message("  Reached end of available rankings")
    break
  }
  
  # Update offset for next batch
  current_offset <- current_offset + BATCH_SIZE
  batch_num <- batch_num + 1
  
  # Small delay to be respectful to the API
  Sys.sleep(0.5)
}

message(sprintf("\n✓ Collected %d unique Steam IDs", length(unique_ids)))

# Trim to target size if we got more
if (length(unique_ids) > TARGET_GAMES) {
  unique_ids <- unique_ids[1:TARGET_GAMES]
  message(sprintf("  Trimmed to target of %d games", TARGET_GAMES))
}

message("\nStep 2: Fetching game names from metadata endpoint...")
message("This may take a while as we need to make individual API calls for each game...")
message(sprintf("Estimated time: %.1f minutes\n", length(unique_ids) * 0.15 / 60))

# Function to fetch metadata for a single game
fetch_game_name <- function(steam_id, index, total) {
  if (index %% 100 == 0) {
    message(sprintf("  Progress: %d/%d games (%.1f%%)", 
                    index, total, 100 * index / total))
  }
  
  tryCatch({
    # Small delay to avoid hammering the API
    Sys.sleep(0.1)
    
    metadata <- vgi_game_metadata(steam_id)
    
    if (!is.null(metadata) && "name" %in% names(metadata)) {
      return(tibble::tibble(
        steamAppId = as.integer(steam_id),
        name = as.character(metadata$name[1])
      ))
    } else {
      # If metadata doesn't have name, return ID with placeholder
      return(tibble::tibble(
        steamAppId = as.integer(steam_id),
        name = paste0("Game_", steam_id)
      ))
    }
  }, error = function(e) {
    # On error, return ID with placeholder name
    return(tibble::tibble(
      steamAppId = as.integer(steam_id),
      name = paste0("Game_", steam_id)
    ))
  })
}

# Fetch metadata for all unique IDs
all_games <- purrr::imap_dfr(unique_ids, function(id, idx) {
  fetch_game_name(id, idx, length(unique_ids))
})

# Remove any duplicates (shouldn't be any, but just in case)
all_games <- all_games %>%
  dplyr::distinct(steamAppId, .keep_all = TRUE) %>%
  dplyr::arrange(name)

message(sprintf("\n✓ Successfully fetched metadata for %d games", nrow(all_games)))

# Show sample of games
message("\nSample of cached games:")
sample_games <- all_games %>%
  dplyr::sample_n(min(10, nrow(all_games)))
print(sample_games)

# Save the cache
message(sprintf("\nStep 3: Saving cache to %s", CACHE_FILE))
saveRDS(all_games, CACHE_FILE)

# Verify the file was saved
if (file.exists(CACHE_FILE)) {
  file_size <- file.size(CACHE_FILE) / 1024  # Size in KB
  message(sprintf("✓ Cache saved successfully (%.1f KB)", file_size))
  
  # Test loading it back
  test_load <- readRDS(CACHE_FILE)
  message(sprintf("✓ Cache verification successful: %d games loaded", nrow(test_load)))
} else {
  message("✗ Error: Cache file was not created")
}

message("\n=== Cache building complete ===")
message(sprintf("Total games cached: %d", nrow(all_games)))
message(sprintf("Cache location: %s", CACHE_FILE))
message("\n⚠️  REMINDER: This cache is for BATCH OPERATIONS, not searching!")
message("For intelligent game searching, use vgi_smart_game_search() which finds")
message("related games through publisher/developer relationships.")
message("\nExample: vgi_smart_game_search('battlefield', 'publisher')")
message("This will find ALL EA games, not just ones with 'battlefield' in the name.")
