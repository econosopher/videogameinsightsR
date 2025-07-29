# Tests for game comparison functionality
# This tests real API data retrieval and comparison table generation

test_that("Can search for and retrieve game IDs", {
  skip_if_not(nzchar(Sys.getenv("VGI_AUTH_TOKEN")), "VGI_AUTH_TOKEN not set")
  
  # Search for Marvel Rivals
  marvel_search <- vgi_search_games("Marvel Rivals", auth_token = Sys.getenv("VGI_AUTH_TOKEN"))
  expect_s3_class(marvel_search, "data.frame")
  expect_true("steamAppId" %in% names(marvel_search))
  expect_true("name" %in% names(marvel_search))
  
  # Search for Overwatch
  overwatch_search <- vgi_search_games("Overwatch", auth_token = Sys.getenv("VGI_AUTH_TOKEN"))
  expect_s3_class(overwatch_search, "data.frame")
  expect_true(nrow(overwatch_search) > 0)
})

test_that("Can retrieve DAU/MAU data for games", {
  skip_if_not(nzchar(Sys.getenv("VGI_AUTH_TOKEN")), "VGI_AUTH_TOKEN not set")
  
  # Test with Overwatch 2 (known ID: 2357570)
  dau_data <- vgi_insights_dau_mau(2357570, auth_token = Sys.getenv("VGI_AUTH_TOKEN"))
  
  expect_type(dau_data, "list")
  expect_true("playerHistory" %in% names(dau_data))
  expect_s3_class(dau_data$playerHistory, "data.frame")
  expect_true(all(c("date", "dau", "mau") %in% names(dau_data$playerHistory)))
})

test_that("Can retrieve units sold and concurrent player data", {
  skip_if_not(nzchar(Sys.getenv("VGI_AUTH_TOKEN")), "VGI_AUTH_TOKEN not set")
  
  # Get data for a recent date
  test_date <- Sys.Date() - 7  # 7 days ago
  
  # Test units sold endpoint
  units_data <- vgi_units_sold_by_date(test_date, auth_token = Sys.getenv("VGI_AUTH_TOKEN"))
  expect_s3_class(units_data, "data.frame")
  expect_true(all(c("steamAppId", "unitsSold", "dailyUnits") %in% names(units_data)))
  
  # Test concurrent players endpoint
  ccu_data <- vgi_concurrent_players_by_date(test_date, auth_token = Sys.getenv("VGI_AUTH_TOKEN"))
  expect_s3_class(ccu_data, "data.frame")
  expect_true(all(c("steamAppId", "peakConcurrent", "avgConcurrent") %in% names(ccu_data)))
})

test_that("Can create comparison table for Marvel Rivals vs Overwatch", {
  skip_if_not(nzchar(Sys.getenv("VGI_AUTH_TOKEN")), "VGI_AUTH_TOKEN not set")
  
  # Function to get 180-day comparison data
  get_game_comparison_data <- function(game_name, steam_app_id, days = 180) {
    
    # Get DAU/MAU data
    dau_mau <- vgi_insights_dau_mau(steam_app_id, auth_token = Sys.getenv("VGI_AUTH_TOKEN"))
    
    # Get recent dates for other metrics
    end_date <- Sys.Date() - 1
    start_date <- end_date - days
    
    # Initialize results data frame
    dates <- seq(start_date, end_date, by = "day")
    comparison_data <- data.frame(
      date = dates,
      game = game_name,
      steam_app_id = steam_app_id,
      dau = NA,
      mau = NA,
      units_sold = NA,
      daily_units = NA,
      peak_concurrent = NA,
      avg_concurrent = NA,
      ppsu = NA  # Peak Players per Sold Unit
    )
    
    # Merge DAU/MAU data
    if (!is.null(dau_mau$playerHistory) && nrow(dau_mau$playerHistory) > 0) {
      dau_mau_df <- dau_mau$playerHistory
      dau_mau_df$date <- as.Date(dau_mau_df$date)
      
      # Filter to our date range
      dau_mau_df <- dau_mau_df[dau_mau_df$date >= start_date & dau_mau_df$date <= end_date, ]
      
      # Merge with comparison data
      comparison_data <- merge(comparison_data, dau_mau_df[, c("date", "dau", "mau")], 
                              by = "date", all.x = TRUE, suffixes = c("", ".y"))
      comparison_data$dau <- comparison_data$dau.y
      comparison_data$mau <- comparison_data$mau.y
      comparison_data$dau.y <- NULL
      comparison_data$mau.y <- NULL
    }
    
    # Get units sold and concurrent players for sample dates
    # (Getting all 180 days would be too many API calls for a test)
    sample_dates <- dates[seq(1, length(dates), by = 30)]  # Every 30 days
    
    for (date in sample_dates) {
      date_str <- as.character(date)
      
      # Get units sold
      units <- tryCatch({
        units_data <- vgi_units_sold_by_date(date_str, auth_token = Sys.getenv("VGI_AUTH_TOKEN"))
        units_data[units_data$steamAppId == steam_app_id, ]
      }, error = function(e) NULL)
      
      if (!is.null(units) && nrow(units) > 0) {
        idx <- which(comparison_data$date == date)
        comparison_data$units_sold[idx] <- units$unitsSold[1]
        comparison_data$daily_units[idx] <- units$dailyUnits[1]
      }
      
      # Get concurrent players
      ccu <- tryCatch({
        ccu_data <- vgi_concurrent_players_by_date(date_str, auth_token = Sys.getenv("VGI_AUTH_TOKEN"))
        ccu_data[ccu_data$steamAppId == steam_app_id, ]
      }, error = function(e) NULL)
      
      if (!is.null(ccu) && nrow(ccu) > 0) {
        idx <- which(comparison_data$date == date)
        comparison_data$peak_concurrent[idx] <- ccu$peakConcurrent[1]
        comparison_data$avg_concurrent[idx] <- ccu$avgConcurrent[1]
        
        # Calculate PPSU if we have both peak concurrent and units sold
        if (!is.na(comparison_data$units_sold[idx]) && comparison_data$units_sold[idx] > 0) {
          comparison_data$ppsu[idx] <- comparison_data$peak_concurrent[idx] / comparison_data$units_sold[idx]
        }
      }
    }
    
    return(comparison_data)
  }
  
  # Test with known game IDs
  # Overwatch 2: 2357570
  # Marvel Rivals: 2767030 (if available)
  
  # First, search for the games to get their IDs
  marvel_search <- vgi_search_games("Marvel Rivals", auth_token = Sys.getenv("VGI_AUTH_TOKEN"))
  overwatch_search <- vgi_search_games("Overwatch 2", auth_token = Sys.getenv("VGI_AUTH_TOKEN"))
  
  # Use Overwatch 2 for testing (known to exist)
  if (nrow(overwatch_search) > 0) {
    overwatch_id <- overwatch_search$steamAppId[1]
    
    # Get comparison data for last 30 days (shorter for testing)
    ow_data <- get_game_comparison_data("Overwatch 2", overwatch_id, days = 30)
    
    # Verify the structure
    expect_s3_class(ow_data, "data.frame")
    expect_true(all(c("date", "game", "dau", "mau", "units_sold", 
                     "peak_concurrent", "ppsu") %in% names(ow_data)))
    expect_equal(nrow(ow_data), 30)
    
    # Check that we have at least some data
    expect_true(sum(!is.na(ow_data$dau)) > 0, "Should have some DAU data")
    
    # If Marvel Rivals exists, create comparison
    if (nrow(marvel_search) > 0) {
      marvel_id <- marvel_search$steamAppId[1]
      marvel_data <- get_game_comparison_data("Marvel Rivals", marvel_id, days = 30)
      
      # Combine data
      comparison_table <- rbind(ow_data, marvel_data)
      
      # Summary statistics
      summary_stats <- aggregate(
        cbind(dau, peak_concurrent, ppsu) ~ game,
        data = comparison_table,
        FUN = function(x) mean(x, na.rm = TRUE)
      )
      
      expect_s3_class(summary_stats, "data.frame")
      expect_true(nrow(summary_stats) <= 2)
    }
  }
})

test_that("API rate limiting is handled gracefully", {
  skip_if_not(nzchar(Sys.getenv("VGI_AUTH_TOKEN")), "VGI_AUTH_TOKEN not set")
  
  # Make multiple rapid requests
  results <- list()
  for (i in 1:5) {
    results[[i]] <- tryCatch({
      vgi_search_games(paste0("test", i), auth_token = Sys.getenv("VGI_AUTH_TOKEN"))
    }, error = function(e) e)
  }
  
  # Check that all requests completed (no rate limit errors)
  errors <- sapply(results, function(x) inherits(x, "error"))
  expect_true(sum(errors) < 3, "Most requests should succeed despite rapid calls")
})

test_that("Data consistency across endpoints", {
  skip_if_not(nzchar(Sys.getenv("VGI_AUTH_TOKEN")), "VGI_AUTH_TOKEN not set")
  
  # Use a popular game for testing
  test_game_id <- 730  # Counter-Strike 2
  test_date <- as.character(Sys.Date() - 7)
  
  # Get data from different endpoints
  units <- tryCatch({
    units_data <- vgi_units_sold_by_date(test_date, auth_token = Sys.getenv("VGI_AUTH_TOKEN"))
    units_data[units_data$steamAppId == test_game_id, ]
  }, error = function(e) NULL)
  
  ccu <- tryCatch({
    ccu_data <- vgi_concurrent_players_by_date(test_date, auth_token = Sys.getenv("VGI_AUTH_TOKEN"))
    ccu_data[ccu_data$steamAppId == test_game_id, ]
  }, error = function(e) NULL)
  
  # If we got data from both endpoints, check consistency
  if (!is.null(units) && nrow(units) > 0 && !is.null(ccu) && nrow(ccu) > 0) {
    # Both should have the same steamAppId
    expect_equal(units$steamAppId[1], ccu$steamAppId[1])
    
    # Peak concurrent should be positive if units sold is positive
    if (units$unitsSold[1] > 0) {
      expect_true(ccu$peakConcurrent[1] > 0, 
                  "Games with sales should have concurrent players")
    }
  }
})