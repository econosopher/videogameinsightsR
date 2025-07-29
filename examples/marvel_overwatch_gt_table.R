#!/usr/bin/env Rscript

# Create GT table comparing Marvel Rivals vs Overwatch
# This generates a LinkedIn-optimized comparison table

library(videogameinsightsR)
library(gt)
library(dplyr)
library(tidyr)
library(scales)

# Check for API token
if (!nzchar(Sys.getenv("VGI_AUTH_TOKEN"))) {
  stop("VGI_AUTH_TOKEN environment variable is required. Set it with Sys.setenv(VGI_AUTH_TOKEN='your_token')")
}

# Function to apply GEC theme to GT table
apply_gec_theme <- function(gt_table) {
  gt_table %>%
    tab_options(
      # Overall table styling
      table.font.names = "Arial",
      table.font.size = px(16),
      table.width = px(1200),
      
      # Header styling
      heading.title.font.size = px(24),
      heading.title.font.weight = "bold",
      heading.subtitle.font.size = px(18),
      heading.align = "left",
      
      # Column headers
      column_labels.font.size = px(16),
      column_labels.font.weight = "bold",
      column_labels.background.color = "#2C3E50",
      column_labels.font.color = "white",
      
      # Data cells
      table.font.color = "#2C3E50",
      data_row.padding = px(8),
      
      # Stripes for readability
      row.striping.include_table_body = TRUE,
      row.striping.background_color = "#F8F9FA",
      
      # Borders
      table.border.top.width = px(3),
      table.border.top.color = "#2C3E50",
      table.border.bottom.width = px(3),
      table.border.bottom.color = "#2C3E50",
      
      # Footer
      source_notes.font.size = px(12)
    )
}

# Function to get comparison data with caching
get_comparison_metrics <- function() {
  cache_file <- ".cache/marvel_overwatch_comparison_gt.rds"
  
  # Check cache
  if (file.exists(cache_file)) {
    cache_data <- readRDS(cache_file)
    if (Sys.Date() == cache_data$date) {
      cat("Using cached data from today\n")
      return(cache_data$data)
    }
  }
  
  cat("Fetching fresh data from API...\n")
  
  # Search for games
  marvel_search <- vgi_search_games("Marvel Rivals")
  overwatch_search <- vgi_search_games("Overwatch 2")
  
  # Get IDs (with fallbacks)
  marvel_id <- if(nrow(marvel_search) > 0) marvel_search$steamAppId[1] else 2767030
  overwatch_id <- if(nrow(overwatch_search) > 0) overwatch_search$steamAppId[1] else 2357570
  
  # Initialize results
  comparison_data <- data.frame(
    game = c("Marvel Rivals", "Overwatch 2"),
    steam_app_id = c(marvel_id, overwatch_id),
    stringsAsFactors = FALSE
  )
  
  # Get current metrics for each game
  for (i in 1:nrow(comparison_data)) {
    game_id <- comparison_data$steam_app_id[i]
    game_name <- comparison_data$game[i]
    
    cat(paste("Fetching data for", game_name, "...\n"))
    
    # Get DAU/MAU data
    dau_mau <- tryCatch({
      vgi_insights_dau_mau(game_id)
    }, error = function(e) NULL)
    
    if (!is.null(dau_mau) && !is.null(dau_mau$playerHistory) && nrow(dau_mau$playerHistory) > 0) {
      # Get latest available data
      latest_dau <- tail(dau_mau$playerHistory[!is.na(dau_mau$playerHistory$dau), ], 1)
      if (nrow(latest_dau) > 0) {
        comparison_data$latest_dau[i] <- latest_dau$dau
        comparison_data$latest_mau[i] <- latest_dau$mau
        comparison_data$dau_date[i] <- as.character(latest_dau$date)
      }
      
      # Calculate 7-day and 30-day averages
      recent_7d <- tail(dau_mau$playerHistory, 7)
      recent_30d <- tail(dau_mau$playerHistory, 30)
      
      comparison_data$avg_dau_7d[i] <- mean(recent_7d$dau, na.rm = TRUE)
      comparison_data$avg_dau_30d[i] <- mean(recent_30d$dau, na.rm = TRUE)
      
      # Calculate trend (comparing last 7 days to previous 7 days)
      if (nrow(dau_mau$playerHistory) >= 14) {
        prev_7d <- tail(head(dau_mau$playerHistory, -7), 7)
        curr_7d <- tail(dau_mau$playerHistory, 7)
        
        prev_avg <- mean(prev_7d$dau, na.rm = TRUE)
        curr_avg <- mean(curr_7d$dau, na.rm = TRUE)
        
        if (!is.na(prev_avg) && prev_avg > 0) {
          comparison_data$dau_trend_pct[i] <- ((curr_avg - prev_avg) / prev_avg) * 100
        }
      }
    }
    
    # Get latest units sold and concurrent players
    dates_to_check <- seq(Sys.Date() - 7, Sys.Date() - 1, by = "day")
    
    for (date in dates_to_check) {
      date_str <- as.character(date)
      
      # Units sold
      units <- tryCatch({
        units_data <- vgi_units_sold_by_date(date_str)
        units_data[units_data$steamAppId == game_id, ]
      }, error = function(e) NULL)
      
      if (!is.null(units) && nrow(units) > 0 && !is.na(units$unitsSold[1])) {
        comparison_data$total_units_sold[i] <- units$unitsSold[1]
        comparison_data$daily_units[i] <- units$dailyUnits[1]
        comparison_data$units_date[i] <- date_str
        break  # Use first available data
      }
    }
    
    # Concurrent players (same date as units if possible)
    if (!is.na(comparison_data$units_date[i])) {
      ccu <- tryCatch({
        ccu_data <- vgi_concurrent_players_by_date(comparison_data$units_date[i])
        ccu_data[ccu_data$steamAppId == game_id, ]
      }, error = function(e) NULL)
      
      if (!is.null(ccu) && nrow(ccu) > 0) {
        comparison_data$peak_concurrent[i] <- ccu$peakConcurrent[1]
        comparison_data$avg_concurrent[i] <- ccu$avgConcurrent[1]
      }
    }
    
    Sys.sleep(0.5)  # Rate limiting
  }
  
  # Calculate PPSU
  comparison_data$ppsu <- ifelse(
    !is.na(comparison_data$total_units_sold) & comparison_data$total_units_sold > 0,
    comparison_data$peak_concurrent / comparison_data$total_units_sold,
    NA
  )
  
  # Save to cache
  if (!dir.exists(".cache")) dir.create(".cache")
  saveRDS(list(date = Sys.Date(), data = comparison_data), cache_file)
  
  return(comparison_data)
}

# Get the comparison data
cat("Creating Marvel Rivals vs Overwatch 2 comparison table...\n")
comparison_data <- get_comparison_metrics()

# Prepare data for GT table
table_data <- comparison_data %>%
  select(
    game,
    latest_dau,
    avg_dau_7d,
    avg_dau_30d,
    dau_trend_pct,
    peak_concurrent,
    total_units_sold,
    daily_units,
    ppsu
  ) %>%
  mutate(
    # Format numbers
    latest_dau = comma(latest_dau, accuracy = 1),
    avg_dau_7d = comma(round(avg_dau_7d), accuracy = 1),
    avg_dau_30d = comma(round(avg_dau_30d), accuracy = 1),
    dau_trend_pct = ifelse(is.na(dau_trend_pct), "N/A", 
                          paste0(ifelse(dau_trend_pct > 0, "+", ""), 
                                 round(dau_trend_pct, 1), "%")),
    peak_concurrent = comma(peak_concurrent, accuracy = 1),
    total_units_sold = comma(total_units_sold, accuracy = 1),
    daily_units = comma(daily_units, accuracy = 1),
    ppsu = ifelse(is.na(ppsu), "N/A", format(round(ppsu, 4), nsmall = 4))
  )

# Create GT table
gt_table <- table_data %>%
  gt() %>%
  tab_header(
    title = "Marvel Rivals vs Overwatch 2: Performance Metrics",
    subtitle = paste("Data as of", Sys.Date())
  ) %>%
  cols_label(
    game = "Game",
    latest_dau = "Latest DAU",
    avg_dau_7d = "7-Day Avg DAU",
    avg_dau_30d = "30-Day Avg DAU",
    dau_trend_pct = "DAU Trend",
    peak_concurrent = "Peak CCU",
    total_units_sold = "Total Units",
    daily_units = "Daily Units",
    ppsu = "PPSU*"
  ) %>%
  tab_source_note(
    source_note = "*PPSU = Peak Players per Sold Unit (engagement efficiency metric)"
  ) %>%
  tab_source_note(
    source_note = paste("Source: Video Game Insights API | Updated:", Sys.Date())
  ) %>%
  # Apply GEC theme
  apply_gec_theme() %>%
  # Additional styling
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(columns = game)
  ) %>%
  # Highlight positive/negative trends
  tab_style(
    style = list(
      cell_text(color = "#27AE60", weight = "bold")
    ),
    locations = cells_body(
      columns = dau_trend_pct,
      rows = grepl("\\+", dau_trend_pct)
    )
  ) %>%
  tab_style(
    style = list(
      cell_text(color = "#E74C3C", weight = "bold")
    ),
    locations = cells_body(
      columns = dau_trend_pct,
      rows = grepl("^-", dau_trend_pct)
    )
  ) %>%
  # Center align numeric columns
  cols_align(
    align = "center",
    columns = -game
  ) %>%
  # Add footnote explaining metrics
  tab_footnote(
    footnote = "7-day trend comparing current week vs previous week",
    locations = cells_column_labels(columns = dau_trend_pct)
  ) %>%
  tab_footnote(
    footnote = "Peak Concurrent Users",
    locations = cells_column_labels(columns = peak_concurrent)
  )

# Save as PNG
output_file <- "marvel_vs_overwatch_comparison_table_api.png"
gtsave(gt_table, output_file, vwidth = 1200, vheight = 800)

cat(paste("\nTable saved as:", output_file, "\n"))

# Also create a simplified mobile-friendly version
mobile_table <- table_data %>%
  select(game, latest_dau, peak_concurrent, ppsu) %>%
  gt() %>%
  tab_header(
    title = "Marvel Rivals vs Overwatch 2",
    subtitle = "Key Performance Metrics"
  ) %>%
  cols_label(
    game = "Game",
    latest_dau = "Daily Active Users",
    peak_concurrent = "Peak Players",
    ppsu = "PPSU"
  ) %>%
  tab_source_note(
    source_note = paste("Updated:", Sys.Date())
  ) %>%
  apply_gec_theme() %>%
  tab_options(
    table.font.size = px(20),
    column_labels.font.size = px(18)
  ) %>%
  cols_align(
    align = "center",
    columns = -game
  )

# Save mobile version
mobile_output <- "marvel_vs_overwatch_mobile_api.png"
gtsave(mobile_table, mobile_output, vwidth = 800, vheight = 400)

cat(paste("Mobile version saved as:", mobile_output, "\n"))

# Print summary to console
cat("\nSummary:\n")
cat("========\n")
print(comparison_data[, c("game", "latest_dau", "peak_concurrent", "total_units_sold", "ppsu")])