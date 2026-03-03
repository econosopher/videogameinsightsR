#!/usr/bin/env Rscript

# Generate sample GT table for Marvel Rivals vs Overwatch comparison
# This uses example data to demonstrate the table format

library(gt)
library(dplyr)
library(scales)

# Create outputs directory if it doesn't exist
output_dir <- "outputs"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Function to apply GEC theme to GT table
apply_gec_theme <- function(gt_table) {
  gt_table %>%
    tab_options(
      table.font.names = "Arial",
      table.font.size = px(16),
      table.width = px(1200),
      heading.title.font.size = px(24),
      heading.title.font.weight = "bold",
      heading.subtitle.font.size = px(18),
      heading.align = "left",
      column_labels.font.size = px(16),
      column_labels.font.weight = "bold",
      column_labels.background.color = "#2C3E50",
      table.font.color = "#2C3E50",
      data_row.padding = px(8),
      row.striping.include_table_body = TRUE,
      row.striping.background_color = "#F8F9FA",
      table.border.top.width = px(3),
      table.border.top.color = "#2C3E50",
      table.border.bottom.width = px(3),
      table.border.bottom.color = "#2C3E50",
      source_notes.font.size = px(12)
    )
}

# Create sample data
sample_data <- data.frame(
  game = c("Marvel Rivals", "Overwatch 2"),
  latest_dau = c(125000, 89000),
  avg_dau_7d = c(118000, 91000),
  avg_dau_30d = c(105000, 95000),
  dau_trend_pct = c(12.4, -4.2),
  peak_concurrent = c(45000, 38000),
  total_units = c(2500000, 8500000),
  avg_daily_units = c(8500, 2100),
  avg_ppsu = c(0.0180, 0.0045),
  stringsAsFactors = FALSE
)

# Format the data for display
table_data <- sample_data %>%
  mutate(
    latest_dau = comma(latest_dau, accuracy = 1),
    avg_dau_7d = comma(avg_dau_7d, accuracy = 1),
    avg_dau_30d = comma(avg_dau_30d, accuracy = 1),
    dau_trend = ifelse(dau_trend_pct > 0, "↑", "↓"),
    dau_trend_pct = paste0(ifelse(dau_trend_pct > 0, "+", ""), 
                          round(dau_trend_pct, 1), "%"),
    peak_concurrent = comma(peak_concurrent, accuracy = 1),
    total_units = comma(total_units, accuracy = 1),
    avg_daily_units = comma(avg_daily_units, accuracy = 1),
    avg_ppsu = format(round(avg_ppsu, 4), nsmall = 4)
  )

# Create GT table
comparison_table <- table_data %>%
  gt() %>%
  tab_header(
    title = "Marvel Rivals vs Overwatch 2: Performance Comparison",
    subtitle = paste("180-day analysis | Updated:", Sys.Date())
  ) %>%
  cols_label(
    game = "Game",
    latest_dau = "Latest DAU",
    avg_dau_7d = "7-Day Avg",
    avg_dau_30d = "30-Day Avg",
    dau_trend = "",
    dau_trend_pct = "Trend",
    peak_concurrent = "Peak CCU",
    total_units = "Total Units",
    avg_daily_units = "Daily Units",
    avg_ppsu = "Avg PPSU"
  ) %>%
  cols_move(
    columns = dau_trend,
    after = dau_trend_pct
  ) %>%
  tab_source_note(
    source_note = "PPSU = Peak Players per Sold Unit (engagement efficiency metric)"
  ) %>%
  tab_source_note(
    source_note = "Source: Video Game Insights API | CCU = Concurrent Users"
  ) %>%
  tab_source_note(
    source_note = "Note: Sample data for demonstration purposes"
  ) %>%
  apply_gec_theme() %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(columns = game)
  ) %>%
  tab_style(
    style = list(
      cell_text(color = "#27AE60", weight = "bold")
    ),
    locations = cells_body(
      columns = dau_trend_pct,
      rows = dau_trend_pct == "+12.4%"
    )
  ) %>%
  tab_style(
    style = list(
      cell_text(color = "#E74C3C", weight = "bold")
    ),
    locations = cells_body(
      columns = dau_trend_pct,
      rows = dau_trend_pct == "-4.2%"
    )
  ) %>%
  tab_style(
    style = list(
      cell_text(color = "#27AE60", size = px(20))
    ),
    locations = cells_body(
      columns = dau_trend,
      rows = dau_trend == "↑"
    )
  ) %>%
  tab_style(
    style = list(
      cell_text(color = "#E74C3C", size = px(20))
    ),
    locations = cells_body(
      columns = dau_trend,
      rows = dau_trend == "↓"
    )
  ) %>%
  cols_align(
    align = "center",
    columns = -game
  ) %>%
  tab_footnote(
    footnote = "7-day trend comparing current week vs previous week",
    locations = cells_column_labels(columns = dau_trend_pct)
  ) %>%
  tab_footnote(
    footnote = "Peak Concurrent Users",
    locations = cells_column_labels(columns = peak_concurrent)
  )

# Save GT table as PNG
gt_output <- file.path(output_dir, "marvel_vs_overwatch_gt_comparison_sample.png")
gtsave(comparison_table, gt_output, vwidth = 1200, vheight = 600)

cat(paste("\nSample GT table saved as:", gt_output, "\n\n"))

# Display the data
print(sample_data)