# Unified visualization generation script for videogameinsightsR
# This script combines all visualizations from the separate scripts

# Load required packages
if (!require(pacman)) install.packages("pacman")
pacman::p_load(
  videogameinsightsR,
  ggplot2,
  dplyr,
  tidyr,
  gt,
  scales,
  forcats
)

# Set API token
api_token <- Sys.getenv("VGI_AUTH_TOKEN")
if (api_token == "") {
  # Set the token directly if not in environment
  Sys.setenv(VGI_AUTH_TOKEN = "000008b2789cfb1597708ca43de8600d2cba0ed3ae")
  api_token <- Sys.getenv("VGI_AUTH_TOKEN")
}

# Set theme for consistent chart appearance
theme_set(theme_minimal() + 
  theme(
    plot.title = element_text(size = 16, face = "bold", margin = margin(b = 10)),
    plot.subtitle = element_text(size = 12, color = "gray40", margin = margin(b = 15)),
    plot.caption = element_text(size = 9, color = "gray50", hjust = 0),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.title.position = "panel",
    plot.subtitle.position = "panel"
  ))

# Create output directory
dir.create("outputs", showWarnings = FALSE, recursive = TRUE)

# Define GEC-style colors
gec_colors <- list(
  primary = "#E4F577",      # Bright Yellow-Green
  secondary = "#363D46",    # Dark Grey
  accent = "#23648D",       # Blue
  green = "#2D6F31",        # Green
  white = "#FCFFFE",        # Off-White
  black = "#000000",        # Black
  grey_light = "#DCDCDC",   # Light Grey
  grey_dark = "#363D46"     # Same as secondary
)

cat("Starting unified visualization generation...\n")
cat(paste(rep("=", 50), collapse = ""), "\n\n")

# ==========================================
# SECTION 1: TOP GAMES ANALYSIS
# ==========================================

cat("SECTION 1: TOP GAMES ANALYSIS\n")
cat(paste(rep("-", 30), collapse = ""), "\n")

# 1.1 Top Games by Revenue
tryCatch({
  cat("Creating top games by revenue chart...\n")
  top_revenue <- vgi_top_games("revenue", limit = 15)
  
  if (nrow(top_revenue) > 0) {
    p1 <- ggplot(top_revenue %>% slice_head(n = 10), 
                 aes(x = reorder(name, value), y = value)) +
      geom_col(fill = "#2E86AB", alpha = 0.8) +
      geom_text(aes(label = dollar(value, scale = 1e-6, suffix = "M")), 
                hjust = -0.1, size = 3.5) +
      coord_flip() +
      scale_y_continuous(labels = dollar_format(scale = 1e-6, suffix = "M"),
                         expand = expansion(mult = c(0, 0.15))) +
      labs(
        title = "Top 10 Games by Revenue",
        subtitle = "All platforms combined",
        x = NULL,
        y = "Revenue (USD)",
        caption = "Data source: Video Game Insights API"
      )
    
    ggsave("outputs/top_games_revenue_api.png", p1, width = 10, height = 8, dpi = 300)
    cat("✓ Saved: top_games_revenue_api.png\n")
  }
}, error = function(e) {
  cat("✗ Error creating revenue chart:", e$message, "\n")
})

# 1.2 Top Games by CCU
tryCatch({
  cat("Creating top games by CCU chart...\n")
  top_ccu <- vgi_top_games("ccu", platform = "steam", limit = 20)
  
  if (nrow(top_ccu) > 0) {
    p2 <- ggplot(top_ccu %>% slice_head(n = 15), 
                 aes(x = reorder(name, value), y = value)) +
      geom_segment(aes(xend = name, yend = 0), color = "gray70", size = 0.5) +
      geom_point(size = 4, color = "#F71735") +
      geom_text(aes(label = comma(value)), hjust = -0.3, size = 3) +
      coord_flip() +
      scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
      labs(
        title = "Top 15 Steam Games by Concurrent Users",
        subtitle = "Peak concurrent players",
        x = NULL,
        y = "Concurrent Users",
        caption = "Data source: Video Game Insights API | Platform: Steam"
      )
    
    ggsave("outputs/top_games_ccu_steam_api.png", p2, width = 10, height = 9, dpi = 300)
    cat("✓ Saved: top_games_ccu_steam_api.png\n")
  }
}, error = function(e) {
  cat("✗ Error creating CCU chart:", e$message, "\n")
})

# 1.3 Platform Comparison
tryCatch({
  cat("Creating platform comparison chart...\n")
  platforms <- c("steam", "playstation", "xbox", "nintendo")
  platform_data <- list()
  
  for (platform in platforms) {
    tryCatch({
      data <- vgi_top_games("revenue", platform = platform, limit = 5)
      data$platform <- platform
      platform_data[[platform]] <- data
    }, error = function(e) {
      cat(sprintf("  - Skipping %s: %s\n", platform, e$message))
    })
  }
  
  if (length(platform_data) > 0) {
    all_platforms <- bind_rows(platform_data)
    
    p3 <- ggplot(all_platforms, 
                 aes(x = reorder(name, -value), y = value, fill = platform)) +
      geom_col(position = "dodge", alpha = 0.8) +
      scale_fill_manual(values = c("steam" = "#171A21", 
                                   "playstation" = "#003791",
                                   "xbox" = "#107C10", 
                                   "nintendo" = "#E60012"),
                        labels = c("Steam", "PlayStation", "Xbox", "Nintendo")) +
      scale_y_continuous(labels = dollar_format(scale = 1e-6, suffix = "M")) +
      labs(
        title = "Top 5 Games by Platform",
        subtitle = "Revenue comparison across gaming platforms",
        x = NULL,
        y = "Revenue (USD Millions)",
        fill = "Platform",
        caption = "Data source: Video Game Insights API"
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggsave("outputs/platform_comparison_revenue_api.png", p3, width = 12, height = 8, dpi = 300)
    cat("✓ Saved: platform_comparison_revenue_api.png\n")
  }
}, error = function(e) {
  cat("✗ Error creating platform comparison:", e$message, "\n")
})

# 1.4 Top Games Revenue Table
tryCatch({
  cat("Creating top games revenue table...\n")
  top_games_data <- vgi_top_games("revenue", limit = 10)
  
  if (nrow(top_games_data) > 0) {
    summary_table <- top_games_data %>%
      slice_head(n = 10) %>%
      mutate(
        rank = row_number(),
        revenue_millions = value / 1e6,
        trend = case_when(
          rank <= 3 ~ "↑",
          rank <= 6 ~ "→",
          TRUE ~ "↓"
        )
      ) %>%
      select(rank, name, revenue_millions, trend) %>%
      gt() %>%
      tab_header(
        title = md("**TOP 10 GAMES BY REVENUE**"),
        subtitle = "All platforms combined | 2024 data"
      ) %>%
      fmt_currency(
        columns = revenue_millions,
        currency = "USD",
        decimals = 1,
        suffixing = TRUE
      ) %>%
      cols_label(
        rank = "RANK",
        name = "GAME TITLE",
        revenue_millions = "REVENUE",
        trend = "TREND"
      ) %>%
      tab_options(
        table.font.names = "Arial, Helvetica, sans-serif",
        table.font.size = px(12),
        heading.padding = px(8),
        heading.border.bottom.style = "none",
        heading.title.font.size = px(18),
        heading.subtitle.font.size = px(13),
        column_labels.background.color = gec_colors$secondary,
        column_labels.font.weight = "bold",
        column_labels.font.size = px(13),
        column_labels.font.color = gec_colors$white,
        table.background.color = gec_colors$white,
        data_row.padding = px(10),
        table.border.top.style = "solid",
        table.border.top.width = px(3),
        table.border.top.color = gec_colors$primary,
        table.border.bottom.style = "solid",
        table.border.bottom.width = px(3),
        table.border.bottom.color = gec_colors$primary,
        table.border.left.style = "solid",
        table.border.left.width = px(3),
        table.border.left.color = gec_colors$primary,
        table.border.right.style = "solid",
        table.border.right.width = px(3),
        table.border.right.color = gec_colors$primary,
        row.striping.include_table_body = TRUE,
        row.striping.background_color = gec_colors$grey_light
      ) %>%
      tab_style(
        style = list(
          cell_fill(color = gec_colors$primary, alpha = 0.2),
          cell_text(weight = "bold", color = gec_colors$secondary)
        ),
        locations = cells_body(rows = 1:3)
      ) %>%
      tab_style(
        style = list(
          cell_text(size = px(16), weight = "bold", align = "center")
        ),
        locations = cells_body(columns = trend)
      ) %>%
      tab_source_note("Data source: Video Game Insights API | Generated with videogameinsightsR")
    
    gtsave(summary_table, "outputs/top_games_revenue_table_api.png", vwidth = 700)
    cat("✓ Saved: top_games_revenue_table_api.png\n")
  }
}, error = function(e) {
  cat("✗ Error creating revenue table:", e$message, "\n")
})

# ==========================================
# SECTION 2: GAME METADATA ANALYSIS
# ==========================================

cat("\nSECTION 2: GAME METADATA ANALYSIS\n")
cat(paste(rep("-", 30), collapse = ""), "\n")

# Fetch game metadata
game_data <- NULL
tryCatch({
  cat("Fetching game metadata...\n")
  game_data <- vgi_game_metadata(892970)  # This returns multiple games
}, error = function(e) {
  cat("✗ Error fetching game metadata:", e$message, "\n")
})

if (!is.null(game_data) && nrow(game_data) > 0) {
  
  # 2.1 Game Prices Comparison
  tryCatch({
    cat("Creating game prices comparison chart...\n")
    price_data <- game_data %>%
      slice_head(n = 15) %>%
      filter(!is.na(price), price > 0) %>%
      mutate(name = substr(name, 1, 30))
    
    if (nrow(price_data) > 0) {
      p4 <- ggplot(price_data, aes(x = reorder(name, price), y = price)) +
        geom_col(fill = gec_colors$accent, alpha = 0.8) +
        geom_text(aes(label = dollar(price)), hjust = -0.1, size = 3.5) +
        coord_flip() +
        scale_y_continuous(labels = dollar_format(), expand = expansion(mult = c(0, 0.15))) +
        labs(
          title = "Game Prices on Steam",
          subtitle = "Sample of games from Video Game Insights API",
          x = NULL,
          y = "Price (USD)",
          caption = "Data source: Video Game Insights API"
        )
      
      ggsave("outputs/game_prices_comparison_api.png", p4, width = 10, height = 8, dpi = 300)
      cat("✓ Saved: game_prices_comparison_api.png\n")
    }
  }, error = function(e) {
    cat("✗ Error creating price comparison:", e$message, "\n")
  })
  
  # 2.2 Genre Distribution
  tryCatch({
    cat("Creating genre distribution chart...\n")
    genre_data <- game_data %>%
      slice_head(n = 50) %>%
      count(genres, sort = TRUE) %>%
      slice_head(n = 10)
    
    if (nrow(genre_data) > 0) {
      p5 <- ggplot(genre_data, aes(x = reorder(genres, n), y = n)) +
        geom_segment(aes(xend = genres, yend = 0), color = "gray70", linewidth = 0.5) +
        geom_point(size = 4, color = gec_colors$primary) +
        geom_text(aes(label = n), hjust = -0.5, size = 3.5) +
        coord_flip() +
        scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
        labs(
          title = "Game Genres Distribution",
          subtitle = "Most common genres in the dataset",
          x = NULL,
          y = "Number of Games",
          caption = "Data source: Video Game Insights API"
        )
      
      ggsave("outputs/genre_distribution_api.png", p5, width = 10, height = 8, dpi = 300)
      cat("✓ Saved: genre_distribution_api.png\n")
    }
  }, error = function(e) {
    cat("✗ Error creating genre distribution:", e$message, "\n")
  })
  
  # 2.3 Publisher Distribution
  tryCatch({
    cat("Creating publisher distribution chart...\n")
    publisher_summary <- game_data %>%
      slice_head(n = 200) %>%
      mutate(publisher_name = sapply(publishers, function(x) {
        if (is.list(x) && length(x) > 0 && is.list(x[[1]]) && !is.null(x[[1]]$companyName)) {
          return(x[[1]]$companyName)
        }
        return("Independent")
      })) %>%
      count(publisher_name, sort = TRUE) %>%
      mutate(
        percentage = n / sum(n) * 100,
        publisher_name = case_when(
          percentage < 2 ~ "Other",
          TRUE ~ publisher_name
        )
      ) %>%
      group_by(publisher_name) %>%
      summarise(
        n = sum(n),
        percentage = sum(percentage)
      ) %>%
      arrange(desc(n))
    
    p6 <- ggplot(publisher_summary, aes(x = "", y = percentage, fill = reorder(publisher_name, percentage))) +
      geom_col(width = 1, color = "white", linewidth = 0.5) +
      coord_polar(theta = "y") +
      scale_fill_manual(values = c(
        gec_colors$accent, gec_colors$primary, gec_colors$green, 
        gec_colors$secondary, "#F71735", "#8B5A3C", "#FF6B6B"
      )) +
      geom_text(aes(label = paste0(round(percentage, 1), "%")), 
                position = position_stack(vjust = 0.5),
                color = "white", fontface = "bold") +
      theme_void() +
      theme(legend.position = "right") +
      labs(
        title = "Game Publishers Market Share",
        subtitle = "Distribution of games by publisher",
        fill = "Publisher",
        caption = "Data source: Video Game Insights API"
      )
    
    ggsave("outputs/publisher_distribution_api.png", p6, width = 10, height = 8, dpi = 300)
    cat("✓ Saved: publisher_distribution_api.png\n")
  }, error = function(e) {
    cat("✗ Error creating publisher distribution:", e$message, "\n")
  })
  
  # 2.4 Top Publishers Bar Chart
  tryCatch({
    cat("Creating top publishers chart...\n")
    publisher_data <- game_data %>%
      slice_head(n = 100) %>%
      mutate(publisher_name = sapply(publishers, function(x) {
        if (is.list(x) && length(x) > 0) {
          if (is.list(x[[1]]) && !is.null(x[[1]]$companyName)) {
            return(x[[1]]$companyName)
          }
        }
        return("Unknown")
      })) %>%
      count(publisher_name, sort = TRUE) %>%
      filter(publisher_name != "Unknown") %>%
      slice_head(n = 10)
    
    if (nrow(publisher_data) > 0) {
      p7 <- ggplot(publisher_data, aes(x = reorder(publisher_name, n), y = n)) +
        geom_col(fill = gec_colors$green, alpha = 0.8) +
        geom_text(aes(label = n), hjust = -0.1, size = 3.5) +
        coord_flip() +
        scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
        labs(
          title = "Top Game Publishers",
          subtitle = "By number of games in the dataset",
          x = NULL,
          y = "Number of Games",
          caption = "Data source: Video Game Insights API"
        )
      
      ggsave("outputs/top_publishers_api.png", p7, width = 10, height = 8, dpi = 300)
      cat("✓ Saved: top_publishers_api.png\n")
    }
  }, error = function(e) {
    cat("✗ Error creating top publishers:", e$message, "\n")
  })
  
  # 2.5 Release Timeline
  tryCatch({
    cat("Creating release timeline...\n")
    release_data <- game_data %>%
      filter(!is.na(releaseDate)) %>%
      mutate(
        release_year = as.numeric(format(as.Date(releaseDate), "%Y")),
        release_decade = floor(release_year / 10) * 10
      ) %>%
      filter(release_year >= 1990, release_year <= 2024) %>%
      count(release_year)
    
    if (nrow(release_data) > 0) {
      p8 <- ggplot(release_data, aes(x = release_year, y = n)) +
        geom_area(fill = gec_colors$primary, alpha = 0.3) +
        geom_line(color = gec_colors$accent, linewidth = 1) +
        geom_point(color = gec_colors$accent, size = 2) +
        scale_x_continuous(breaks = seq(1990, 2024, 5)) +
        labs(
          title = "Game Releases Over Time",
          subtitle = "Number of games released per year",
          x = "Release Year",
          y = "Number of Games",
          caption = "Data source: Video Game Insights API"
        )
      
      ggsave("outputs/release_timeline_api.png", p8, width = 12, height = 6, dpi = 300)
      cat("✓ Saved: release_timeline_api.png\n")
    }
  }, error = function(e) {
    cat("✗ Error creating release timeline:", e$message, "\n")
  })
  
  # 2.6 Price Distribution
  tryCatch({
    cat("Creating price distribution chart...\n")
    price_ranges <- game_data %>%
      filter(!is.na(price), price > 0) %>%
      mutate(
        price_range = case_when(
          price == 0 ~ "Free",
          price < 5 ~ "$0-4.99",
          price < 10 ~ "$5-9.99",
          price < 20 ~ "$10-19.99",
          price < 30 ~ "$20-29.99",
          price < 40 ~ "$30-39.99",
          price < 60 ~ "$40-59.99",
          TRUE ~ "$60+"
        ),
        price_range = factor(price_range, levels = c(
          "Free", "$0-4.99", "$5-9.99", "$10-19.99", 
          "$20-29.99", "$30-39.99", "$40-59.99", "$60+"
        ))
      ) %>%
      count(price_range)
    
    if (nrow(price_ranges) > 0) {
      p9 <- ggplot(price_ranges, aes(x = price_range, y = n)) +
        geom_col(fill = gec_colors$green, alpha = 0.8) +
        geom_text(aes(label = n), vjust = -0.5, size = 3.5) +
        labs(
          title = "Game Price Distribution",
          subtitle = "Number of games in each price range",
          x = "Price Range",
          y = "Number of Games",
          caption = "Data source: Video Game Insights API"
        ) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      ggsave("outputs/price_distribution_api.png", p9, width = 10, height = 8, dpi = 300)
      cat("✓ Saved: price_distribution_api.png\n")
    }
  }, error = function(e) {
    cat("✗ Error creating price distribution:", e$message, "\n")
  })
  
  # 2.7 Subgenre Distribution
  tryCatch({
    cat("Creating subgenre analysis...\n")
    subgenre_data <- game_data %>%
      filter(!is.na(subgenres)) %>%
      separate_rows(subgenres, sep = ", ") %>%
      count(subgenres, sort = TRUE) %>%
      slice_head(n = 15)
    
    if (nrow(subgenre_data) > 0) {
      p10 <- ggplot(subgenre_data, aes(x = reorder(subgenres, n), y = n)) +
        geom_col(aes(fill = n), show.legend = FALSE) +
        scale_fill_gradient(low = gec_colors$grey_light, high = gec_colors$accent) +
        coord_flip() +
        geom_text(aes(label = n), hjust = -0.1, size = 3.5) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
        labs(
          title = "Most Common Game Subgenres",
          subtitle = "Top 15 subgenres in the dataset",
          x = NULL,
          y = "Number of Games",
          caption = "Data source: Video Game Insights API"
        )
      
      ggsave("outputs/subgenre_distribution_api.png", p10, width = 10, height = 8, dpi = 300)
      cat("✓ Saved: subgenre_distribution_api.png\n")
    }
  }, error = function(e) {
    cat("✗ Error creating subgenre distribution:", e$message, "\n")
  })
}

# ==========================================
# SECTION 3: SEARCH AND ANALYSIS
# ==========================================

cat("\nSECTION 3: SEARCH AND ANALYSIS\n")
cat(paste(rep("-", 30), collapse = ""), "\n")

# 3.1 RPG Games Timeline
tryCatch({
  cat("Creating RPG games timeline...\n")
  rpg_games <- vgi_search_games("rpg", limit = 30)
  
  if (nrow(rpg_games) > 0 && "release_date" %in% names(rpg_games)) {
    rpg_timeline <- rpg_games %>%
      filter(!is.na(release_date)) %>%
      mutate(release_year = as.numeric(format(as.Date(release_date), "%Y"))) %>%
      filter(release_year >= 2015) %>%
      slice_head(n = 20)
    
    p11 <- ggplot(rpg_timeline, aes(x = release_year, y = reorder(name, release_year))) +
      geom_point(size = 3, color = "#8B5A3C") +
      geom_segment(aes(xend = release_year, yend = name), 
                   xend = min(rpg_timeline$release_year) - 0.5,
                   color = "gray80", linetype = "dotted") +
      scale_x_continuous(breaks = seq(2015, 2024, 1)) +
      labs(
        title = "RPG Games Release Timeline",
        subtitle = "Search results for 'RPG' query",
        x = "Release Year",
        y = NULL,
        caption = "Data source: Video Game Insights API"
      ) +
      theme(panel.grid.major.y = element_blank())
    
    ggsave("outputs/rpg_games_timeline_api.png", p11, width = 10, height = 8, dpi = 300)
    cat("✓ Saved: rpg_games_timeline_api.png\n")
  }
}, error = function(e) {
  cat("✗ Error creating RPG timeline:", e$message, "\n")
})

# 3.2 Metrics Comparison Heatmap
tryCatch({
  cat("Creating metrics comparison heatmap...\n")
  metrics <- c("revenue", "units", "ccu")
  comparison_data <- list()
  
  for (metric in metrics) {
    tryCatch({
      data <- vgi_top_games(metric, limit = 5)
      data$metric <- metric
      data$normalized_value <- scales::rescale(data$value, to = c(0, 100))
      comparison_data[[metric]] <- data
    }, error = function(e) {
      cat(sprintf("  - Skipping metric %s: %s\n", metric, e$message))
    })
  }
  
  if (length(comparison_data) > 0) {
    metrics_df <- bind_rows(comparison_data)
    
    p12 <- ggplot(metrics_df, aes(x = metric, y = name, fill = normalized_value)) +
      geom_tile(color = "white", size = 0.5) +
      geom_text(aes(label = round(normalized_value, 0)), color = "white", size = 4) +
      scale_fill_gradient2(low = "#2C3E50", mid = "#E74C3C", high = "#F39C12", midpoint = 50) +
      labs(
        title = "Game Performance Across Different Metrics",
        subtitle = "Normalized scores (0-100 scale)",
        x = "Metric",
        y = NULL,
        fill = "Score",
        caption = "Data source: Video Game Insights API"
      ) +
      theme(
        axis.text.x = element_text(angle = 0),
        legend.position = "right"
      )
    
    ggsave("outputs/metrics_comparison_heatmap_api.png", p12, width = 10, height = 6, dpi = 300)
    cat("✓ Saved: metrics_comparison_heatmap_api.png\n")
  }
}, error = function(e) {
  cat("✗ Error creating metrics heatmap:", e$message, "\n")
})

# ==========================================
# SECTION 4: TABLES
# ==========================================

cat("\nSECTION 4: TABLES\n")
cat(paste(rep("-", 30), collapse = ""), "\n")

# 4.1 Game Details Table
if (!is.null(game_data) && nrow(game_data) > 0) {
  tryCatch({
    cat("Creating game details table...\n")
    table_data <- game_data %>%
      slice_head(n = 10) %>%
      select(name, genres, price, releaseDate) %>%
      mutate(
        rank = row_number(),
        releaseDate = as.Date(releaseDate)
      ) %>%
      select(rank, name, genres, price, releaseDate)
    
    game_table <- table_data %>%
      gt() %>%
      tab_header(
        title = md("**VIDEO GAME INSIGHTS SAMPLE DATA**"),
        subtitle = "Top 10 games from API response"
      ) %>%
      fmt_currency(
        columns = price,
        currency = "USD"
      ) %>%
      fmt_date(
        columns = releaseDate,
        date_style = "iso"
      ) %>%
      cols_label(
        rank = "RANK",
        name = "GAME TITLE",
        genres = "GENRE",
        price = "PRICE",
        releaseDate = "RELEASE DATE"
      ) %>%
      tab_options(
        table.font.names = "Arial, Helvetica, sans-serif",
        table.font.size = px(12),
        heading.title.font.size = px(18),
        heading.subtitle.font.size = px(13),
        column_labels.background.color = gec_colors$secondary,
        column_labels.font.weight = "bold",
        table.border.top.width = px(3),
        table.border.top.color = gec_colors$primary,
        table.border.bottom.width = px(3),
        table.border.bottom.color = gec_colors$primary
      ) %>%
      tab_style(
        style = list(
          cell_text(color = gec_colors$white, weight = "bold")
        ),
        locations = cells_column_labels(everything())
      ) %>%
      tab_source_note("Data source: Video Game Insights API | Generated with videogameinsightsR")
    
    gtsave(game_table, "outputs/game_details_table_api.png", vwidth = 800)
    cat("✓ Saved: game_details_table_api.png\n")
  }, error = function(e) {
    cat("✗ Error creating game details table:", e$message, "\n")
  })
  
  # 4.2 Genre Analysis Table
  tryCatch({
    cat("Creating genre analysis table...\n")
    comparison_data <- game_data %>%
      slice_head(n = 100) %>%
      group_by(genres) %>%
      summarise(
        game_count = n(),
        avg_price = mean(price, na.rm = TRUE),
        min_price = min(price, na.rm = TRUE),
        max_price = max(price, na.rm = TRUE),
        publishers = n_distinct(sapply(publishers, function(x) {
          if (is.list(x) && length(x) > 0 && is.list(x[[1]])) x[[1]]$companyName else "Unknown"
        }))
      ) %>%
      arrange(desc(game_count)) %>%
      slice_head(n = 8)
    
    if (nrow(comparison_data) > 0) {
      comparison_table <- comparison_data %>%
        gt() %>%
        tab_header(
          title = md("**GAME GENRE ANALYSIS**"),
          subtitle = "Price and publisher statistics by genre"
        ) %>%
        fmt_currency(
          columns = c(avg_price, min_price, max_price),
          currency = "USD"
        ) %>%
        fmt_number(
          columns = game_count,
          decimals = 0
        ) %>%
        cols_label(
          genres = "GENRE",
          game_count = "GAMES",
          avg_price = "AVG PRICE",
          min_price = "MIN",
          max_price = "MAX",
          publishers = "PUBLISHERS"
        ) %>%
        tab_options(
          table.font.names = "Arial, Helvetica, sans-serif",
          table.font.size = px(12),
          heading.title.font.size = px(18),
          heading.subtitle.font.size = px(13),
          column_labels.background.color = gec_colors$secondary,
          column_labels.font.weight = "bold",
          table.border.top.width = px(3),
          table.border.top.color = gec_colors$primary,
          table.border.bottom.width = px(3),
          table.border.bottom.color = gec_colors$primary,
          data_row.padding = px(10)
        ) %>%
        tab_style(
          style = list(
            cell_text(color = gec_colors$white, weight = "bold")
          ),
          locations = cells_column_labels(everything())
        ) %>%
        tab_style(
          style = list(
            cell_fill(color = gec_colors$primary, alpha = 0.2)
          ),
          locations = cells_body(
            columns = avg_price,
            rows = avg_price > 20
          )
        ) %>%
        data_color(
          columns = game_count,
          colors = scales::col_numeric(
            palette = c("white", gec_colors$accent),
            domain = NULL
          )
        ) %>%
        tab_source_note("Data source: Video Game Insights API | Generated with videogameinsightsR")
      
      gtsave(comparison_table, "outputs/genre_analysis_table_api.png", vwidth = 900)
      cat("✓ Saved: genre_analysis_table_api.png\n")
    }
  }, error = function(e) {
    cat("✗ Error creating genre analysis table:", e$message, "\n")
  })
}

# 4.3 Platform Comparison Table
if (exists("all_platforms") && nrow(all_platforms) > 0) {
  tryCatch({
    cat("Creating platform comparison table...\n")
    platform_summary <- all_platforms %>%
      group_by(platform) %>%
      summarise(
        total_revenue = sum(value, na.rm = TRUE),
        avg_revenue = mean(value, na.rm = TRUE),
        game_count = n(),
        top_game = first(name)
      ) %>%
      ungroup() %>%
      mutate(
        market_share = total_revenue / sum(total_revenue) * 100,
        platform_name = case_when(
          platform == "steam" ~ "Steam",
          platform == "playstation" ~ "PlayStation",
          platform == "xbox" ~ "Xbox",
          platform == "nintendo" ~ "Nintendo",
          TRUE ~ platform
        )
      ) %>%
      arrange(desc(total_revenue))
    
    platform_table <- platform_summary %>%
      select(platform_name, top_game, avg_revenue, market_share) %>%
      gt() %>%
      tab_header(
        title = md("**GAMING PLATFORM PERFORMANCE**"),
        subtitle = "Revenue analysis by platform"
      ) %>%
      fmt_currency(
        columns = avg_revenue,
        currency = "USD",
        scale_by = 1e-6,
        decimals = 1,
        suffixing = FALSE
      ) %>%
      fmt_percent(
        columns = market_share,
        scale_values = FALSE,
        decimals = 1
      ) %>%
      cols_label(
        platform_name = "PLATFORM",
        top_game = "TOP GAME",
        avg_revenue = "AVG REVENUE (M)",
        market_share = "MARKET SHARE"
      ) %>%
      tab_options(
        table.font.names = "Arial, Helvetica, sans-serif",
        table.font.size = px(12),
        heading.title.font.size = px(18),
        heading.subtitle.font.size = px(13),
        column_labels.background.color = gec_colors$accent,
        column_labels.font.color = gec_colors$white,
        column_labels.font.weight = "bold",
        table.background.color = gec_colors$white,
        data_row.padding = px(10),
        table.border.top.width = px(3),
        table.border.top.color = gec_colors$accent,
        table.border.bottom.width = px(3),
        table.border.bottom.color = gec_colors$accent
      ) %>%
      tab_style(
        style = list(
          cell_fill(color = gec_colors$green, alpha = 0.3),
          cell_text(weight = "bold")
        ),
        locations = cells_body(
          columns = market_share,
          rows = market_share > 30
        )
      ) %>%
      data_color(
        columns = market_share,
        colors = scales::col_numeric(
          palette = c(gec_colors$grey_light, gec_colors$accent),
          domain = c(0, 100)
        )
      ) %>%
      tab_source_note("Data source: Video Game Insights API")
    
    gtsave(platform_table, "outputs/platform_comparison_table_api.png", vwidth = 800)
    cat("✓ Saved: platform_comparison_table_api.png\n")
  }, error = function(e) {
    cat("✗ Error creating platform table:", e$message, "\n")
  })
}

cat("\n\n")
cat(paste(rep("=", 50), collapse = ""), "\n")
cat("All visualizations have been generated successfully!\n")
cat("Output files saved to: outputs/\n")
cat(paste(rep("=", 50), collapse = ""), "\n")

# List all generated files
output_files <- list.files("outputs", pattern = "\\.(png|jpg)$", full.names = FALSE)
if (length(output_files) > 0) {
  cat("\nGenerated files:\n")
  for (file in output_files) {
    cat("  -", file, "\n")
  }
} else {
  cat("\nNo output files were generated.\n")
}