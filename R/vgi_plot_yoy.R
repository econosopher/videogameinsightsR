#' Plot Year-over-Year Comparison
#'
#' Create visualizations for year-over-year game metric comparisons.
#'
#' @param yoy_comparison A vgi_yoy_comparison object from vgi_game_summary_yoy()
#' @param metric Character string. Which metric to plot: "ccu", "revenue", "units"
#' @param type Character string. Plot type: "line" (time series), "bar" (totals),
#'   or "growth" (YoY growth percentages)
#' @param games Character vector. Specific game names to include. If NULL, 
#'   includes all games.
#'
#' @return A ggplot2 object
#'
#' @export
#' @examples
#' \dontrun{
#' # Get year-over-year comparison
#' yoy_data <- vgi_game_summary_yoy(
#'   steam_app_ids = c(892970, 1145360),
#'   years = c(2023, 2024),
#'   start_month = "Jan",
#'   end_month = "Mar"
#' )
#' 
#' # Plot CCU comparison as line chart
#' vgi_plot_yoy(yoy_data, metric = "ccu", type = "line")
#' 
#' # Plot revenue totals as bar chart
#' vgi_plot_yoy(yoy_data, metric = "revenue", type = "bar")
#' 
#' # Plot year-over-year growth
#' vgi_plot_yoy(yoy_data, metric = "revenue", type = "growth")
#' }
vgi_plot_yoy <- function(yoy_comparison, 
                        metric = c("ccu", "revenue", "units"),
                        type = c("line", "bar", "growth"),
                        games = NULL) {
  
  # Check if ggplot2 is available
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting. Please install it.")
  }
  
  if (!requireNamespace("scales", quietly = TRUE)) {
    stop("Package 'scales' is required for plotting. Please install it.")
  }
  
  # Validate inputs
  if (!inherits(yoy_comparison, "vgi_yoy_comparison")) {
    stop("Input must be a vgi_yoy_comparison object from vgi_game_summary_yoy()")
  }
  
  metric <- match.arg(metric)
  type <- match.arg(type)
  
  # Select appropriate plotting function
  plot_fn <- switch(type,
    line = plot_yoy_time_series,
    bar = plot_yoy_bars,
    growth = plot_yoy_growth
  )
  
  return(plot_fn(yoy_comparison, metric, games))
}

#' Plot Year-over-Year Time Series
#'
#' @noRd
#' @keywords internal
plot_yoy_time_series <- function(yoy_comparison, metric, games = NULL) {
  # Get time series data
  ts_data <- yoy_comparison$time_series_comparison[[
    switch(metric,
      ccu = "concurrent",
      revenue = "revenue", 
      units = "units"
    )
  ]]
  
  if (is.null(ts_data) || nrow(ts_data) == 0) {
    stop(sprintf("No time series data available for metric '%s'", metric))
  }
  
  # Filter games if specified
  if (!is.null(games)) {
    # Get game names from comparison table
    game_mapping <- unique(yoy_comparison$comparison_table[, c("steamAppId", "name")])
    selected_ids <- game_mapping$steamAppId[game_mapping$name %in% games]
    ts_data <- ts_data[ts_data$steamAppId %in% selected_ids, ]
  }
  
  # Add game names
  game_names <- unique(yoy_comparison$comparison_table[, c("steamAppId", "name")])
  ts_data <- merge(ts_data, game_names, by = "steamAppId", all.x = TRUE)
  
  # Select appropriate value column
  value_col <- switch(metric,
    ccu = "peakConcurrent",
    revenue = "revenue",
    units = "unitsSold"
  )
  
  # Create line plot
  p <- ggplot2::ggplot(ts_data, ggplot2::aes(x = normalized_date, 
                                             y = .data[[value_col]], 
                                             color = as.factor(year))) +
    ggplot2::geom_line(size = 1.2) +
    ggplot2::geom_point(size = 2, alpha = 0.7) +
    ggplot2::facet_wrap(~name, scales = "free_y", ncol = 2) +
    ggplot2::scale_x_date(date_labels = "%b %d", date_breaks = "1 month") +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_color_brewer(palette = "Set1", name = "Year") +
    ggplot2::labs(
      title = sprintf("Year-over-Year %s Comparison", 
                     switch(metric, ccu = "Concurrent Users", 
                           revenue = "Revenue", units = "Units Sold")),
      subtitle = yoy_comparison$period,
      x = "Date (normalized)",
      y = switch(metric,
                ccu = "Peak Concurrent Users",
                revenue = "Revenue ($)",
                units = "Units Sold"),
      caption = sprintf("Data source: Video Game Insights API | %d API calls",
                       yoy_comparison$api_calls)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 16, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 12),
      legend.position = "bottom",
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
  
  return(p)
}

#' Plot Year-over-Year Bar Comparison
#'
#' @noRd
#' @keywords internal
plot_yoy_bars <- function(yoy_comparison, metric, games = NULL) {
  # Get comparison table
  comp_data <- yoy_comparison$comparison_table
  
  # Filter games if specified
  if (!is.null(games)) {
    comp_data <- comp_data[comp_data$name %in% games, ]
  }
  
  # Select metric column
  metric_col <- switch(metric,
    ccu = "avg_peak_ccu",
    revenue = "total_revenue",
    units = "total_units"
  )
  
  if (!(metric_col %in% names(comp_data))) {
    stop(sprintf("Metric '%s' not available in comparison data", metric))
  }
  
  # Create bar plot
  p <- ggplot2::ggplot(comp_data, 
                      ggplot2::aes(x = as.factor(year), 
                                  y = .data[[metric_col]], 
                                  fill = as.factor(year))) +
    ggplot2::geom_col(position = "dodge", alpha = 0.8) +
    ggplot2::facet_wrap(~name, scales = "free_y", ncol = 2) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_fill_brewer(palette = "Set2", guide = "none") +
    ggplot2::labs(
      title = sprintf("Year-over-Year %s Comparison", 
                     switch(metric, 
                           ccu = "Average Peak CCU",
                           revenue = "Total Revenue", 
                           units = "Total Units Sold")),
      subtitle = yoy_comparison$period,
      x = "Year",
      y = switch(metric,
                ccu = "Average Peak CCU",
                revenue = "Total Revenue ($)",
                units = "Total Units Sold"),
      caption = sprintf("Data source: Video Game Insights API | %d API calls",
                       yoy_comparison$api_calls)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 16, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 12),
      strip.text = ggplot2::element_text(face = "bold")
    )
  
  # Add value labels on bars
  p <- p + ggplot2::geom_text(
    ggplot2::aes(label = scales::comma(.data[[metric_col]])),
    position = ggplot2::position_dodge(width = 0.9),
    vjust = -0.5,
    size = 3
  )
  
  return(p)
}

#' Plot Year-over-Year Growth Rates
#'
#' @noRd
#' @keywords internal
plot_yoy_growth <- function(yoy_comparison, metric, games = NULL) {
  # Get comparison table
  comp_data <- yoy_comparison$comparison_table
  
  # Filter games if specified
  if (!is.null(games)) {
    comp_data <- comp_data[comp_data$name %in% games, ]
  }
  
  # Select growth column
  growth_col <- switch(metric,
    ccu = "avg_peak_ccu_yoy_growth",
    revenue = "total_revenue_yoy_growth",
    units = "total_units_yoy_growth"
  )
  
  if (!(growth_col %in% names(comp_data))) {
    stop(sprintf("Growth data for '%s' not available", metric))
  }
  
  # Filter out NA growth values (first year has no previous year)
  growth_data <- comp_data[!is.na(comp_data[[growth_col]]), ]
  
  if (nrow(growth_data) == 0) {
    stop("No year-over-year growth data available (need at least 2 years)")
  }
  
  # Create growth plot
  p <- ggplot2::ggplot(growth_data, 
                      ggplot2::aes(x = as.factor(year), 
                                  y = .data[[growth_col]],
                                  fill = .data[[growth_col]] > 0)) +
    ggplot2::geom_col(alpha = 0.8) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
    ggplot2::facet_wrap(~name, ncol = 2) +
    ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%")) +
    ggplot2::scale_fill_manual(values = c("TRUE" = "#2ecc71", "FALSE" = "#e74c3c"),
                               guide = "none") +
    ggplot2::labs(
      title = sprintf("Year-over-Year Growth: %s", 
                     switch(metric, 
                           ccu = "Peak CCU",
                           revenue = "Revenue", 
                           units = "Units Sold")),
      subtitle = yoy_comparison$period,
      x = "Year",
      y = "YoY Growth (%)",
      caption = sprintf("Data source: Video Game Insights API | %d API calls",
                       yoy_comparison$api_calls)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 16, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 12),
      strip.text = ggplot2::element_text(face = "bold")
    )
  
  # Add value labels
  p <- p + ggplot2::geom_text(
    ggplot2::aes(label = sprintf("%+.1f%%", .data[[growth_col]])),
    vjust = ifelse(growth_data[[growth_col]] > 0, -0.5, 1.5),
    size = 3.5,
    fontface = "bold"
  )
  
  return(p)
}