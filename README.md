# videogameinsightsR

<!-- badges: start -->
[![R-CMD-check](https://github.com/econosopher/videogameinsightsR/workflows/R-CMD-check/badge.svg)](https://github.com/econosopher/videogameinsightsR/actions)
<!-- badges: end -->

R package for interfacing with the Video Game Insights API to fetch comprehensive gaming analytics data for Steam games.

## Installation

```r
# Install from GitHub
devtools::install_github("econosopher/videogameinsightsR")
```

## Authentication

Store your Video Game Insights API token as an environment variable:

```r
# Set in your R session
Sys.setenv(VGI_AUTH_TOKEN = "your_token_here")

# Or add to .Renviron for persistent storage
usethis::edit_r_environ()
# Add: VGI_AUTH_TOKEN="YOUR_SECRET_TOKEN_HERE"
```

## Overview

The videogameinsightsR package provides a comprehensive R interface to the Video Game Insights API, enabling:

- Concurrent player statistics (CCU)
- Daily and Monthly Active Users (DAU/MAU)
- Revenue and units sold data
- Game metadata including publishers and developers
- Game search functionality
- Comprehensive summary reports

## Core Functions

### Game Information

- `vgi_search_games()` - Search for games by title
- `vgi_game_metadata()` - Get detailed metadata for a single game

### Player Metrics

- `vgi_concurrent_players_by_date()` - Get concurrent player statistics (supports multiple IDs)
- `vgi_active_players_by_date()` - Get DAU/MAU data (supports multiple IDs)

### Revenue & Sales

- `vgi_revenue_by_date()` - Get revenue data (supports multiple IDs)
- `vgi_units_sold_by_date()` - Get units sold information (supports multiple IDs)

### Comprehensive Summary

- `vgi_game_summary()` - Get all available metrics in one call
- `vgi_game_summary_yoy()` - Compare metrics across multiple years

## Usage Examples

```r
library(videogameinsightsR)

# Search for games
valheim_results <- vgi_search_games("Valheim")

# Get game metadata
valheim_metadata <- vgi_game_metadata(892970)

# Get concurrent players for multiple games
battlefield_ids <- c(1517290, 1238810, 1238860, 1238840, 24960)
ccu_data <- vgi_concurrent_players_by_date(
  steam_app_ids = battlefield_ids,
  start_date = "2024-07-01",
  end_date = "2024-07-07"
)

# Get comprehensive summary
game_summary <- vgi_game_summary(
  steam_app_ids = battlefield_ids,
  start_date = "2024-07-01",
  end_date = "2024-07-31",
  metrics = c("concurrent", "active", "revenue", "units")  # Optional
)

# Access summary table with averages
print(game_summary$summary_table)

# Access time series data
print(game_summary$time_series$concurrent)
print(game_summary$time_series$revenue)

# Year-over-Year Comparison
yoy_comparison <- vgi_game_summary_yoy(
  steam_app_ids = c(892970, 1145360),
  years = c(2023, 2024, 2025),
  start_month = "Jan",
  end_month = "Mar"
)

# View comparison table with growth rates
print(yoy_comparison$comparison_table)

# Access time series data for custom visualization
revenue_data <- yoy_comparison$time_series_comparison$revenue
```

## Important Notes

### Steam App IDs Required

The Video Game Insights API requires explicit Steam App ID filtering to access production data. Without the `steamAppIds` parameter, endpoints return only demo data.

```r
# ❌ INCORRECT - Returns demo data
ccu_data <- vgi_concurrent_players_by_date("2024-07-01")

# ✅ CORRECT - Returns actual data
ccu_data <- vgi_concurrent_players_by_date(
  steam_app_ids = c(892970, 1145360),
  start_date = "2024-07-01"
)
```

### Date Availability

- **Daily Active Users (DAU)**: Available from 2024-03-18 onwards
- **Monthly Active Users (MAU)**: Available from 2024-03-23 onwards
- Functions handle these restrictions gracefully and return available data

### Response Format

The API returns different field names depending on the endpoint:
- Revenue: `revenueTotal` (total) and `revenueChange` (daily change)
- Units: `unitsSoldTotal` (total) and `unitsSoldChange` (daily change)
- CCU: `peakConcurrent` and `avgConcurrent`

## Rate Limiting and Batching

To reduce stress on the API, the package supports configurable rate limiting:

```r
# Set rate limiting via environment variables
Sys.setenv(VGI_BATCH_SIZE = "10")   # API calls per batch
Sys.setenv(VGI_BATCH_DELAY = "1.5") # Seconds between batches

# Or use the rate limiter directly
limiter <- create_rate_limiter(calls_per_batch = 5, delay_seconds = 2)
for (date in dates) {
  data <- vgi_concurrent_players_by_date(date)
  limiter$increment()  # Applies delay when threshold reached
}
```

Functions that make many sequential API calls (like `vgi_game_summary`) automatically
apply intelligent batching based on the request size.

### Configuration

You can control behavior via options or environment variables:

- Base URL: set `options(vgi.base_url = "https://vginsights.com/api/v3")` or `VGI_BASE_URL`
- Timeouts: `options(vgi.timeout = 30)`
- Retries: `options(vgi.retry_max_tries = 4)`
- Request cache TTL (seconds, GET only): `options(vgi.request_cache_ttl = 3600)` or `VGI_REQUEST_CACHE_TTL_SECONDS`
- Auto rate limiting (global): `options(vgi.auto_rate_limit = TRUE)`
- Calls per batch: `options(vgi.calls_per_batch = 10)` or `VGI_BATCH_SIZE`
- Delay seconds: `options(vgi.batch_delay = 1)` or `VGI_BATCH_DELAY`
- Verbose request logging: `options(vgi.verbose = TRUE)`

## Recent Updates

### Version 0.0.3 (2025-08-01)
- Added `vgi_game_summary_yoy()` for year-over-year comparisons
- Support for flexible date specification (months or explicit dates)
- Automatic calculation of year-over-year growth percentages
- Support for periods that cross year boundaries (e.g., holiday season)
- Returns comparison tables and normalized time series data
- Added rate limiting utilities to reduce API stress

### Version 0.0.2 (2025-08-01)
- Added support for multiple Steam App IDs in all data retrieval functions
- Fixed field mapping for revenue and units sold endpoints
- Added comprehensive `vgi_game_summary()` function
- Improved error handling for date availability constraints
- Fixed metadata endpoint to use correct URL structure
- Enhanced API response handling with automatic data frame conversion
- Fixed date handling in loops to prevent numeric conversion

## Development

This package is under active development. If you encounter any issues or have suggestions, please file an issue on [GitHub](https://github.com/econosopher/videogameinsightsR).

## License

MIT License