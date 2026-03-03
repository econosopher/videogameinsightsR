# VideoGameInsightsR

<!-- badges: start -->
[![R-CMD-check](https://github.com/econosopher/VideoGameInsightsR/workflows/R-CMD-check/badge.svg)](https://github.com/econosopher/VideoGameInsightsR/actions)
<!-- badges: end -->

R package for the [Video Game Insights](https://app.sensortower.com/vgi/) API. Retrieve
player counts, revenue, units sold, wishlists, reviews, and more for Steam games
-- all returned as tidy tibbles with snake\_case column names.

## Installation

```r
devtools::install_github("econosopher/VideoGameInsightsR")
```

## Quick Start

```r
library(VideoGameInsightsR)

# Store your API token (once per session, or add to .Renviron)
Sys.setenv(VGI_AUTH_TOKEN = "your_token_here")

# Search and inspect a game
vgi_search_games("Valheim")
vgi_game_metadata(892970)
```

## Core Workflow

The typical workflow is **search -> inspect -> pull time series -> analyse**.
Every function returns a tibble so results pipe naturally.

```r
library(dplyr)

# 1. Find a game
games <- vgi_search_games("Elden Ring")
app_id <- games$steam_app_id[1]

# 2. Pull metadata
meta <- vgi_game_metadata(app_id)
meta$name
#> "ELDEN RING"

# 3. Get daily time-series data
ccu   <- vgi_concurrent_players_by_date("2025-06-01", steam_app_ids = app_id)
dau   <- vgi_active_players_by_date("2025-06-01", steam_app_ids = app_id)
rev   <- vgi_revenue_by_date("2025-06-01", steam_app_ids = app_id)
units <- vgi_units_sold_by_date("2025-06-01", steam_app_ids = app_id)

# 4. Or grab everything at once with the historical data endpoint
hist <- vgi_historical_data(app_id)
names(hist)
#> "steam_app_id" "revenue" "units_sold" "concurrent_players"
#> "active_players" "reviews" "wishlists" "followers" "price_history"
```

## Function Families

### Game Discovery

| Function | Description |
|---|---|
| `vgi_search_games()` | Search by title (uses local cache + API fallback) |
| `vgi_game_list()` | Full game catalogue |
| `vgi_top_games()` | Top games by various ranking metrics |
| `vgi_game_metadata()` | Detailed metadata for a single game |

### Time Series (by date)

All accept `date` and optional `steam_app_ids` to filter.

| Function | Columns |
|---|---|
| `vgi_concurrent_players_by_date()` | `peak_concurrent`, `avg_concurrent` |
| `vgi_active_players_by_date()` | `dau`, `mau`, `dau_mau_ratio` |
| `vgi_revenue_by_date()` | `revenue`, `daily_revenue` |
| `vgi_units_sold_by_date()` | `units_sold`, `daily_units` |
| `vgi_reviews_by_date()` | `positive_reviews`, `negative_reviews`, `positive_ratio` |
| `vgi_followers_by_date()` | `follower_count` |
| `vgi_wishlists_by_date()` | `wishlist_count` |

### Comprehensive Historical Data

```r
hist <- vgi_historical_data(892970)
hist$revenue        # tibble: date, revenue, daily_revenue
hist$active_players # tibble: date, dau, mau
hist$price_history  # tibble: date, price_initial, price_final
```

### Insights (per-game summaries)

| Function | Returns |
|---|---|
| `vgi_insights_ccu()` | CCU history |
| `vgi_insights_dau_mau()` | Active player history |
| `vgi_insights_revenue()` | Revenue time series |
| `vgi_insights_units()` | Units sold time series |
| `vgi_insights_playtime()` | Average/median playtime + ranges |
| `vgi_insights_player_regions()` | Player geographic distribution |
| `vgi_insights_price_history()` | Price change periods by currency |

### Convenience Functions

```r
# Everything in one call
summary <- vgi_game_summary(
  steam_app_ids = c(892970, 1245620),
  start_date = "2025-01-01",
  end_date   = "2025-01-31"
)
summary$summary_table
summary$time_series$concurrent

# Year-over-year comparison
yoy <- vgi_game_summary_yoy(
  steam_app_ids = 892970,
  years = c(2024, 2025),
  start_month = "Jan",
  end_month = "Mar"
)
yoy$comparison_table
```

### Publishers and Developers

```r
vgi_publishers_overview()   # All publishers with key metrics
vgi_developers_overview()   # All developers with key metrics
vgi_publisher_list()        # Publisher names and IDs
vgi_developer_list()        # Developer names and IDs
```

## Configuration

```r
# Base URL (default: v4)
options(vgi.base_url = "https://vginsights.com/api/v4")

# Timeouts and retries
options(vgi.timeout = 30)
options(vgi.retry_max_tries = 4)

# Request caching (seconds, GET only)
options(vgi.request_cache_ttl = 3600)

# Rate limiting
options(vgi.auto_rate_limit = TRUE)
options(vgi.calls_per_batch = 10)
options(vgi.batch_delay = 1)

# Verbose logging
options(vgi.verbose = TRUE)
```

## Important Notes

- **Steam App IDs required**: Pass explicit IDs to time-series functions.
  Without them the API returns demo/aggregate data only.
- **DAU/MAU availability**: DAU from 2024-03-18, MAU from 2024-03-23.
- **Column names**: All output uses snake\_case (e.g. `steam_app_id`,
  `peak_concurrent`, `daily_revenue`). This is a breaking change from v0.0.x.
- **Return types**: All data-returning functions produce tibbles.

## Development

```r
devtools::load_all()
devtools::test()
devtools::check(cran = TRUE)
```

Tests use `httptest2` fixtures under `tests/testthat/vginsights.com/`.

## License

MIT
