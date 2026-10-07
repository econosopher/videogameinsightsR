# VideoGameInsightsR

<!-- badges: start -->
[![R-CMD-check](https://github.com/econosopher/VideoGameInsightsR/workflows/R-CMD-check/badge.svg)](https://github.com/econosopher/VideoGameInsightsR/actions)
<!-- badges: end -->

R package for the [Video Game Insights](https://app.sensortower.com/vgi/) API
(by Sensor Tower). Retrieve player counts, revenue, units sold, wishlists,
reviews, and more for Steam games -- all returned as tidy tibbles with
snake\_case column names.

The package covers both API generations:

- **v3** -- per-Steam-game endpoints (CCU, DAU/MAU, revenue, units,
  reviews, wishlists, followers, price history, rankings, player overlap)
  with offset/limit paging. Per-game series functions reach these with
  `version = "v3"`. Interactive docs: <https://app.sensortower.com/vgi/api/v3/api-docs/>.
- **v4** (default for per-game series and player insights) -- multi-platform
  (Steam, Xbox, PlayStation) metadata, companies, player insights, full daily
  histories and snapshots by date, with VGI IDs / slugs and cursor paging.
  Interactive docs: <https://app.sensortower.com/vgi/api/v4/api-docs/>.

See `dev/api_coverage_2026-10.md` for the endpoint-by-endpoint mapping.

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

# 3. Pull the full daily history for one game (v4; add version = "v3" for v3)
ccu   <- vgi_insights_ccu(app_id)$player_history   # date, avg, median, max, min
rev   <- vgi_insights_revenue(app_id)              # date, revenue_change, revenue_total
units <- vgi_insights_units(app_id)                # date, units_sold_change, units_sold_total
wl    <- vgi_insights_wishlists(app_id)$wishlist_changes

# 4. Or grab every metric at once with the historical data endpoint
hist <- vgi_historical_data(app_id)
names(hist)
#> "steam_app_id" "revenue" "units_sold" "concurrent_players" "active_players"
#> "reviews" "wishlists" "followers" "price_history" "daily"

# 5. One day across many games (v4 snapshot, optional country breakdown)
snap <- vgi_historical_data_by_date("2025-06-01", steam_app_ids = c(app_id, 730))
by_country <- vgi_historical_data_by_date("2025-06-01", steam_app_ids = app_id,
                                          countries = c("US", "DE"))
```

## Function Families

### Game Discovery

| Function | Description |
|---|---|
| `vgi_search_games()` | Search by title (uses local cache + API fallback) |
| `vgi_game_list()` | Full game catalogue (v4 adds `vgi_id`) |
| `vgi_top_games()` | Top games by ranking metric |
| `vgi_game_rankings()` | Rank / percentile table, or one game's ranks |
| `vgi_game_metadata()` | v3 metadata for a single Steam game |
| `vgi_games_metadata()` | v4 multi-platform metadata by Steam id, VGI id or slug |

### Snapshots by date

All accept `date` and optional `steam_app_ids` to filter (v4 snapshot).
`vgi_historical_data_by_date()` returns every metric at once and supports
`vgi_ids`, `slugs`, `countries` and `regions`; `vgi_snapshot_by_date()` gives
direct access to the v3 `{date}` endpoints.

| Function | Columns |
|---|---|
| `vgi_concurrent_players_by_date()` | `peak_concurrent`, `avg_concurrent` |
| `vgi_active_players_by_date()` | `dau`, `mau`, `dau_mau_ratio` |
| `vgi_revenue_by_date()` | `revenue`, `daily_revenue` |
| `vgi_units_sold_by_date()` | `units_sold`, `daily_units` |
| `vgi_reviews_by_date()` | `positive_reviews`, `negative_reviews`, `positive_ratio` |
| `vgi_followers_by_date()` | `follower_count` |
| `vgi_wishlists_by_date()` | `wishlist_count` |

### Per-game history (v3)

```r
hist <- vgi_historical_data(892970)
hist$revenue        # tibble: date, revenue, daily_revenue
hist$active_players # tibble: date, dau, mau
hist$price_history  # tibble: date, price_initial, price_final
hist$daily          # the raw daily table with every metric
```

| Function | Returns |
|---|---|
| `vgi_insights_ccu()` | `$player_history`: date, avg, median, max, min |
| `vgi_insights_dau_mau()` | `$player_history`: date, dau, mau |
| `vgi_insights_revenue()` | date, revenue_change, revenue_total |
| `vgi_insights_units()` | date, units_sold_change, units_sold_total |
| `vgi_insights_reviews()` | date, positive, negative, total, positive_ratio |
| `vgi_insights_wishlists()` | `$wishlist_changes`: date, wishlists_total, wishlists_change |
| `vgi_insights_followers()` | `$followers_change`: date, followers_total, followers_change |
| `vgi_insights_price_history()` | `$price_changes`: currency, price_initial, price_final, first_date, last_date |
| `vgi_price_history()` | v4 multi-platform price periods by Steam id, VGI id or slug |

### Player insights

| Function | Scope |
|---|---|
| `vgi_insights_playtime()`, `vgi_top_countries()`, `vgi_top_regions()`, `vgi_top_wishlist_countries()` | one Steam game (v3) |
| `vgi_player_overlap()` | one game; v3 by Steam id, v4 by `vgi_id` / `slug` |
| `vgi_all_games_playtime()`, `vgi_all_games_top_countries()`, `vgi_all_games_regions()`, `vgi_all_games_wishlist_countries()` | many games (v4); filter by `steam_app_ids`, `vgi_ids`, `slugs`; playtime also by `countries` / `regions` |

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
vgi_publishers_overview(slugs = "free-lives")  # v4: per-platform revenue, genre mix
vgi_publisher_list(search = "valve")           # v4 directory (id, name, slug)
vgi_publisher_info(28663)                      # v3 summary metrics
vgi_publisher_games(28663)                     # v3 Steam App IDs for the publisher
vgi_publishers(limit = 100)                    # v3 catalogue with metrics
# ...and the matching vgi_developer*() functions
```

### Pagination

v4 functions take `limit` and `cursor` and return the next cursor as an
attribute; pass `all_pages = TRUE` to fetch everything.

```r
page1 <- vgi_games_metadata(limit = 500)
page2 <- vgi_games_metadata(limit = 500, cursor = attr(page1, "next_cursor"))
everything <- vgi_publisher_list(all_pages = TRUE)
```

## Configuration

```r
# API root (the version segment is added per call; default shown)
options(vgi.base_url = "https://vginsights.com/api")

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

- **Steam App IDs required**: Pass explicit IDs to snapshot-by-date functions.
  Without them the API returns its default handful of games.
- **API host**: requests go to `https://vginsights.com/api/{v3,v4}`, which
  redirects to `https://app.sensortower.com/vgi/api/{v3,v4}`. The old
  `vginsights.com/api` documentation page is gone; use the Sensor Tower
  Swagger pages linked above.
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

Tests replay `httptest2` fixtures under `tests/testthat/{v3,v4}/`
(re-record with `Rscript dev/record_fixtures.R`). Live smoke tests run only
when `VGI_AUTH_TOKEN` is set.

## License

MIT
