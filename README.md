# videogameinsightsR

<!-- badges: start -->
[![R-CMD-check](https://github.com/econosopher/videogameinsightsR/workflows/R-CMD-check/badge.svg)](https://github.com/econosopher/videogameinsightsR/actions)
<!-- badges: end -->

R package for the [Video Game Insights](https://app.sensortower.com/vgi/) API
(by Sensor Tower). Retrieve player counts, revenue, units, wishlists, reviews,
player insights and company data for games on Steam, Xbox and PlayStation,
all returned as tidy tibbles with snake\_case column names.

Since 0.3.0 the package uses only the v4 API: multi-platform rows, VGI IDs and
slugs alongside Steam App IDs, and cursor paging. Interactive docs:
<https://app.sensortower.com/vgi/api/v4/api-docs/>. See
`dev/api_coverage_2026-10.md` for the endpoint-by-endpoint mapping.

## Installation

```r
install.packages("videogameinsightsR")

# development version
devtools::install_github("econosopher/videogameinsightsR")
```

## Quick Start

```r
library(videogameinsightsR)

# Store your API token (once per session, or add to .Renviron)
Sys.setenv(VGI_AUTH_TOKEN = "your_token_here")

# Search and inspect a game
vgi_search_games("Valheim")
vgi_game_metadata(892970)
```

## Core Workflow

The typical workflow is **search -> inspect -> pull time series -> analyse**.

```r
library(dplyr)

# 1. Find a game
games <- vgi_search_games("Elden Ring")
app_id <- games$steam_app_id[1]

# 2. Pull metadata (one row per game; per-platform prices, dates and store URLs)
meta <- vgi_game_metadata(app_id)
meta[, c("name", "genre", "steam_genres", "release_date_steam")]

# 3. Pull the full daily history for one Steam game
ccu   <- vgi_insights_ccu(app_id)$player_history   # date, avg, median, max, min
rev   <- vgi_insights_revenue(app_id)              # date, revenue_change, revenue_total
units <- vgi_insights_units(app_id)                # date, units_sold_change, units_sold_total
wl    <- vgi_insights_wishlists(app_id)$wishlist_changes

# 4. Or grab every metric at once
hist <- vgi_historical_data(app_id)
names(hist)
#> "steam_app_id" "revenue" "units_sold" "concurrent_players" "active_players"
#> "reviews" "wishlists" "followers" "price_history" "daily"

# 5. One day across many games, every platform, optional country breakdown
snap <- vgi_historical_data_by_date("2025-06-01", steam_app_ids = c(app_id, 730))
by_country <- vgi_historical_data_by_date("2025-06-01", steam_app_ids = app_id,
                                          countries = c("US", "DE"))
```

## Genres

VGI assigns each game one genre and subgenre (`genre`, `subgenre`, for example
"Shooter" / "Shooter (General)"). Steam's own store genres, several per game,
are kept separately in `steam_genres` and `steam_subgenres`. The two can
disagree: ARC Raiders is an "Action" game to VGI but a "Shooter" subgenre on
Steam. `vgi_filter_genre()` filters the local cache on either
(`source = "vgi"` or `"steam"`).

## Function Families

### Game discovery and metadata

| Function | Description |
|---|---|
| `vgi_search_games()` | Search by title (local cache, then slug lookup) |
| `vgi_game_list()` | Full catalogue: `vgi_id`, `steam_app_id` (NA off Steam), `name` |
| `vgi_game_metadata()` | Metadata for one Steam game |
| `vgi_game_metadata_batch()` | Metadata for many Steam games |
| `vgi_games_metadata()` | Metadata by Steam ID, VGI ID or slug, with cursor paging |

### Snapshots by date

All accept `date` and optional `steam_app_ids`. `vgi_historical_data_by_date()`
returns every metric at once and supports `vgi_ids`, `slugs`, `countries` and
`regions`.

| Function | Columns |
|---|---|
| `vgi_concurrent_players_by_date()` | `peak_concurrent`, `avg_concurrent` |
| `vgi_active_players_by_date()` | `dau`, `mau`, `dau_mau_ratio` |
| `vgi_revenue_by_date()` | `revenue`, `daily_revenue` |
| `vgi_units_sold_by_date()` | `units_sold`, `daily_units` |
| `vgi_reviews_by_date()` | `positive_reviews`, `negative_reviews`, `positive_ratio` |
| `vgi_followers_by_date()` | `follower_count` |
| `vgi_wishlists_by_date()` | `wishlist_count` |

### Per-game history (Steam)

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
| `vgi_price_history()` | price periods on every platform, by Steam ID, VGI ID or slug |

### Player insights

| Function | Scope |
|---|---|
| `vgi_insights_playtime()`, `vgi_top_countries()`, `vgi_top_regions()`, `vgi_top_wishlist_countries()` | one Steam game |
| `vgi_player_overlap()` | one game, by Steam ID, VGI ID or slug |
| `vgi_all_games_player_overlap()` | walks the catalogue one game per request |
| `vgi_all_games_playtime()`, `vgi_all_games_top_countries()`, `vgi_all_games_regions()`, `vgi_all_games_wishlist_countries()` | many games; filter by `steam_app_ids`, `vgi_ids`, `slugs`; playtime also by `countries` / `regions` |

### Publishers and developers

```r
vgi_publishers_overview(slugs = "free-lives")  # per-platform revenue and genre mix
vgi_publisher_list(search = "valve")           # directory (id, name, slug)
vgi_publisher_info(28663)                      # Steam revenue and game counts, releases per platform
vgi_publisher_games(28663)                     # the publisher's Steam App IDs
vgi_all_publisher_games(vgi_ids = 28663)       # VGI game IDs on every platform
# ...and the matching vgi_developer*() functions
```

### Convenience functions

```r
summary <- vgi_game_summary(
  steam_app_ids = c(892970, 1245620),
  start_date = "2025-01-01",
  end_date   = "2025-01-31"
)
summary$summary_table

yoy <- vgi_game_summary_yoy(
  steam_app_ids = 892970,
  years = c(2024, 2025),
  start_month = "Jan",
  end_month = "Mar"
)
yoy$comparison_table
```

### Pagination

Functions that list many records take `limit` and `cursor` and return the next
cursor as an attribute; pass `all_pages = TRUE` to fetch everything.

```r
page1 <- vgi_games_metadata(limit = 500)
page2 <- vgi_games_metadata(limit = 500, cursor = attr(page1, "next_cursor"))
everything <- vgi_publisher_list(all_pages = TRUE)
```

## Configuration

```r
# API root (the /v4 segment is added; default shown)
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

- **Platforms**: on Xbox, VGI's "units owned" includes Game Pass players and
  other free copies; "paid units" excludes them. Xbox and PlayStation figures
  are VGI model estimates.
- **Steam App IDs**: pass explicit IDs to the snapshot-by-date functions.
  Without them the API returns its default handful of games.
- **API host**: requests go to `https://vginsights.com/api/v4`, which
  redirects to `https://app.sensortower.com/vgi/api/v4`.
- **Return types**: all data-returning functions produce tibbles with
  snake\_case columns.

## Development

```r
devtools::load_all()
devtools::test()
devtools::check(cran = TRUE)
```

Tests replay `httptest2` fixtures under `tests/testthat/api/v4/`
(re-record with `Rscript dev/record_fixtures.R`). Live smoke tests run only
when `VGI_AUTH_TOKEN` is set.

## License

MIT
