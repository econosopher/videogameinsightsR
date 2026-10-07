# VideoGameInsightsR 0.2.0

## API sync (October 2026)

The package now talks to both generations of the Video Game Insights API,
which is hosted by Sensor Tower. Interactive docs:
<https://app.sensortower.com/vgi/api/v3/api-docs/> and
<https://app.sensortower.com/vgi/api/v4/api-docs/>. The old
`vginsights.com/api` documentation page no longer exists; the API itself still
answers at `https://vginsights.com/api/{v3,v4}` (301 to
`app.sensortower.com/vgi/api`). The endpoint-by-endpoint mapping lives in
`dev/api_coverage_2026-10.md`; the specs are saved under `dev/specs/`.

### Fixed

* The per-game time series (`vgi_historical_data()`, `vgi_insights_ccu()`,
  `vgi_insights_dau_mau()`, `vgi_insights_revenue()`, `vgi_insights_units()`,
  `vgi_insights_reviews()`, `vgi_insights_wishlists()`,
  `vgi_insights_followers()`) stay on the v4 `/historical-data` endpoint, which
  returns a game's full daily history. With `steamAppIds` the API ignores
  `cursor` and repeats the same page; 0.1.1 followed it once and returned every
  day twice. Pages that repeat the previous `nextCursor` are now discarded,
  and rows are filtered to the Steam game's world-wide slice.
  `vgi_insights_units()` and `vgi_insights_revenue()` no longer error, and
  read the v4 `unitsOwned*` / `premiumRevenue*` fields (the deprecated
  `unitsSold*` / `revenue*` names are a fallback).
* Every per-game time-series and player-insight function gains
  `version = c("v4", "v3")`. `"v3"` calls the per-game v3 endpoints
  (`/engagement/...`, `/commercial-performance/...`, `/reception/...`,
  `/interest-level/...`, `/historical-data/games/{id}`,
  `/player-insights/games/{id}/...`), which remain live; v4 has no
  equivalents of those endpoint families.
* `vgi_insights_price_history()` reads the v4 `/price-history` endpoint by
  default (Steam rows, every currency) instead of rebuilding USD periods
  from daily snapshots.
* `vgi_insights_playtime()`, `vgi_insights_player_regions()`,
  `vgi_top_regions()`, `vgi_top_countries()` and
  `vgi_top_wishlist_countries()` keep the v4 multi-game endpoints by default.
* `vgi_player_overlap()` defaults to the v3 per-game endpoint: v4
  `/player-overlap` currently returns null overlap figures, and 0.1.1 sent it
  the wrong parameter (`steamAppIds`) and returned no rows.
* `vgi_game_rankings()` uses `/games/rankings` (and `/games/{id}/rankings`
  via the new `steam_app_id` argument) instead of recomputing ranks from a
  snapshot. The `date` argument is deprecated and ignored.
* `vgi_game_metadata()` uses `/games/{id}/metadata` and returns the full v3
  record (tags, modes, themes, developers and publishers as list-columns).
  Unknown games return an empty tibble instead of an error.
* `vgi_publisher_info()`, `vgi_publisher_games()`, `vgi_developer_info()`
  and `vgi_developer_games()` are back on v3 and return tibbles / Steam App
  IDs as documented (the v4 versions had silently returned VGI game IDs).
* `vgi_all_games_metadata()` pages `/games/metadata` with real
  `offset`/`limit` semantics.
* `vgi_top_games()` and `vgi_peak_ccu_by_ids()` were broken by the 0.1.0
  snake_case change and work again.
* `vgi_smart_game_search()` resolves company games through the v3 game-id
  endpoints so the Steam IDs it returns can be looked up.

### New (v4 coverage)

* Version-aware request layer: `make_api_request(version = "v3"|"v4")`,
  `get_base_url(version)`, a cursor pagination helper
  (`all_pages = TRUE`, `next_cursor` attribute on results) and an
  offset/limit pager for v3 catalogues.
* `vgi_games_metadata()`: v4 `/games/metadata` by `steam_app_ids`,
  `vgi_ids` or `slugs`, with per-platform price / release-date / store-URL
  columns.
* `vgi_historical_data_by_date()`: v4 `/historical-data` snapshot with
  `countries` / `regions` breakdowns and cursor paging.
* `vgi_price_history()`: v4 `/price-history` by Steam id, VGI id or slug.
* `vgi_player_overlap()` gains `vgi_id`, `slug` and `version` arguments for
  the v4 `/player-overlap` endpoint.
* `vgi_all_games_top_countries()`, `vgi_all_games_regions()`,
  `vgi_all_games_wishlist_countries()` and `vgi_all_games_playtime()` accept
  `steam_app_ids`, `vgi_ids`, `slugs`, `limit`, `cursor` and `all_pages`;
  playtime also accepts `countries` and `regions`.
* `vgi_publishers_overview()` / `vgi_developers_overview()` gain
  `all_pages`; `vgi_publisher_list()` / `vgi_developer_list()` gain
  `vgi_ids`, `slugs`, `cursor`, `all_pages` and `version`;
  `vgi_all_publisher_games()` / `vgi_all_developer_games()` gain the same
  filters plus a v3 mode that returns Steam App IDs.
* `vgi_publishers()` and `vgi_developers()`: v3 company catalogues.
* `vgi_snapshot_by_date()`: direct access to the eight v3 `{date}`
  endpoints.
* `vgi_steam_market_data()` and `vgi_game_list()` gain a `version`
  argument (v4 default).

### Changed

* v4 per-game series start when VGI began tracking the game (often months
  before release, with zero or null figures) rather than at release; use
  `version = "v3"` or filter by date for launch-window series.
* List-returning functions (`vgi_insights_ccu()`, `vgi_historical_data()`,
  `vgi_player_overlap()`, ...) now snake_case their element names
  (`$player_history`, `$price_changes`), matching the 0.1.0 column
  convention and the README. `$playerHistory`-style access no longer works.
* `vgi_insights_price_history()` returns one `price_changes` tibble with a
  `currency` column for both the single-currency and all-currency calls.
* `options(vgi.base_url = ...)` and `VGI_BASE_URL` now take the API root
  (`https://vginsights.com/api`); a value ending in `/v3` or `/v4` is still
  accepted and the suffix is dropped.
* Deprecated: `vgi_insights_entitlements()` and
  `vgi_entitlements_by_date()` (the API has no entitlements endpoint; they
  alias the units-sold functions and warn once per session),
  `vgi_game_rankings(date=)`, `vgi_*_list(min_games=)`.

### Tests

* The test suite replays recorded v3/v4 fixtures (`dev/record_fixtures.R`)
  and ships them in the package so `R CMD check` exercises the real
  parsing paths. Live smoke tests run only when `VGI_AUTH_TOKEN` is set.

# VideoGameInsightsR 0.1.1

* Fixed DESCRIPTION URL to follow redirect (vginsights.com -> app.sensortower.com/vgi/).
* Replaced `rappdirs` with `tools::R_user_dir()` for CRAN-preferred caching.
* Switched API-dependent examples from `\donttest{}` to `\dontrun{}`.

# VideoGameInsightsR 0.1.0

## Breaking Changes

* All column names are now **snake_case** instead of camelCase.
  For example, `steamAppId` is now `steam_app_id`, `peakConcurrent` is
  `peak_concurrent`, and `dailyRevenue` is `daily_revenue`.
* All data-returning functions now return **tibbles** instead of plain
  `data.frame` objects.

## New Features

* Added `.vgi_clean_names()` internal utility that converts API column
  names to snake_case and coerces results to tibbles in one step.
* README rewritten with focused workflow examples and a comprehensive
  function reference table.

## Improvements

* Consistent tidyverse-friendly output across all 67 exported functions.
* Function parameters already used snake_case; output now matches.

---

# VideoGameInsightsR 0.0.4

* Migrated package core to VGI API v4.
* Added `vgi_publishers_overview()` and `vgi_developers_overview()` for
  rich company data.
* Added `vgi_top_regions()` for regional performance data.
* Search functions are significantly faster via local caching.
* Re-architected data endpoints to use `historical-data` snapshot polling,
  handling API v4 shape changes gracefully.

# VideoGameInsightsR 0.0.3

* Added `vgi_game_summary_yoy()` for year-over-year comparisons.
* Support for flexible date specification (months or explicit dates).
* Automatic YoY growth percentage calculation.
* Added rate limiting utilities.

# VideoGameInsightsR 0.0.2

* Added multi-ID support in all data retrieval functions.
* Added `vgi_game_summary()` for comprehensive single-call analysis.
* Fixed field mapping for revenue and units sold endpoints.

# VideoGameInsightsR 0.0.1

* Initial release with core VGI API wrapper functions.
