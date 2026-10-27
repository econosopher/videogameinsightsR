# videogameinsightsR 0.3.0

The package now talks only to the v4 Video Game Insights API
(`https://vginsights.com/api/v4`, hosted by Sensor Tower; interactive docs at
<https://app.sensortower.com/vgi/api/v4/api-docs/>). v4 rows are
multi-platform (Steam, Xbox, PlayStation), carry VGI IDs and slugs alongside
Steam App IDs, and page with a cursor. This release breaks compatibility with
0.1.1 in the ways listed below.

## Breaking changes

* Removed, because v4 has no equivalent endpoint: `vgi_game_rankings()`,
  `vgi_top_games()`, `vgi_smart_search()` and
  `vgi_top_games_with_activity()` (all built on the v3 rankings endpoint).
* Removed in favour of v4 functions: `vgi_all_games_metadata()` (use
  `vgi_games_metadata(all_pages = TRUE)`), and the deprecated aliases
  `vgi_insights_entitlements()` and `vgi_entitlements_by_date()` (use
  `vgi_insights_units()` and `vgi_units_sold_by_date()`).
* `vgi_game_metadata()` and `vgi_game_metadata_batch()` return the
  `vgi_games_metadata()` shape: VGI's single `genre` / `subgenre`, Steam's
  `steam_genres` / `steam_subgenres`, per-platform `price_*`,
  `release_date_*` and `store_url_*` columns, `vgi_id`, `slug` and
  `platforms`. The v3 `genres`, `subgenres`, `price`, `release_date`,
  `full_release_date`, `steam_url` and `id` columns are gone. Unknown games
  return an empty tibble.
* `vgi_game_list()` returns `vgi_id`, `steam_app_id` (NA for games not on
  Steam) and `name`, sorted by VGI ID; the `id` alias is gone.
* `vgi_publisher_info()` / `vgi_developer_info()` read `/companies/{kind}`:
  Steam revenue and game counts plus `slug`, `country`,
  `games_released_{steam,playstation,xbox}` and `vgi_url`.
  `vgi_publisher_games()` / `vgi_developer_games()` still return Steam App
  IDs, resolved from VGI game IDs through `/games/metadata`.
* `vgi_all_publisher_games()` / `vgi_all_developer_games()` return VGI game
  IDs only; the `offset` argument and `id_type` column are gone.
  `vgi_publisher_list()` / `vgi_developer_list()` lose `min_games`.
* `vgi_player_overlap()` uses v4 `/player-overlap` (by Steam ID, VGI ID or
  slug) and loses `offset`. `vgi_all_games_player_overlap()` walks the
  catalogue by `cursor`, one game per request, instead of `offset`.
* The `version` argument is gone from every function, and
  `make_api_request()` no longer takes one. `options(vgi.base_url)` and
  `VGI_BASE_URL` take the API root; a trailing `/v3` or `/v4` is dropped.
* Per-game series start when VGI began tracking the game (often before
  release, with zero or null figures), and Steam early-access sales are
  counted on the launch day.
* List-returning functions snake_case their element names
  (`$player_history`, `$price_changes`).
* `vgi_filter_genre()` matches VGI's genre by default; pass
  `source = "steam"` for Steam's genres. `vgi_build_cache()` builds the cache
  from `/games/metadata` and loses `batch_size`. `vgi_fetch_by_ids()` keeps
  the `"metadata"` and `"active_players"` data types.

## New

* `vgi_games_metadata()`: metadata by `steam_app_ids`, `vgi_ids` or `slugs`,
  with cursor paging.
* `vgi_historical_data_by_date()`: one day's snapshot on every platform, with
  `countries` / `regions` breakdowns.
* `vgi_price_history()`: price periods on every platform, by Steam ID, VGI ID
  or slug.
* The multi-game player-insight functions accept `steam_app_ids`, `vgi_ids`,
  `slugs`, `limit`, `cursor` and `all_pages`; playtime also takes
  `countries` and `regions`. Company overview and list functions take
  `vgi_ids`, `slugs`, `cursor` and `all_pages`.

## Fixed

* Per-game time series no longer return every day twice (the API repeats a
  page when filtered by ID), and `vgi_insights_units()` /
  `vgi_insights_revenue()` read the v4 `unitsOwned*` / `premiumRevenue*`
  fields instead of erroring.
* `vgi_peak_ccu_by_ids()` and `vgi_peak_ccu_by_names()` work again and
  return `steam_app_id`.

## Tests

* The suite replays recorded v4 fixtures (`dev/record_fixtures.R`) shipped
  with the package, so `R CMD check` exercises the real parsing paths. Live
  smoke tests run only when `VGI_AUTH_TOKEN` is set.

# videogameinsightsR 0.1.1

* Fixed DESCRIPTION URL to follow redirect (vginsights.com -> app.sensortower.com/vgi/).
* Replaced `rappdirs` with `tools::R_user_dir()` for CRAN-preferred caching.
* Switched API-dependent examples from `\donttest{}` to `\dontrun{}`.

# videogameinsightsR 0.1.0

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

# videogameinsightsR 0.0.4

* Migrated package core to VGI API v4.
* Added `vgi_publishers_overview()` and `vgi_developers_overview()` for
  rich company data.
* Added `vgi_top_regions()` for regional performance data.
* Search functions are significantly faster via local caching.
* Re-architected data endpoints to use `historical-data` snapshot polling,
  handling API v4 shape changes gracefully.

# videogameinsightsR 0.0.3

* Added `vgi_game_summary_yoy()` for year-over-year comparisons.
* Support for flexible date specification (months or explicit dates).
* Automatic YoY growth percentage calculation.
* Added rate limiting utilities.

# videogameinsightsR 0.0.2

* Added multi-ID support in all data retrieval functions.
* Added `vgi_game_summary()` for comprehensive single-call analysis.
* Fixed field mapping for revenue and units sold endpoints.

# videogameinsightsR 0.0.1

* Initial release with core VGI API wrapper functions.
