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
