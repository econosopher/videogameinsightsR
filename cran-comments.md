## Resubmission

This is a resubmission. Changes requested in CRAN feedback were addressed:

* Added missing `\value` sections for exported methods:
  `apply_rate_limit()` and `print.vgi_yoy_comparison()`.
* Expanded return-value documentation for year-over-year comparison objects,
  including class/structure and field meanings.
* Removed commented-out code from `vgi_all_games_metadata()` examples.
* Replaced non-suppressible `cat()` usage in non-print functions with
  suppressible messaging (`message()`), while keeping `cat()` only in print methods.
* Updated cache behavior to avoid default writes in user home-space paths.
  Default cache location now uses a temporary directory, with optional override
  via `options(VideoGameInsightsR.cache_dir = "...")` or `VGI_CACHE_DIR`.
* Updated examples that write files to use `tempdir()` paths.
* Updated examples that change graphical parameters (`par(...)`) to restore
  original settings after plotting.
* Removed runtime package installation (`install.packages()`) from manual test scripts.

## Test environments

* macOS Sequoia 15.3, R 4.2.2 (local)

## R CMD check results

0 errors | 0 warnings | 2 notes

Notes:
* This is a new submission.
* `unable to verify current time` (environment/time-check note on local run).
