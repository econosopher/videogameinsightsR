## Update

This is an update of videogameinsightsR from 0.1.1 to 0.3.0 (0.2.0 was never
submitted). The 'Video Game Insights' API has moved to v4, and the package
now uses only v4 endpoints. Functions that depended on v3-only endpoints
(rankings) were removed, and several return shapes changed; NEWS.md lists
every breaking change.

## Test environments

* macOS, R 4.2.2 (local)

## R CMD check results

0 errors | 0 warnings | 1 note

* `checking for future file timestamps ... NOTE: unable to verify current
  time`. This comes from the local check environment, not the package.

## Reverse dependencies

There are no reverse dependencies on CRAN.

## Notes

* Examples call a paid, authenticated web API and are wrapped in
  `\dontrun{}`.
* Tests replay recorded API responses shipped under `tests/testthat/api/` and
  need no network access. Live tests are skipped on CRAN.
