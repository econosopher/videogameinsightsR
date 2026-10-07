# VideoGameInsightsR tests

- `testthat/test-request.R` - request plumbing: base URL / version resolution,
  auth header, query construction, cursor and offset pagination, response
  helpers, deprecation warnings.
- `testthat/test-v3-endpoints.R` - v3 endpoint functions replayed against
  recorded fixtures.
- `testthat/test-v4-endpoints.R` - v4 endpoint functions replayed against
  recorded fixtures.
- `testthat/test-vgi-functions.R` - input validation contracts.
- `testthat/test-live.R` - live smoke tests; skipped unless `VGI_AUTH_TOKEN`
  is set and never run on CRAN.
- `testthat/api/v3/`, `testthat/api/v4/` - httptest2 fixtures (response
  bodies only), replayed by `with_vgi_fixtures()` in
  `testthat/helper-fixtures.R`, which points the package at the short root
  `https://api`. Directories ending in `old` (e.g. `units-sold`) are stored
  with a trailing `_` because R CMD build drops them; the helper restores them.
  Re-record with `Rscript dev/record_fixtures.R` after sourcing a token.
- `manual/` - ad-hoc scripts, excluded from the build.

```r
devtools::test()
devtools::test(filter = "v4-endpoints")
```
