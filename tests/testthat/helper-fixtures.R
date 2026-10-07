# Replay recorded API responses (tests/testthat/api/{v3,v4}, recorded by
# dev/record_fixtures.R). httptest2 maps a request URL to a fixture path by
# dropping the scheme, so the tests point the package at the short root
# "https://api"; that keeps fixture paths under the 100-byte limit R CMD check
# enforces on tarballs. A wrong path or query string fails the fixture lookup,
# so every replayed call also pins the URL contract.
#
# R CMD build silently drops directories whose names end in "old" (it treats
# them as backups), which would remove .../units-sold/. Such directories are
# stored with a trailing "_" and restored under their real name in a
# per-session copy of the fixture tree.
vgi_fixture_dir <- local({
  dir <- NULL
  function() {
    if (is.null(dir)) {
      dir <<- tempfile("vgi-fixtures-")
      dir.create(dir)
      file.copy(testthat::test_path("api"), dir, recursive = TRUE)
      stored <- list.dirs(dir)
      for (d in rev(stored[grepl("old_$", stored)])) file.rename(d, sub("_$", "", d))
    }
    dir
  }
})

with_vgi_fixtures <- function(code) {
  withr::with_options(
    list(vgi.base_url = "https://api", vgi.request_cache_ttl = 0, vgi.auto_rate_limit = FALSE,
         httptest2.mock.paths = vgi_fixture_dir()),
    httptest2::with_mock_api(code)
  )
}
