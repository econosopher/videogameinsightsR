# Record httptest2 fixtures for the testthat suite.
#
# Run from the package root with VGI_AUTH_TOKEN set in the environment:
#   Rscript dev/record_fixtures.R
#
# Responses are captured under a temporary directory as
# vginsights.com/api/{v3,v4}/... and then copied to tests/testthat/api/...,
# where tests/testthat/helper-fixtures.R replays them with the package pointed
# at the short root "https://api" (keeps paths under R CMD check's 100-byte
# limit). Fixtures hold response bodies only; the api-key request header is
# never written. Keep the request set small: every call here is replayed
# offline by the tests.

if (!requireNamespace("httptest2", quietly = TRUE)) stop("Install httptest2 to record fixtures.")
suppressMessages(devtools::load_all(quiet = TRUE))
if (!nzchar(Sys.getenv("VGI_AUTH_TOKEN"))) stop("VGI_AUTH_TOKEN not set; cannot record fixtures.")

options(vgi.verbose = FALSE, vgi.request_cache_ttl = 0, vgi.auto_rate_limit = TRUE,
        vgi.base_url = "https://vginsights.com/api")
capture_dir <- tempfile("vgi-fixtures-")
httptest2::.mockPaths(capture_dir)

# Never write the key: drop the api-key header and scrub its value from
# anything httptest2 saves (URL, headers, body).
local({
  token <- Sys.getenv("VGI_AUTH_TOKEN")
  httptest2::set_redactor(function(response) {
    response <- httptest2::redact_headers(response, "api-key")
    httptest2::gsub_response(response, token, "REDACTED", fixed = TRUE)
  })
})

dressmaker <- 4019220L   # released 2026-09-21, publisher Free Lives (28663), vgiId 2047657
free_lives <- 28663L

httptest2::start_capturing()
# --- v3 ---
vgi_game_metadata(dressmaker)
try(vgi_game_metadata(999999999), silent = TRUE)          # 404 -> .R fixture
vgi_game_rankings(steam_app_id = dressmaker)
vgi_game_rankings(limit = 3)
vgi_game_rankings(limit = 6)                               # vgi_top_games(limit = 3) fetches 2x
vgi_historical_data(dressmaker, version = "v3")
vgi_insights_ccu(dressmaker, version = "v3")
vgi_insights_revenue(dressmaker, version = "v3")
vgi_insights_units(dressmaker, version = "v3")
vgi_insights_reviews(dressmaker, version = "v3")
vgi_insights_wishlists(dressmaker, version = "v3")
vgi_insights_price_history(dressmaker, currency = "USD", version = "v3")
vgi_insights_playtime(dressmaker, version = "v3")
vgi_top_countries(dressmaker, version = "v3")
vgi_player_overlap(dressmaker, limit = 5)
vgi_publisher_info(free_lives)
vgi_publisher_games(free_lives)
vgi_publishers(limit = 2)
# --- v4 ---
vgi_games_metadata(steam_app_ids = dressmaker)
vgi_games_metadata(slugs = "dressmaker")
vgi_games_metadata(limit = 2)
vgi_historical_data_by_date("2026-09-28", steam_app_ids = dressmaker)
vgi_historical_data_by_date("2026-09-28", steam_app_ids = dressmaker, countries = c("US", "DE"))
vgi_concurrent_players_by_date("2026-09-28", steam_app_ids = dressmaker)   # by-date wrappers (limit = 50)
vgi_price_history(steam_app_id = dressmaker)                # also vgi_insights_price_history() v4
vgi_historical_data(dressmaker)                             # v4 history: every vgi_insights_* series
vgi_insights_playtime(dressmaker)                           # v4 per-game player insights
vgi_top_regions(dressmaker)
vgi_top_wishlist_countries(dressmaker)
vgi_publishers_overview(vgi_ids = free_lives)
vgi_all_publisher_games(vgi_ids = free_lives)
vgi_publisher_list(limit = 3)
vgi_all_games_top_countries(steam_app_ids = dressmaker)
vgi_all_games_playtime(steam_app_ids = dressmaker, countries = "US")
vgi_steam_market_data()
httptest2::stop_capturing()

captured <- file.path(capture_dir, "vginsights.com", "api")
for (f in list.files(captured, recursive = TRUE)) {
  dst <- file.path("tests/testthat/api", f)
  dir.create(dirname(dst), recursive = TRUE, showWarnings = FALSE)
  file.copy(file.path(captured, f), dst, overwrite = TRUE)
}
# R CMD build drops directories ending in "old"; store them with a trailing
# "_" (tests/testthat/helper-fixtures.R restores the real name at test time).
for (d in rev(list.dirs("tests/testthat/api"))) {
  if (grepl("old$", d)) {
    unlink(paste0(d, "_"), recursive = TRUE)
    file.rename(d, paste0(d, "_"))
  }
}

# Long daily histories are trimmed to the launch window after recording so
# the fixtures stay small: rows with date < 2026-09-15 are dropped from
# api/v3/historical-data/games/4019220.json,
# api/v3/interest-level/wishlists/games/4019220.json and the v4
# api/v4/historical-data-fbd7e9.json (historical-data?steamAppIds=4019220).

files <- list.files("tests/testthat/api", recursive = TRUE, full.names = TRUE)
cat(sprintf("%7d  %s\n", file.size(files), files), sep = "")
leaks <- files[vapply(files, function(f) any(grepl(Sys.getenv("VGI_AUTH_TOKEN"), readLines(f, warn = FALSE), fixed = TRUE)), logical(1))]
if (length(leaks) > 0) stop("api key found in ", length(leaks), " fixture file(s); do not commit them.")
