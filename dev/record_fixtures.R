"# Record HTTP fixtures for tests using httptest2"

if (!requireNamespace("httptest2", quietly = TRUE)) {
  stop("Please install httptest2 to record fixtures.")
}

# Load package code for development session
if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(quiet = TRUE)
} else if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(quiet = TRUE)
} else {
  stop("Please install devtools or pkgload to load the package code for recording.")
}

# Load env from parent if present to get VGI_AUTH_TOKEN
parent_env_renviron <- file.path(dirname(getwd()), ".Renviron")
if (file.exists(parent_env_renviron)) readRenviron(parent_env_renviron)
if (requireNamespace("dotenv", quietly = TRUE)) {
  parent_env_env <- file.path(dirname(getwd()), ".env")
  if (file.exists(parent_env_env)) dotenv::load_dot_env(parent_env_env)
}

if (!nzchar(Sys.getenv("VGI_AUTH_TOKEN"))) {
  stop("VGI_AUTH_TOKEN not set; cannot record fixtures.")
}

fixtures_dir <- file.path("tests", "fixtures")
if (!dir.exists(fixtures_dir)) dir.create(fixtures_dir, recursive = TRUE)

# Note: Using basic start_capturing/stop_capturing for broad compatibility

message("Starting capture into ", fixtures_dir)
httptest2::start_capturing()
on.exit(httptest2::stop_capturing(), add = TRUE)

# Keep requests small and deterministic
options(vgi.verbose = FALSE)
options(vgi.request_cache_ttl = 0)
options(vgi.auto_rate_limit = TRUE)

# Minimal set of API calls used across tests
token <- Sys.getenv("VGI_AUTH_TOKEN")

# 1) Game list (limit small if supported by API; if ignored, still fine)
try({
  invisible(vgi_game_list(auth_token = token))
}, silent = TRUE)

# 2) Search (filters locally but still calls game list internally)
try({
  invisible(vgi_search_games("counter", limit = 10, auth_token = token))
}, silent = TRUE)

# 3) Metadata for known IDs
try({
  invisible(vgi_game_metadata(steam_app_id = 730, auth_token = token))
}, silent = TRUE)

# Also capture metadata for IDs used in tests
for (id in c(570, 440, 10, 20, 30, 40, 50, 60, 70, 80, 90, 130)) {
  try({
    invisible(vgi_game_metadata(steam_app_id = id, auth_token = token))
  }, silent = TRUE)
}

# 4) Batch metadata with a mix of valid/invalid IDs
try({
  invisible(vgi_game_metadata_batch(c(730, 99999999), auth_token = token))
}, silent = TRUE)

# 5) Rankings
try({
  invisible(vgi_game_rankings(auth_token = token))
}, silent = TRUE)

# 6) Top games by revenue (limit small)
try({
  invisible(vgi_top_games("revenue", limit = 5, auth_token = token))
}, silent = TRUE)

message("Finished recording fixtures.")


