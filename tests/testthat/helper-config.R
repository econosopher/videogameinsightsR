# Test configuration helper

# Load parent env if present to help local runs
try(suppressWarnings(readRenviron("../.Renviron")), silent = TRUE)
if (requireNamespace("dotenv", quietly = TRUE) && file.exists("../.env")) {
  try(dotenv::load_dot_env("../.env"), silent = TRUE)
}

# In CI, enable a small GET cache window to stabilize flaky upstream behavior
if (nzchar(Sys.getenv("CI"))) {
  ttl <- as.numeric(Sys.getenv("VGI_REQUEST_CACHE_TTL_SECONDS", "3600"))
  options(vgi.request_cache_ttl = ttl)
}


