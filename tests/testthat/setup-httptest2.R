# httptest2 setup: redact secrets and normalize request headers

if (requireNamespace("httptest2", quietly = TRUE)) {
  # Redact the API key header in fixtures
  httptest2::redact_http_headers("api-key")
  # Normalize User-Agent to reduce fixture churn
  httptest2::redact_http_headers("User-Agent")
}


