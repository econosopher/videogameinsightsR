# Comprehensive API Diagnostics Script
# Run this to diagnose all API issues and generate reports

library(videogameinsightsR)
library(testthat)

cat("=== VIDEO GAME INSIGHTS API DIAGNOSTICS ===\n")
cat("Running comprehensive API tests...\n\n")

# 1. Run specification compliance tests
cat("1. Testing API Specification Compliance...\n")
cat("=========================================\n")
test_results_spec <- test_file("tests/test_api_spec_compliance.R")
cat("\n")

# 2. Run endpoint validation tests
cat("2. Testing Endpoint Response Validation...\n")
cat("==========================================\n")
test_results_validation <- test_file("tests/test_endpoint_validation.R")
cat("\n")

# 3. Generate summary report
cat("3. DIAGNOSTIC SUMMARY\n")
cat("====================\n")

# Load the detailed reports if they exist
if (file.exists("api_compliance_report.rds")) {
  compliance_report <- readRDS("api_compliance_report.rds")
  
  # Count issues
  non_compliant <- sum(sapply(compliance_report, function(x) 
    !is.null(x$non_spec_params) && length(x$non_spec_params) > 0))
  
  cat(sprintf("Specification Compliance: %d/%d functions have issues\n",
              non_compliant, length(compliance_report)))
  
  if (non_compliant > 0) {
    cat("\nFunctions with parameter mismatches:\n")
    for (func_name in names(compliance_report)) {
      if (length(compliance_report[[func_name]]$non_spec_params) > 0) {
        cat(sprintf("  - %s: sends %s (not in spec)\n",
                    func_name,
                    paste(compliance_report[[func_name]]$non_spec_params, collapse = ", ")))
      }
    }
  }
}

if (file.exists("endpoint_validation_report.rds")) {
  validation_report <- readRDS("endpoint_validation_report.rds")
  
  cat(sprintf("\nAPI Health Status: %s\n", validation_report$api_health))
  
  if (length(validation_report$issues) > 0) {
    cat("\nCritical Issues Found:\n")
    for (issue in validation_report$issues) {
      cat(sprintf("  - %s\n", issue))
    }
  }
}

# 4. Recommendations
cat("\n4. RECOMMENDATIONS\n")
cat("==================\n")

cat("Based on the diagnostic results:\n\n")

cat("For API Provider:\n")
cat("- Fix /games/rankings endpoint to return current games (not just IDs 10-400)\n")
cat("- Ensure date filtering actually filters results\n")
cat("- Update API specification to match actual accepted parameters\n")
cat("- Fix data ingestion to include games newer than 2007\n")

cat("\nFor R Package:\n")
cat("- Update functions to only send parameters accepted by the API spec\n")
cat("- Add warnings when API returns suspicious data (e.g., only old games)\n")
cat("- Implement better error handling for malformed responses\n")
cat("- Cache game metadata to work around missing names in responses\n")

cat("\nFor Testing:\n")
cat("- Run these diagnostics regularly to catch API changes\n")
cat("- Add automated alerts when API health degrades\n")
cat("- Monitor for data freshness (newest game dates)\n")
cat("- Compare API results with known good data sources\n")

cat("\n\nDiagnostics complete. Reports saved to:\n")
cat("- api_compliance_report.rds\n")
cat("- endpoint_validation_report.rds\n")