# Scalable API Specification Compliance Testing Framework
# Tests each R package function against the OpenAPI specification
# to ensure parameters match what the API actually accepts

library(testthat)
library(jsonlite)
library(httr2)
library(videogameinsightsR)

# Load API specification
spec_path <- system.file("extdata", "vgi-api-spec.json", package = "videogameinsightsR")
if (spec_path == "") {
  spec_path <- "../vgi-api-spec.json"
}
api_spec <- fromJSON(spec_path)

# Helper function to extract endpoint parameters from OpenAPI spec
get_endpoint_params <- function(path, method = "get") {
  endpoint <- api_spec$paths[[path]][[method]]
  if (is.null(endpoint)) return(NULL)
  
  params <- list()
  if (!is.null(endpoint$parameters)) {
    for (param in endpoint$parameters) {
      params[[param$name]] <- list(
        in_spec = param[["in"]],
        required = isTRUE(param$required),
        description = param$description,
        type = param$schema$type
      )
    }
  }
  return(params)
}

# Helper function to trace API calls and capture parameters
capture_api_call <- function(func, ...) {
  called_params <- NULL
  called_endpoint <- NULL
  
  # Temporarily override make_api_request to capture calls
  trace(videogameinsightsR:::make_api_request, 
        exit = function() {
          env <- parent.frame()
          if (exists("endpoint", env)) called_endpoint <<- get("endpoint", env)
          if (exists("query_params", env)) called_params <<- get("query_params", env)
        },
        print = FALSE)
  
  # Execute function
  tryCatch({
    invisible(func(...))
  }, error = function(e) {
    # Ignore errors - we're testing parameter passing, not functionality
  })
  
  untrace(videogameinsightsR:::make_api_request)
  
  return(list(endpoint = called_endpoint, params = names(called_params)))
}

# Test suite for API specification compliance
context("API Specification Compliance")

test_that("vgi_game_rankings sends only spec-compliant parameters", {
  spec_params <- get_endpoint_params("/v3/games/rankings")
  spec_param_names <- names(spec_params)
  
  # Test what parameters the function actually sends
  call_info <- capture_api_call(vgi_game_rankings, 
                                genre = "Shooter", 
                                platform = "steam",
                                start_date = "2025-07-20",
                                end_date = "2025-07-26",
                                limit = 10)
  
  sent_params <- call_info$params
  
  # Check for non-spec parameters
  non_spec_params <- setdiff(sent_params, spec_param_names)
  
  expect_equal(length(non_spec_params), 0,
               info = paste("Function sends non-spec parameters:", 
                           paste(non_spec_params, collapse = ", "),
                           "\nSpec only allows:", 
                           paste(spec_param_names, collapse = ", ")))
})

test_that("vgi_top_games sends only spec-compliant parameters", {
  # Check each metric endpoint
  metrics <- c("revenue", "units", "ccu", "dau", "followers")
  
  for (metric in metrics) {
    endpoint_path <- sprintf("/v3/analytics/top-games-%s", metric)
    spec_params <- get_endpoint_params(endpoint_path)
    
    if (is.null(spec_params)) {
      warning(paste("No spec found for endpoint:", endpoint_path))
      next
    }
    
    spec_param_names <- names(spec_params)
    
    call_info <- capture_api_call(vgi_top_games,
                                  metric = metric,
                                  platform = "steam",
                                  limit = 10)
    
    sent_params <- call_info$params
    non_spec_params <- setdiff(sent_params, spec_param_names)
    
    expect_equal(length(non_spec_params), 0,
                 info = paste("Metric", metric, "sends non-spec parameters:", 
                             paste(non_spec_params, collapse = ", ")))
  }
})

test_that("vgi_game_list sends only spec-compliant parameters", {
  spec_params <- get_endpoint_params("/v3/games/game-list")
  spec_param_names <- names(spec_params)
  
  call_info <- capture_api_call(vgi_game_list,
                                genre = "RPG",
                                developer = "Valve",
                                publisher = "EA",
                                platform = "steam",
                                limit = 50)
  
  sent_params <- call_info$params
  non_spec_params <- setdiff(sent_params, spec_param_names)
  
  expect_equal(length(non_spec_params), 0,
               info = paste("Function sends non-spec parameters:", 
                           paste(non_spec_params, collapse = ", ")))
})

# Generate compliance report
generate_compliance_report <- function() {
  report <- list()
  
  # Test all exported functions
  pkg_functions <- ls("package:videogameinsightsR")
  api_functions <- grep("^vgi_", pkg_functions, value = TRUE)
  
  for (func_name in api_functions) {
    func <- get(func_name)
    
    # Skip functions without API calls
    if (func_name %in% c("vgi_auth_check")) next
    
    # Capture a sample call
    tryCatch({
      if (func_name == "vgi_game_metadata") {
        call_info <- capture_api_call(func, game_id = 730)
      } else if (func_name == "vgi_game_rankings") {
        call_info <- capture_api_call(func, limit = 10)
      } else if (func_name == "vgi_developer_list") {
        call_info <- capture_api_call(func, search = "test")
      } else if (func_name == "vgi_publisher_list") {
        call_info <- capture_api_call(func, search = "test")
      } else if (func_name == "vgi_game_list") {
        call_info <- capture_api_call(func, genre = "Action")
      } else {
        call_info <- capture_api_call(func)
      }
      
      if (!is.null(call_info$endpoint)) {
        # Extract path from endpoint
        path <- gsub("https://vginsights.com/api", "", call_info$endpoint)
        path <- gsub("\\?.*", "", path)  # Remove query string
        
        spec_params <- get_endpoint_params(path)
        
        report[[func_name]] <- list(
          endpoint = call_info$endpoint,
          sent_params = call_info$params,
          spec_params = names(spec_params),
          non_spec_params = setdiff(call_info$params, names(spec_params))
        )
      }
    }, error = function(e) {
      report[[func_name]] <- list(error = e$message)
    })
  }
  
  return(report)
}

# Run compliance report
cat("\n=== API SPECIFICATION COMPLIANCE REPORT ===\n\n")
report <- generate_compliance_report()

for (func_name in names(report)) {
  cat(sprintf("Function: %s\n", func_name))
  
  if (!is.null(report[[func_name]]$error)) {
    cat(sprintf("  Error: %s\n", report[[func_name]]$error))
  } else {
    cat(sprintf("  Endpoint: %s\n", report[[func_name]]$endpoint))
    
    if (length(report[[func_name]]$non_spec_params) > 0) {
      cat(sprintf("  ⚠️  Non-spec parameters: %s\n", 
                  paste(report[[func_name]]$non_spec_params, collapse = ", ")))
      cat(sprintf("  Spec allows: %s\n", 
                  paste(report[[func_name]]$spec_params, collapse = ", ")))
    } else {
      cat("  ✓ All parameters match specification\n")
    }
  }
  cat("\n")
}

# Summary statistics
total_functions <- length(report)
compliant_functions <- sum(sapply(report, function(x) 
  is.null(x$error) && length(x$non_spec_params) == 0))

cat(sprintf("\nSUMMARY: %d/%d functions fully compliant with API specification\n",
            compliant_functions, total_functions))

# Export detailed report to a temp location to avoid polluting repo
report_path <- file.path(tempdir(), "api_compliance_report.rds")
saveRDS(report, report_path)
cat("\nDetailed report saved to:", report_path, "\n")