# Scalable Endpoint Implementation Strategy

## Overview
With 44 endpoints in the new Video Game Insights API, we need a systematic approach to implement and maintain them efficiently.

## Current Status
- **Implemented and Updated**: 9 endpoints
- **Remaining**: 35 endpoints

## Key Patterns Identified

### 1. Common Response Patterns
Most endpoints follow similar response patterns:
- **Single Entity**: `{steamAppId, data}`
- **List with History**: `{steamAppId, historyArray}`
- **Change + Total Pattern**: `{date, changeValue, totalValue}`

### 2. Endpoint Categories
- **Game-specific** (`/games/{steamAppId}/...`): 25 endpoints
- **Date-based** (`/category/{date}`): 8 endpoints  
- **Company-specific** (`/developers|publishers/{companyId}/...`): 6 endpoints
- **Rankings/Lists** (`/games/rankings`, `/games/game-list`): 5 endpoints

## Implementation Strategy

### 1. Create Generic Helper Functions

```r
# Generic function to handle game-specific endpoints
fetch_game_data <- function(steam_app_id, endpoint_suffix, auth_token) {
  validate_numeric(steam_app_id, "steam_app_id")
  
  result <- make_api_request(
    endpoint = paste0("games/", steam_app_id, "/", endpoint_suffix),
    auth_token = auth_token,
    method = "GET"
  )
  
  return(result)
}

# Generic function to process history arrays
process_history_array <- function(result, history_field, date_fields = "date") {
  if (!is.null(result[[history_field]]) && length(result[[history_field]]) > 0) {
    # Convert to data frame
    history_df <- do.call(rbind, lapply(result[[history_field]], as.data.frame))
    
    # Convert date fields
    for (field in date_fields) {
      if (field %in% names(history_df)) {
        history_df[[field]] <- as.Date(history_df[[field]])
      }
    }
    
    # Sort by first date field
    if (length(date_fields) > 0 && date_fields[1] %in% names(history_df)) {
      history_df <- history_df[order(history_df[[date_fields[1]]]), ]
    }
    
    result[[history_field]] <- history_df
  }
  
  return(result)
}
```

### 2. Batch Implementation Approach

Group similar endpoints and implement them together:

#### Group 1: Commercial Performance (6 endpoints)
- ✅ `/commercial-performance/revenue/games/{steamAppId}`
- ✅ `/commercial-performance/units-sold/games/{steamAppId}`
- ✅ `/commercial-performance/price-history/games/{steamAppId}`
- ❌ `/commercial-performance/revenue/{date}`
- ❌ `/commercial-performance/units-sold/{date}`
- ❌ `/commercial-performance/price-history/{date}`

#### Group 2: Engagement Metrics (8 endpoints)
- ✅ `/engagement/concurrent-players/games/{steamAppId}`
- ✅ `/engagement/active-players/games/{steamAppId}`
- ❌ `/engagement/concurrent-players/{date}`
- ❌ `/engagement/active-players/{date}`
- ❌ `/engagement/weekly-active-users/games/{steamAppId}`
- ❌ `/engagement/weekly-active-users/{date}`

#### Group 3: Player Insights (6 endpoints)
- ❌ `/player-insights/games/{steamAppId}/playtime`
- ❌ `/player-insights/games/{steamAppId}/regions`
- ❌ `/player-insights/games/{steamAppId}/player-overlap`
- ❌ `/player-insights/{date}/playtime`
- ❌ `/player-insights/{date}/regions`

#### Group 4: Reception & Interest (8 endpoints)
- ✅ `/reception/reviews/games/{steamAppId}`
- ❌ `/reception/reviews/{date}`
- ❌ `/interest-level/wishlists/games/{steamAppId}`
- ❌ `/interest-level/wishlists/{date}`
- ❌ `/interest-level/followers/games/{steamAppId}`
- ❌ `/interest-level/followers/{date}`

#### Group 5: Company Endpoints (6 endpoints)
- ❌ `/developers/{companyId}`
- ❌ `/developers/{companyId}/game-ids`
- ❌ `/publishers/{companyId}`
- ❌ `/publishers/{companyId}/game-ids`

#### Group 6: Discovery & Rankings (5 endpoints)
- ❌ `/games/rankings`
- ✅ `/games/game-list`
- ❌ `/games/{steamAppId}/metadata`
- ❌ `/games/{steamAppId}/historical-data`
- ❌ `/analytics/steam-market-data`

### 3. Template Function Structure

```r
#' Get [Feature] Data for a Game
#'
#' [Description of what the endpoint returns]
#'
#' @param steam_app_id Integer. The Steam App ID of the game.
#' @param auth_token Character string. Your VGI API authentication token.
#'   Defaults to the VGI_AUTH_TOKEN environment variable.
#'
#' @return A list containing:
#' \describe{
#'   \item{steamAppId}{Integer. The Steam App ID}
#'   \item{[dataField]}{Data frame with [describe columns]}
#' }
#'
#' @export
#' @examples
#' \dontrun{
#' # Basic usage example
#' result <- vgi_[feature]_[metric](steam_app_id = 730)
#' }
vgi_[feature]_[metric] <- function(steam_app_id, 
                                  auth_token = Sys.getenv("VGI_AUTH_TOKEN")) {
  
  # Use generic helper
  result <- fetch_game_data(
    steam_app_id = steam_app_id,
    endpoint_suffix = "[endpoint-path]",
    auth_token = auth_token
  )
  
  # Process history if applicable
  result <- process_history_array(
    result = result,
    history_field = "[historyFieldName]",
    date_fields = c("date")
  )
  
  return(result)
}
```

### 4. Testing Strategy

Create generic test templates:

```r
test_that("vgi_[function] validates inputs correctly", {
  # Test non-numeric steam_app_id
  expect_error(
    vgi_[function]("not_a_number"),
    "steam_app_id must be numeric"
  )
  
  # Test NULL steam_app_id
  expect_error(
    vgi_[function](NULL),
    "steam_app_id must be numeric"
  )
})
```

### 5. Documentation Automation

Create a script to generate consistent documentation:

```r
generate_function_docs <- function(endpoint_info) {
  # Generate roxygen2 documentation
  # Generate test cases
  # Update NAMESPACE
  # Update API_MIGRATION.md
}
```

## Implementation Priority

1. **High Priority** (Most commonly used):
   - Player regions
   - Playtime statistics
   - Wishlist data
   - Game rankings

2. **Medium Priority** (Useful for analysis):
   - Date-based endpoints for cross-game comparisons
   - Player overlap data
   - Weekly active users

3. **Low Priority** (Specialized use cases):
   - Historical data endpoints
   - Steam market analytics

## Maintenance Strategy

1. **Version Management**: Tag releases before major API updates
2. **Backward Compatibility**: Maintain old function signatures where possible
3. **Deprecation Warnings**: Add warnings for functions that will be removed
4. **Migration Guides**: Document changes for each endpoint

## Automation Opportunities

1. **Code Generation**: Script to generate boilerplate for new endpoints
2. **Test Generation**: Automated test creation based on endpoint patterns
3. **Documentation Updates**: Auto-update docs when endpoints change
4. **API Monitoring**: Script to detect API changes

## Next Steps

1. Implement generic helper functions
2. Create code generation scripts
3. Batch implement remaining endpoints by group
4. Set up automated testing for all endpoints
5. Create comprehensive examples for each endpoint group