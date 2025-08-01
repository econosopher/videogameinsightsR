# API Specification vs R Package Implementation Mismatches

## Overview
This document details all discrepancies between the Video Game Insights API specification and the videogameinsightsR package implementation. These mismatches explain why the API returns unexpected results.

## Critical Mismatches

### 1. `/games/rankings` Endpoint

**API Specification Accepts:**
- `offset` (optional) - for pagination
- `limit` (optional) - max records to return

**R Package Sends:**
- ❌ `platform` - NOT accepted by API
- ❌ `genre` - NOT accepted by API  
- ❌ `start_date` - NOT accepted by API
- ❌ `end_date` - NOT accepted by API
- ✅ `limit` - accepted

**Impact:** The API ignores all filtering parameters and returns the same static list of old games (IDs 10-400) regardless of what filters are requested.

### 2. `/games/game-list` Endpoint

**API Specification Accepts:**
- No parameters documented in spec

**R Package Sends:**
- ❌ `platform` - NOT accepted
- ❌ `genre` - NOT accepted
- ❌ `developer` - NOT accepted
- ❌ `publisher` - NOT accepted
- ❌ `limit` - NOT accepted

**Impact:** Filtering doesn't work; API returns unfiltered results.

### 3. `/games/metadata` Endpoint (Batch)

**API Specification Accepts:**
- Game IDs in request body or path

**R Package Sends:**
- ❌ `offset` - NOT accepted
- ❌ `limit` - NOT accepted

**Impact:** Pagination parameters are ignored.

### 4. `/developers/developer-list` Endpoint

**API Specification Accepts:**
- No parameters documented

**R Package Sends:**
- ❌ `search` - NOT accepted
- ❌ `min_games` - NOT accepted
- ❌ `limit` - NOT accepted

**Impact:** Search functionality doesn't work as expected.

### 5. `/publishers/publisher-list` Endpoint

**API Specification Accepts:**
- No parameters documented

**R Package Sends:**
- ❌ `search` - NOT accepted
- ❌ `min_games` - NOT accepted
- ❌ `limit` - NOT accepted

**Impact:** Search functionality doesn't work as expected.

### 6. Top Games Analytics Endpoints

**Issue:** The endpoints `/analytics/top-games-{metric}` don't exist in the API specification at all.

**R Package Expects:**
- `/analytics/top-games-revenue`
- `/analytics/top-games-units`
- `/analytics/top-games-ccu`
- `/analytics/top-games-dau`
- `/analytics/top-games-followers`

**API Returns:** 404 Not Found for all these endpoints

## Data Quality Issues

### 1. Old Games Only
- API returns games with Steam IDs 10-400 (released 2004-2007)
- Missing all modern games (e.g., CS2, PUBG, Apex Legends)
- Same games returned regardless of date range

### 2. Date Filtering Ignored
- Requesting July 2025 data returns same results as 2024 data
- Date parameters are sent but not processed by API

### 3. Missing Game Names
- Rankings endpoint returns game IDs without names
- Requires separate metadata calls to get game names
- Metadata endpoint works for individual games but not reliably in batch

## Root Cause Analysis

The fundamental issue is that the R package was developed based on expected API behavior rather than the actual API specification. The API either:

1. **Has an outdated specification** - The documented spec doesn't match the actual implementation
2. **Is using cached/test data** - Returns static dataset instead of live data
3. **Has incomplete implementation** - Endpoints exist but don't support expected parameters

## Recommendations

### Immediate Fixes for R Package

1. **Update function signatures** to only accept parameters the API actually supports
2. **Add warnings** when API returns suspicious data (e.g., only old games)
3. **Remove non-existent endpoints** like top-games analytics
4. **Document known limitations** in function help and README

### For API Provider

1. **Update API specification** to match actual implementation
2. **Fix data source** to return current games, not just 2004-2007 titles
3. **Implement filtering** for genre, platform, date ranges
4. **Add the missing analytics endpoints** or remove from documentation

### For Testing

1. **Always validate against API spec** before implementing functions
2. **Test with real API calls** not assumptions
3. **Monitor data freshness** (newest game release dates)
4. **Compare with known good sources** (Excel exports, website data)

## Testing Framework Usage

Run the diagnostic tests regularly:

```r
# From the videogameinsightsR directory
source("R/run_api_diagnostics.R")
```

This will:
- Check all functions for parameter compliance
- Validate endpoint responses
- Generate detailed reports
- Identify new issues as they arise

## Summary

The videogameinsightsR package sends many parameters that the API doesn't accept, explaining why filtering doesn't work. Additionally, the API returns a static dataset of old games regardless of parameters, making it unusable for current gaming analytics. Both the R package and the API need updates to work correctly together.