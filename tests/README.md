# videogameinsightsR Tests

This directory contains the test suite for the videogameinsightsR package.

## Test Structure

- `testthat/` - Standard testthat unit tests
  - `test-api-responses.R` - Tests for API response handling
  - `test-data-processing.R` - Tests for data transformation functions
  - `test-game-comparison.R` - Tests for game comparison functionality
  - `test-utils.R` - Tests for utility functions
  - `test-vgi-functions.R` - Tests for main VGI functions

- `test_marvel_overwatch_comparison.R` - Standalone test script for Marvel Rivals vs Overwatch comparison

## Running Tests

### Unit Tests
```r
# Run all tests
devtools::test()

# Run specific test file
devtools::test(filter = "game-comparison")
```

### Standalone Test Script
```bash
# Run the Marvel Rivals vs Overwatch comparison test
Rscript tests/test_marvel_overwatch_comparison.R
```

## Test Coverage

The test suite covers:

1. **API Endpoint Testing**
   - Successful response handling
   - Error response handling
   - Empty result handling
   - Rate limiting behavior

2. **Data Processing**
   - Different input types
   - Field validation
   - Data consistency

3. **Game Comparison**
   - Game search functionality
   - DAU/MAU data retrieval
   - Units sold data retrieval
   - Concurrent player data retrieval
   - PPSU (Peak Players per Sold Unit) calculation
   - 180-day historical data retrieval

## Marvel Rivals vs Overwatch Test

The `test_marvel_overwatch_comparison.R` script specifically tests:

- **DAU (Daily Active Users)**: Player engagement metrics
- **PPSU (Peak Players per Sold Unit)**: Efficiency of player retention
- **Daily Units Sold**: Sales velocity tracking
- **180-day historical data**: Long-term trend analysis

The script includes:
- Smart caching to minimize API calls
- Data visualization generation
- CSV export of comparison data
- Summary statistics calculation

## Environment Variables

All tests require the `VGI_AUTH_TOKEN` environment variable to be set:

```r
Sys.setenv(VGI_AUTH_TOKEN = "your_api_token_here")
```

Tests will skip if the token is not available.