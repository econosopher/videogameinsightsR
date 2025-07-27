# videogameinsightsR Examples

This directory contains example scripts and visualizations created with the videogameinsightsR package.

## Requirements

You must have a valid Video Game Insights API token to run these examples. The package will not function without proper authentication.

## Setting up your API Token

Before running any examples, set your API token:

```r
# Option 1: Set for current session
Sys.setenv(VGI_AUTH_TOKEN = "your_token_here")

# Option 2: Add to .Renviron file (permanent)
usethis::edit_r_environ()
# Add: VGI_AUTH_TOKEN="your_token_here"
```

## Example Scripts

### 1. `example_usage.R`
Comprehensive demonstration of all major package functions including:
- Basic game information retrieval
- Rankings and top games
- Time series data analysis
- Developer and publisher information
- Batch operations
- Advanced analytics
- Error handling examples

### 2. `generate_visualizations.R`
Creates a complete set of data visualizations:

```r
# From the examples directory
source("generate_visualizations.R")
```

## Generated Outputs

The script creates the following visualizations:

1. **Top Games by Revenue** - Horizontal bar chart showing highest-earning games
2. **Top Games by CCU** - Lollipop chart of games with most concurrent users
3. **Platform Comparison** - Revenue comparison across gaming platforms
4. **GT Tables** - Professional-looking data tables with GEC styling
5. **RPG Timeline** - Release timeline for RPG games
6. **Metrics Heatmap** - Performance comparison across different metrics

All outputs are saved to `examples/outputs/` with the `_api` suffix to indicate they were generated from API data.