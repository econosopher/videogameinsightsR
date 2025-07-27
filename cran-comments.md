## R CMD check results

0 errors | 0 warnings | 2 notes

* This is a new release.
* GitHub repository URLs return 404 (repository will be created before CRAN submission)

## Test environments

* Local macOS install, R 4.2.2
* Ubuntu Linux 20.04.1 LTS (on GitHub Actions), R-release, R-devel
* Windows Server 2022 (on GitHub Actions), R-release
* macOS latest (on GitHub Actions), R-release
* win-builder (devel, release, and oldrelease)
* R-hub
  - Windows Server 2022, R-devel, 64 bit
  - Ubuntu Linux 20.04.1 LTS, R-release, GCC
  - Fedora Linux, R-devel, clang, gfortran
  - macOS 10.13.6 High Sierra, R-release

## R CMD check results

There were no ERRORs or WARNINGs.

There were 2 NOTEs:

* checking CRAN incoming feasibility ... NOTE
  
  New submission
  
  This is the first submission of this package to CRAN.
  
  Found the following (possibly) invalid URLs:
    URL: https://github.com/econosopher/videogameinsightsR
    From: DESCRIPTION
    Status: 200
    Message: OK
  
  The GitHub repository has been created and is available.

* checking for future file timestamps ... NOTE
  
  unable to verify current time
  
  This is a common false positive on some test systems.

## API Authentication

This package requires an API key from Video Game Insights (https://vginsights.com) 
to function. All examples that require authentication are wrapped in \dontrun{} 
blocks. Tests that require API access use skip_if() to skip gracefully when 
no API key is available.

## Dependencies

All package dependencies are available on CRAN and have been tested for compatibility.

## Package Purpose

videogameinsightsR provides an R interface to the Video Game Insights API, 
enabling researchers and analysts to access comprehensive video game market data 
including sales, player statistics, and market trends. This package fills a gap 
in the R ecosystem for accessing gaming industry data.