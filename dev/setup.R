# Development setup script for videogameinsightsR

# Install required packages for development
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
if (!requireNamespace("usethis", quietly = TRUE)) {
  install.packages("usethis")
}
if (!requireNamespace("roxygen2", quietly = TRUE)) {
  install.packages("roxygen2")
}

# Load development packages
library(devtools)
library(usethis)
library(roxygen2)

# Document the package
devtools::document()

# Run tests
devtools::test()

# Check the package
devtools::check()

# Install the package locally
devtools::install()

cat("Setup complete! The package is ready for development.\n")
cat("Use devtools::load_all() to load the package during development.\n")