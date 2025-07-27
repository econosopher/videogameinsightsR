# R-hub checks for CRAN submission
# This script runs comprehensive checks on multiple platforms

# Install rhub if needed
if (!requireNamespace("rhub", quietly = TRUE)) {
  install.packages("rhub")
}

library(rhub)

# Get the package path
pkg_path <- here::here()

# Validate email for R-hub (you'll need to validate your email first time)
# rhub::validate_email("your.email@example.com")

# Run checks on multiple platforms
# These are the recommended checks for CRAN submission

cat("Starting R-hub checks for CRAN submission...\n")
cat("This will check the package on multiple platforms.\n")
cat("Note: You need to have validated your email with rhub first.\n\n")

# 1. Windows Server 2022, R-devel, 64 bit
cat("1. Checking on Windows Server 2022 (R-devel)...\n")
rhub::check(platform = "windows-x86_64-devel")

# 2. Ubuntu Linux 20.04.1 LTS, R-release, GCC
cat("\n2. Checking on Ubuntu Linux (R-release)...\n")
rhub::check(platform = "ubuntu-gcc-release")

# 3. Fedora Linux, R-devel, clang, gfortran
cat("\n3. Checking on Fedora Linux (R-devel, clang)...\n")
rhub::check(platform = "fedora-clang-devel")

# 4. macOS 10.13.6 High Sierra, R-release, CRAN's setup
cat("\n4. Checking on macOS (R-release)...\n")
rhub::check(platform = "macos-highsierra-release-cran")

# Optional: Check with valgrind for memory issues
# cat("\n5. Checking with valgrind (memory debugging)...\n")
# rhub::check(platform = "linux-x86_64-rocker-gcc-san")

# Check for CRAN incoming feasibility
cat("\n\nChecking CRAN incoming feasibility...\n")
rhub::check_for_cran()

cat("\n\nAll R-hub checks submitted!\n")
cat("Check your email for the results.\n")
cat("You can also check the status with: rhub::list_package_checks()\n")