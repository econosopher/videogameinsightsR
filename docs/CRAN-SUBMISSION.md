# CRAN Submission Checklist for videogameinsightsR

This checklist ensures the package meets all CRAN requirements before submission.

## Pre-submission Checklist

### Package Structure
- [ ] DESCRIPTION file is complete and properly formatted
  - [ ] Title is in title case and < 65 characters
  - [ ] Description is at least one complete sentence
  - [ ] Authors@R field is used
  - [ ] License is CRAN-compatible (MIT + file LICENSE)
  - [ ] URL and BugReports fields are included
  - [ ] All dependencies have version requirements

### Documentation
- [ ] All exported functions have complete documentation
- [ ] All parameters are documented with @param
- [ ] Return values are documented with @return
- [ ] Examples are provided and wrapped in \dontrun{} if they require API access
- [ ] Package-level documentation exists (?videogameinsightsR)
- [ ] NEWS.md file is up to date
- [ ] README.md does not contain any badges that won't work on CRAN

### Code Quality
- [ ] No use of .Internal() or .Call() to base R
- [ ] No modification of user's options(), par(), or working directory
- [ ] All external resources use \dontrun{} in examples
- [ ] Proper error messages (no cat() to stderr)
- [ ] No browser() or debug() calls
- [ ] Spell check passed: `spelling::spell_check_package()`

### Testing
- [ ] Test coverage > 80%
- [ ] All tests pass locally
- [ ] Tests work without internet connection (skip_if_offline())
- [ ] Tests don't write to user's home directory
- [ ] No test files > 5MB

### R CMD check
- [ ] `R CMD check --as-cran` passes with 0 errors, 0 warnings, 0 notes
- [ ] Checked on R-devel
- [ ] Checked on at least 2 different OS (via GitHub Actions)
- [ ] No NOTEs about package size (< 5MB)
- [ ] No NOTEs about non-standard files

### Platform Checks

#### Local checks
```r
# Run these locally
devtools::check()
devtools::check(args = "--as-cran")
rcmdcheck::rcmdcheck(args = "--as-cran")
```

#### R-hub checks
```r
# Run comprehensive R-hub checks
rhub::check_for_cran()
rhub::check(platform = "windows-x86_64-devel")
rhub::check(platform = "ubuntu-gcc-release")
rhub::check(platform = "fedora-clang-devel")
rhub::check(platform = "macos-highsierra-release-cran")
```

#### win-builder checks
```r
devtools::check_win_devel()
devtools::check_win_release()
devtools::check_win_oldrelease()
```

### API Key Handling
- [ ] Examples use \dontrun{} for code requiring authentication
- [ ] Tests skip gracefully when API key is not available
- [ ] No API keys are included in the package
- [ ] Clear documentation on how to obtain API keys

### Final Checks
- [ ] Version number is appropriate (0.1.0 for first submission)
- [ ] cran-comments.md is prepared
- [ ] Reverse dependency checks (if applicable)
- [ ] Package builds on all target platforms
- [ ] Vignettes build without errors

## Submission Process

1. Update cran-comments.md with:
   - Test environments used
   - R CMD check results
   - Downstream dependencies (if any)

2. Run final checks:
   ```r
   devtools::release()
   ```

3. Submit to CRAN:
   ```r
   devtools::submit_cran()
   ```

## Post-submission

- [ ] Monitor email for CRAN feedback
- [ ] Be prepared to respond within 2 weeks
- [ ] Update GitHub repository with CRAN release tag
- [ ] Announce release

## Common CRAN Feedback

1. **Examples take too long**: Wrap in \donttest{} or \dontrun{}
2. **Writing to user directory**: Use tempdir() instead
3. **Internet resource errors**: Use skip_if_offline() in tests
4. **Package size**: Compress data, reduce dependencies
5. **Non-portable code**: Test on multiple platforms

## Resources

- [CRAN Repository Policy](https://cran.r-project.org/web/packages/policies.html)
- [Writing R Extensions](https://cran.r-project.org/doc/manuals/r-release/R-exts.html)
- [R Packages book](https://r-pkgs.org/)
- [rOpenSci Packages Guide](https://devguide.ropensci.org/)