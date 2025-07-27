test_that("get_base_url returns correct URL", {
  expect_equal(get_base_url(), "https://vginsights.com/api/v3")
})

test_that("get_auth_token works correctly", {
  # Test with explicit token
  expect_equal(get_auth_token("test_token"), "test_token")
  
  # Test with environment variable
  withr::with_envvar(
    c("VGI_AUTH_TOKEN" = "env_token"),
    {
      expect_equal(get_auth_token(), "env_token")
    }
  )
  
  # Test error when no token
  withr::with_envvar(
    c("VGI_AUTH_TOKEN" = ""),
    {
      expect_error(get_auth_token(), "Authentication token is required")
    }
  )
})

test_that("validate_platform works correctly", {
  # Valid platforms
  expect_silent(validate_platform("steam"))
  expect_silent(validate_platform("playstation"))
  expect_silent(validate_platform("xbox"))
  expect_silent(validate_platform("nintendo"))
  expect_silent(validate_platform("all"))
  
  # Invalid platform
  expect_error(validate_platform("invalid"), "Invalid platform")
})

test_that("validate_date works correctly", {
  # Valid date string
  expect_equal(validate_date("2024-01-01"), "2024-01-01")
  
  # Valid Date object
  expect_equal(validate_date(as.Date("2024-01-01")), "2024-01-01")
  
  # NULL date
  expect_null(validate_date(NULL))
  
  # Invalid date
  expect_error(validate_date("not a date"), "must be a Date object")
})