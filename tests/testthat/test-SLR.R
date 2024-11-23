# Load the function
source("~/SLR/R/SLR.R")

# Test the simple_lm function
test_that("simple_lm computes correct coefficients", {
  # Example data
  x <- c(1, 2, 3, 4, 5)
  y <- c(2.2, 2.8, 3.6, 4.5, 5.1)

  # Call the function
  model <- simple_lm(x, y)

  # Check the coefficients
  expect_equal(model$coefficients$intercept, 1.39, tolerance = 0.01)  # Intercept
  expect_equal(model$coefficients$slope, 0.75, tolerance = 0.01)      # Slope

  # Check the fitted values
  expected_fitted <- c(2.14, 2.89, 3.64, 4.39, 5.14)
  expect_equal(model$fitted_values, expected_fitted, tolerance = 0.01)

  # Check the residuals
  expected_residuals <- y - expected_fitted
  expect_equal(model$residuals, expected_residuals, tolerance = 0.01)
})

test_that("simple_lm handles input length mismatch", {
  # Test for length mismatch
  a <- c(1, 2, 3)
  b <- c(1, 2)

  # Expect an error
  expect_error(simple_lm(a, b), "The lengths of x and y must be the same.")
})

test_that("simple_lm handles edge cases", {
  # Test for single point case
  x <- c(1)
  y <- c(2)
  model <- simple_lm(x, y)

  # For a single point, the result should be the point itself
  expect_equal(model$coefficients$intercept, 2, tolerance = 0.01)
  expect_equal(model$coefficients$slope, 0, tolerance = 0.01)
  expect_equal(model$fitted_values, c(2), tolerance = 0.01)
  expect_equal(model$residuals, c(0), tolerance = 0.01)
})
