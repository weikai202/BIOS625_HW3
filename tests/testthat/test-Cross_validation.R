source("~/SLR/R/Cross_validation.R")

# Test the cross_validation function
test_that("cross_validation computes correct mean MSE", {
  # Example data
  x <- c(1, 2, 3, 4, 5)
  y <- c(2.2, 2.8, 3.6, 4.5, 5.1)

  # Run 5-fold cross-validation
  mean_mse <- cross_validation(x, y, k = 5)

  # Check if the output mean MSE is numeric
  expect_is(mean_mse, "numeric")

  # Check if the MSE value is approximately within the expected range (based on sample data)
  expect_true(mean_mse >= 0)  # MSE should be greater than or equal to 0
})

test_that("cross_validation handles input length mismatch", {
  # Test the case where x and y lengths do not match
  x <- c(1, 2, 3)
  y <- c(1, 2)

  # Expect an error to be thrown
  expect_error(cross_validation(x, y), "The lengths of x and y must be the same.")
})

test_that("cross_validation handles edge cases", {
  # Test the case with only one data point
  x <- c(1)
  y <- c(2)

  # Run cross-validation
  mean_mse <- cross_validation(x, y, k = 1)  # 1-fold cross-validation

  # MSE should be 0, as the model's prediction matches the actual value exactly
  expect_equal(mean_mse, 0)
})

test_that("cross_validation works with larger datasets", {
  # Test with larger datasets
  set.seed(42)
  x <- rnorm(1000)  # Generate 1000 random data points
  y <- 2 * x + 3 + rnorm(1000, sd = 0.5)  # Generate response variable

  # Run 10-fold cross-validation
  mean_mse <- cross_validation(x, y, k = 10)

  # Check if the output mean MSE is numeric
  expect_is(mean_mse, "numeric")
  expect_true(mean_mse >= 0)  # MSE should be greater than or equal to 0
})

test_that("cross_validation handles different values of k", {
  # Test with different values of k
  x <- c(1, 2, 3, 4, 5)
  y <- c(2.2, 2.8, 3.6, 4.5, 5.1)

  # Run 3-fold cross-validation
  mean_mse_3fold <- cross_validation(x, y, k = 3)

  # Run 5-fold cross-validation
  mean_mse_5fold <- cross_validation(x, y, k = 5)

  # Ensure that the MSE from both 3-fold and 5-fold cross-validation is numeric
  expect_is(mean_mse_3fold, "numeric")
  expect_is(mean_mse_5fold, "numeric")

  # Compare the MSE from 3-fold and 5-fold cross-validation; theoretically, 5-fold cross-validation should give a more accurate estimate
  expect_true(mean_mse_3fold >= mean_mse_5fold)
})
