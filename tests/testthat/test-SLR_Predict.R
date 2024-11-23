source("~/SLR/R/SLR_Predict.R")

test_that("predict_simple_lm computes predictions correctly", {

  # Create a simple linear model object
  model <- list(
    coefficients = list(
      intercept = 2,  # Assume intercept is 2
      slope = 3       # Assume slope is 3
    )
  )

  # Test 1: Check the prediction when x = 0
  result_0 <- predict_simple_lm(model, 0)
  expect_equal(result_0, 2)  # 2 + 3 * 0 = 2

  # Test 2: Check the prediction when x = 1
  result_1 <- predict_simple_lm(model, 1)
  expect_equal(result_1, 5)  # 2 + 3 * 1 = 5

  # Test 3: Check the prediction when x = 10
  result_10 <- predict_simple_lm(model, 10)
  expect_equal(result_10, 32)  # 2 + 3 * 10 = 32

  # Test 4: Check the prediction when x = -1
  result_neg1 <- predict_simple_lm(model, -1)
  expect_equal(result_neg1, -1)  # 2 + 3 * (-1) = -1
})
