# 加载函数
source("~/SLR/R/SLR.R")  # 确保路径正确

# 测试 simple_lm 函数
test_that("simple_lm computes correct coefficients", {
  # 示例数据
  x <- c(1, 2, 3, 4, 5)
  y <- c(2.2, 2.8, 3.6, 4.5, 5.1)

  # 调用函数
  model <- simple_lm(x, y)

  # 检查系数
  expect_equal(model$coefficients$intercept, 1.39, tolerance = 0.01)  # 截距
  expect_equal(model$coefficients$slope, 0.75, tolerance = 0.01)      # 斜率

  # 检查拟合值
  expected_fitted <- c(2.14, 2.89, 3.64, 4.39, 5.14)
  expect_equal(model$fitted_values, expected_fitted, tolerance = 0.01)

  # 检查残差
  expected_residuals <- y - expected_fitted
  expect_equal(model$residuals, expected_residuals, tolerance = 0.01)
})

test_that("simple_lm handles input length mismatch", {
  # 测试长度不匹配的情况
  a <- c(1, 2, 3)
  b <- c(1, 2)

  # 期待错误
  expect_error(simple_lm(a, b), "The lengths of x and y must be the same.")
})

test_that("simple_lm handles edge cases", {
  # 测试单点情况
  x <- c(1)
  y <- c(2)
  model <- simple_lm(x, y)

  # 单点情况结果为点本身
  expect_equal(model$coefficients$intercept, 2, tolerance = 0.01)
  expect_equal(model$coefficients$slope, 0, tolerance = 0.01)
  expect_equal(model$fitted_values, c(2), tolerance = 0.01)
  expect_equal(model$residuals, c(0), tolerance = 0.01)
})
