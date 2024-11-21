#' Simple Linear Regression
#'
#' This function performs simple linear regression without using any external package.
#'
#' @param x A numeric vector of independent variable values.
#' @param y A numeric vector of dependent variable values.
#' @return A list containing coefficients (intercept and slope), fitted values, and residuals.
#' @examples
#' x <- c(1, 2, 3, 4, 5)
#' y <- c(2.2, 2.8, 3.6, 4.5, 5.1)
#' model <- simple_lm(x, y)
#' print(model)
#' @export
simple_lm <- function(x, y) {
  if (length(x) != length(y)) {
    stop("The lengths of x and y must be the same.")
  }

  n <- length(x)

  if (n == 1) {  # 针对单点情况处理
    intercept <- y[1]
    slope <- 0
    fitted_values <- y
    residuals <- rep(0, n)
  } else {
    x_mean <- mean(x)
    y_mean <- mean(y)
    slope <- sum((x - x_mean) * (y - y_mean)) / sum((x - x_mean)^2)
    intercept <- y_mean - slope * x_mean

    fitted_values <- intercept + slope * x
    residuals <- y - fitted_values
  }

  return(list(
    coefficients = list(intercept = intercept, slope = slope),
    fitted_values = fitted_values,
    residuals = residuals
  ))
}
