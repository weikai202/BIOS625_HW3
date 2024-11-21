#' Least Squares Estimation for Linear Regression
#'
#' This function implements ordinary least squares estimation to fit a linear regression model.
#'
#' @param X A matrix or data frame of predictor variables (independent variables). The first column should not include a column of ones; the function will handle the intercept term automatically.
#' @param y A numeric vector of response variable (dependent variable).
#' @return A list containing:
#'   - `coefficients`: Estimated coefficients (including the intercept).
#'   - `fitted_values`: Predicted values for the input data.
#'   - `residuals`: Residuals from the fitted model.
#'   - `r_squared`: R-squared value of the model.
#' @examples
#' X <- matrix(c(1, 2, 3, 4, 5, 2, 3, 4, 5, 6), ncol = 2)
#' y <- c(1.2, 2.3, 2.8, 4.2, 5.0)
#' model <- least_squares(X, y)
#' print(model)
#' @export
least_squares <- function(X, y) {
  # Check if X and y dimensions match
  if (nrow(X) != length(y)) {
    stop("Number of rows in X must match the length of y.")
  }

  # Add a column of ones to X for the intercept
  X <- cbind(1, X)

  # Compute the coefficients using the formula: beta = (X'X)^(-1)X'y
  XtX <- t(X) %*% X            # X'X
  XtX_inv <- solve(XtX)        # (X'X)^(-1)
  Xty <- t(X) %*% y            # X'y
  coefficients <- XtX_inv %*% Xty

  # Compute fitted values: y_hat = X * beta
  fitted_values <- X %*% coefficients

  # Compute residuals: e = y - y_hat
  residuals <- y - fitted_values

  # Compute R-squared
  ss_total <- sum((y - mean(y))^2)  # Total sum of squares
  ss_residual <- sum(residuals^2)  # Residual sum of squares
  r_squared <- 1 - (ss_residual / ss_total)

  # Return results as a list
  return(list(
    coefficients = as.vector(coefficients),  # Convert matrix to vector
    fitted_values = as.vector(fitted_values),
    residuals = as.vector(residuals),
    r_squared = r_squared
  ))
}

