#' Perform k-fold Cross-Validation for Linear Regression
#'
#' This function performs k-fold cross-validation on a linear regression model. It splits the data into k subsets (folds), trains the model on the training set, and evaluates the model's performance on the test set using Mean Squared Error (MSE). The final result is the average MSE across all folds.
#'
#' @param x A numeric vector representing the predictor variable(s) (independent variable).
#' @param y A numeric vector representing the response variable (dependent variable).
#' @param k An integer specifying the number of folds for cross-validation. The default is 5.
#'
#' @return A numeric value representing the mean MSE (Mean Squared Error) from the k-fold cross-validation.
#'
#' @details The function splits the data into `k` folds and performs the following steps:
#' - For each fold, it trains a linear regression model on the training set (the other k-1 folds) and tests the model on the test set (the current fold).
#' - The MSE for each fold is calculated and stored.
#' - Finally, the function returns the average MSE across all folds.
#'
#' @examples
#' # Example usage with simulated data
#' x <- rnorm(100)  # Generate random predictor data
#' y <- 2 * x + 3 + rnorm(100, sd = 0.5)  # Generate response variable with noise
#' mean_mse <- cross_validation(x, y, k = 5)
#' print(mean_mse)
#'
#' @export
cross_validation <- function(x, y, k = 5) {
  n <- length(x)
  fold_size <- floor(n / k)
  mse_values <- numeric(k)

  # Check if the lengths of x and y are the same
  if (length(x) != length(y)) {
    stop("The lengths of x and y must be the same.")
  }

  # If the sample size is smaller than k, return NaN or handle it as 0 (special case handling)
  if (n < k) {
    warning("The number of samples is less than the number of folds. Returning NaN for MSE.")
    return(NaN)
  }

  # Perform cross-validation
  for (i in 1:k) {
    test_indices <- ((i - 1) * fold_size + 1):min(i * fold_size, n)
    train_indices <- setdiff(1:n, test_indices)

    # Fit the linear model
    model <- simple_lm(x[train_indices], y[train_indices])

    # Predict the test set
    predictions <- predict_simple_lm(model, x[test_indices])
    mse_values[i] <- mean((y[test_indices] - predictions)^2)
  }

  # Calculate and return the average MSE
  mean_mse <- mean(mse_values)
  return(mean_mse)
}
