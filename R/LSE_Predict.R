#' Predict using the Least Squares Model
#'
#' This function predicts new response values using a fitted least squares model.
#'
#' @param model A list returned by the `least_squares` function.
#' @param new_X A matrix or data frame of new predictor variables (independent variables). The first column should not include a column of ones.
#' @return A numeric vector of predicted values.
#' @examples
#' X <- matrix(c(1, 2, 3, 4, 5, 2, 3, 4, 5, 6), ncol = 2)
#' y <- c(1.2, 2.3, 2.8, 4.2, 5.0)
#' model <- least_squares(X, y)
#' new_X <- matrix(c(6, 7, 8, 9, 10, 11), ncol = 2)
#' predictions <- predict_least_squares(model, new_X)
#' print(predictions)
#' @export
predict_least_squares <- function(model, new_X) {
  # Add a column of ones to new_X for the intercept
  new_X <- cbind(1, new_X)

  # Extract coefficients from the model
  coefficients <- model$coefficients

  # Predict using the formula: y_hat = new_X * beta
  predictions <- new_X %*% coefficients

  return(as.vector(predictions))
}
