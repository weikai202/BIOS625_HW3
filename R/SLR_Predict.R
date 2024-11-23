#' Predict using Simple Linear Regression
#'
#' This function predicts new values using a fitted simple linear regression model.
#'
#' @param model A model object returned by simple_lm.
#' @param new_x A numeric vector of new independent variable values.
#' @return A numeric vector of predicted values.
#' @examples
#' x <- c(1, 2, 3, 4, 5)
#' y <- c(2.2, 2.8, 3.6, 4.5, 5.1)
#' model <- simple_lm(x, y)
#' predictions <- predict_simple_lm(model, c(6, 7))
#' print(predictions)
#' @export
predict_simple_lm <- function(model, new_x) {
  intercept <- model$coefficients$intercept
  slope <- model$coefficients$slope
  return(intercept + slope * new_x)
}
