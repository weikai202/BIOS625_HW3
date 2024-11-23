#' Plot Simple Linear Regression Results
#'
#' This function plots the actual values and the fitted regression line for a
#' simple linear regression model. The actual data points are shown as red
#' dots, and the fitted values (from the regression model) are shown as blue
#' dots and a blue line.
#'
#' @param x A numeric vector of independent variable values.
#' @param y A numeric vector of dependent variable values.
#' @param model A fitted simple linear regression model (created using the simple_lm function).
#'
#' @details
#' The function first plots the actual data points (x, y) as red dots. It then
#' computes the fitted values based on the provided regression model and plots
#' those as blue dots. Additionally, a blue line is drawn to show the linear
#' regression fit.
#'
#' @examples
#' # Example data
#' x <- c(1, 2, 3, 4, 5)
#' y <- c(2.2, 2.8, 3.6, 4.5, 5.1)
#'
#' # Fit a simple linear regression model
#' model <- simple_lm(x, y)
#'
#' # Plot the actual values and fitted regression line
#' plot_lm(x, y, model)
#'
#' @seealso \code{\link{simple_lm}} for fitting a simple linear regression model.
#'
#' @export
plot_lm <- function(x, y, model) {
  # Plot actual values
  plot(x, y, col = "red", pch = 19, main = "Simple Linear Regression",
       xlab = "X", ylab = "Y")

  # Calculate fitted values
  fitted_values <- predict_simple_lm(model, x)

  # Plot fitted values as blue dots
  points(x, fitted_values, col = "blue", pch = 19)

  # Draw regression line
  lines(x, fitted_values, col = "blue", lwd = 0.5)
}
