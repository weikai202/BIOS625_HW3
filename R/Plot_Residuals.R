#' Plot Residuals of a Simple Linear Regression Model
#'
#' This function generates a residual plot for a simple linear regression model.
#' It calculates the residuals (the difference between actual and fitted values),
#' and plots these residuals against the independent variable (x). A red horizontal line
#' at y = 0 is added to indicate the baseline of residuals.
#'
#' @param x A numeric vector of independent variable values.
#' @param y A numeric vector of dependent variable values.
#' @param model A fitted simple linear regression model (created using the simple_lm function).
#'
#' @details
#' The function first uses the provided model to predict the fitted values for the
#' given independent variable values (x). It then calculates the residuals as the
#' difference between the actual values (y) and the fitted values. The residuals
#' are plotted as blue points, and a red horizontal line is drawn at zero to help
#' visualize the residual distribution.
#'
#' @examples
#' # Example data
#' x <- c(1, 2, 3, 4, 5)
#' y <- c(2.2, 2.8, 3.6, 4.5, 5.1)
#'
#' # Fit a simple linear regression model
#' model <- simple_lm(x, y)
#'
#' # Plot the residuals
#' plot_residuals(x, y, model)
#'
#' @seealso \code{\link{simple_lm}} for fitting a simple linear regression model.
#'
#' @export
plot_residuals <- function(x, y, model) {
  # Calculate fitted values and residuals
  fitted_values <- predict_simple_lm(model, x)
  residuals <- y - fitted_values

  # Plot residuals
  plot(x, residuals, col = "blue", pch = 19, main = "Residual Plot",
       xlab = "X", ylab = "Residuals")
  abline(h = 0, col = "red", lwd = 2)
}
