#' Generate Random Data and Fit a Simple Linear Regression Model
#'
#' This function generates random data that follows a linear relationship with added noise.
#' It then fits a simple linear regression model to the generated data and plots the actual
#' data points along with the fitted regression line.
#'
#' @param n An integer representing the number of data points to generate. Default is 100.
#'
#' @details
#' The function generates random data for the independent variable \( x \) using a normal distribution.
#' The dependent variable \( y \) is created using the equation \( y = 2x + 3 + \epsilon \), where \( \epsilon \)
#' is random noise with standard deviation 0.5. After generating the data, the function fits a simple linear regression
#' model using the `simple_lm` function and then visualizes the results using the `plot_lm` function.
#'
#' @examples
#' # Generate random data and fit a linear model
#' generate_data_and_fit(n = 100)
#'
#' @seealso \code{\link{simple_lm}} for fitting a simple linear regression model.
#' @seealso \code{\link{plot_lm}} for plotting the data and fitted regression line.
#'
#' @export
generate_data_and_fit <- function(n = 100) {
  # Generate random data
  x <- rnorm(n)  # Generate n random data points for x from normal distribution
  y <- 2 * x + 3 + rnorm(n, sd = 0.5)  # y = 2 * x + 3 + noise

  # Fit the linear regression model
  model <- simple_lm(x, y)

  # Plot the results
  plot_lm(x, y, model)
}

