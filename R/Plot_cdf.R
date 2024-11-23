#' Plot Cumulative Distribution Function (CDF)
#'
#' This function plots the cumulative distribution function (CDF) of a given numeric vector.
#' It sorts the input data and calculates the cumulative probability, then plots the CDF.
#'
#' @param x A numeric vector of data for which the cumulative distribution function will be calculated.
#'
#' @details
#' The function first sorts the data in ascending order and calculates the cumulative probability
#' for each data point by dividing its rank by the total number of data points. The resulting CDF
#' is then plotted as a line graph with the x-axis representing the sorted data points and the y-axis
#' representing the cumulative probability.
#'
#' @examples
#' # Example data
#' x <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
#'
#' # Plot the CDF for the data
#' plot_cdf(x)
#'
#' @seealso \code{\link{hist}} for plotting a histogram.
#'
#' @export
plot_cdf <- function(x) {
  # Sort the data in ascending order
  sorted_x <- sort(x)

  # Calculate the cumulative distribution
  cdf <- (1:length(sorted_x)) / length(sorted_x)

  # Plot the CDF
  plot(sorted_x, cdf, type = "l", main = "Cumulative Distribution Function",
       xlab = "X", ylab = "CDF")
}
