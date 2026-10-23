# stdev.R - Standard Deviation (base R only, no libraries)
# Author: Safiya Muhammad | BDA400 Assignment 5
#
# stdev(data)
# Returns the POPULATION standard deviation: the square root of the mean of the
# squared differences from the mean (divides by n, not n - 1). This matches the
# formula in the assignment. Base R's sd() divides by n - 1, so
# stdev(x) == sd(x) * sqrt((n - 1) / n).

stdev <- function(data) {
  # Validate the input
  if (!is.numeric(data)) stop("`data` must be a numeric vector", call. = FALSE)
  if (length(data) == 0) stop("`data` must contain at least one value", call. = FALSE)

  # Calculate the mean of the data
  mean_value <- sum(data) / length(data)

  # Calculate the differences between the data points and the mean
  diff_values <- data - mean_value

  # Calculate the squared differences
  squared_diff <- diff_values * diff_values

  # Calculate the variance (mean of squared differences)
  variance <- sum(squared_diff) / length(squared_diff)

  # Calculate the standard deviation (square root of the variance)
  standard_deviation <- sqrt(variance)

  return(standard_deviation)
}
