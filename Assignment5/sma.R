# sma.R - Simple Moving Average (base R only, no libraries)
# Author: Safiya Muhammad | BDA400 Assignment 5
#
# sma(data, period)
#   data   : numeric vector
#   period : number of points in each window
# Returns a numeric vector of length(data) - period + 1 values: the mean of each
# consecutive window of `period` data points (no padding, as in the pseudocode).

sma <- function(data, period) {
  # Validate the inputs
  if (!is.numeric(data)) stop("`data` must be a numeric vector", call. = FALSE)
  if (!is.numeric(period) || length(period) != 1 || is.na(period) ||
      period < 1 || period != round(period)) {
    stop("`period` must be a single positive whole number", call. = FALSE)
  }

  # Check if the length of data is less than the specified period
  if (length(data) < period) {
    stop("Data length should be greater than or equal to the period", call. = FALSE)
  }

  # Initialize a vector to store the SMA values
  n_windows  <- length(data) - period + 1
  sma_values <- numeric(n_windows)

  # Calculate SMA for each window of 'period' data points
  for (i in 1:n_windows) {
    current_window <- data[i:(i + period - 1)]      # the current window
    mean_value     <- sum(current_window) / period  # mean of the window
    sma_values[i]  <- mean_value                    # store it
  }

  return(sma_values)
}
