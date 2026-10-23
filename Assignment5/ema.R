# ema.R - Exponential Moving Average (base R only, no libraries)
# Author: Safiya Muhammad | BDA400 Assignment 5
#
# ema(data, period)
#   data   : numeric vector
#   period : EMA period; the smoothing multiplier is 2 / (period + 1)
# Returns a numeric vector with the same length as `data`. The first value equals
# the first data point; later values use
#   EMA[i] = (data[i] - EMA[i-1]) * multiplier + EMA[i-1]

ema <- function(data, period) {
  # Validate the inputs
  if (!is.numeric(data)) stop("`data` must be a numeric vector", call. = FALSE)
  if (!is.numeric(period) || length(period) != 1 || is.na(period) ||
      period < 1 || period != round(period)) {
    stop("`period` must be a single positive whole number", call. = FALSE)
  }

  # Calculate the multiplier for EMA
  multiplier <- 2 / (period + 1)

  # Initialize an empty array to store EMA values
  ema_values <- numeric(length(data))

  # Loop through the data array
  for (i in seq_along(data)) {
    if (i == 1) {
      # Calculate EMA for the first data point
      ema_values[i] <- data[i]
    } else {
      # Calculate EMA for subsequent data points
      ema_values[i] <- (data[i] - ema_values[i - 1]) * multiplier + ema_values[i - 1]
    }
  }

  return(ema_values)
}
