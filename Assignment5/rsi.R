# rsi.R - Relative Strength Index (base R only, no libraries)
# Author: Safiya Muhammad | BDA400 Assignment 5
#
# rsi(data, period)
# Returns a numeric vector the same length as `data`. The first `period` values are
# NA (not enough history); the rest are 100 - 100 / (1 + RS), where RS is the
# ratio of Wilder-smoothed average gain to average loss. Implemented exactly as the
# assignment pseudocode: the smoothing loop starts at i = period + 1.
# Special cases: no losses gives 100; no gains and no losses gives 50.
# If there are fewer than period + 1 data points, a vector of NA is returned
# with a warning.

rsi <- function(data, period) {
  # Validate the inputs
  if (!is.numeric(data)) stop("`data` must be a numeric vector", call. = FALSE)
  if (!is.numeric(period) || length(period) != 1 || is.na(period) ||
      period < 1 || period != round(period)) {
    stop("`period` must be a single positive whole number", call. = FALSE)
  }

  n <- length(data)

  # Not enough data: period differences are needed
  if (n < period + 1) {
    warning("Need at least period + 1 data points; returning NA", call. = FALSE)
    return(rep(NA_real_, n))
  }

  # Calculate the differences between consecutive data points
  diff_values <- data[-1] - data[-n]

  # Initialize two vectors to store the gains and losses
  gains  <- numeric(length(diff_values))
  losses <- numeric(length(diff_values))

  # Calculate gains and losses
  for (i in seq_along(diff_values)) {
    if (diff_values[i] > 0) {
      gains[i] <- diff_values[i]
    } else {
      losses[i] <- abs(diff_values[i])
    }
  }

  # Calculate the average gains and average losses for the first 'period' points
  avg_gain <- sum(gains[1:period]) / period
  avg_loss <- sum(losses[1:period]) / period

  # Initialize the RSI vector with NA values
  rsi_values <- rep(NA_real_, n)

  # Calculate RSI values using Wilder's smoothing method
  for (i in (period + 1):n) {
    avg_gain <- (avg_gain * (period - 1) + gains[i - 1]) / period
    avg_loss <- (avg_loss * (period - 1) + losses[i - 1]) / period

    if (avg_loss == 0) {
      rsi_values[i] <- if (avg_gain == 0) 50 else 100   # avoid 0/0 and division by zero
    } else {
      rs <- avg_gain / avg_loss
      rsi_values[i] <- 100 - (100 / (1 + rs))
    }
  }

  return(rsi_values)
}
