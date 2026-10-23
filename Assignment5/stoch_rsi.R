# stoch_rsi.R - Stochastic RSI (base R only, no libraries)
# Author: Safiya Muhammad | BDA400 Assignment 5
#
# stoch_rsi(data, period, k_period, d_period)
# Follows the assignment pseudocode:
#   k_values = (RSI - min RSI) / (max RSI - min RSI)   (min and max over all RSI values)
#   k_line   = sma(k_values, k_period)
#   d_line   = sma(k_line, d_period)
# Returns list(k_line, d_line). Because sma() does not pad, k_line has
# length(data) - k_period + 1 values and d_line is shorter again; values that
# depend on the NA warm-up of the RSI are NA.
#
# Needs rsi() and sma(). If they have not been loaded, identical copies are
# defined below so this file also works on its own.

if (!exists("sma", mode = "function")) {
  sma <- function(data, period) {
    if (length(data) < period) {
      stop("Data length should be greater than or equal to the period", call. = FALSE)
    }
    n_windows  <- length(data) - period + 1
    sma_values <- numeric(n_windows)
    for (i in 1:n_windows) {
      sma_values[i] <- sum(data[i:(i + period - 1)]) / period
    }
    return(sma_values)
  }
}

if (!exists("rsi", mode = "function")) {
  rsi <- function(data, period) {
    n <- length(data)
    if (n < period + 1) {
      warning("Need at least period + 1 data points; returning NA", call. = FALSE)
      return(rep(NA_real_, n))
    }
    diff_values <- data[-1] - data[-n]
    gains  <- numeric(length(diff_values))
    losses <- numeric(length(diff_values))
    for (i in seq_along(diff_values)) {
      if (diff_values[i] > 0) gains[i] <- diff_values[i] else losses[i] <- abs(diff_values[i])
    }
    avg_gain <- sum(gains[1:period]) / period
    avg_loss <- sum(losses[1:period]) / period
    rsi_values <- rep(NA_real_, n)
    for (i in (period + 1):n) {
      avg_gain <- (avg_gain * (period - 1) + gains[i - 1]) / period
      avg_loss <- (avg_loss * (period - 1) + losses[i - 1]) / period
      if (avg_loss == 0) {
        rsi_values[i] <- if (avg_gain == 0) 50 else 100
      } else {
        rsi_values[i] <- 100 - (100 / (1 + avg_gain / avg_loss))
      }
    }
    return(rsi_values)
  }
}

stoch_rsi <- function(data, period, k_period, d_period) {
  # Validate the input
  if (!is.numeric(data)) stop("`data` must be a numeric vector", call. = FALSE)

  # Calculate the RSI
  rsi_values <- rsi(data, period)

  # Calculate the StochRSI: normalise RSI between 0 and 1
  valid   <- rsi_values[!is.na(rsi_values)]
  min_rsi <- if (length(valid) > 0) min(valid) else NA_real_
  max_rsi <- if (length(valid) > 0) max(valid) else NA_real_
  if (is.na(min_rsi) || max_rsi == min_rsi) {
    k_values <- rep(NA_real_, length(rsi_values))      # nothing to normalise
  } else {
    k_values <- (rsi_values - min_rsi) / (max_rsi - min_rsi)
  }

  # Calculate the %K line (smoothed StochRSI)
  k_line <- sma(k_values, k_period)

  # Calculate the %D line (simple moving average of %K)
  d_line <- sma(k_line, d_period)

  # Return the %K and %D lines as a list
  result <- list(k_line = k_line, d_line = d_line)
  return(result)
}
