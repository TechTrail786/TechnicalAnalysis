# macd.R - Moving Average Convergence Divergence (base R only, no libraries)
# Author: Safiya Muhammad | BDA400 Assignment 5
#
# macd(data, short_period, long_period, signal_period)
# Returns a list with three vectors, each the same length as `data`:
#   macd_line   : short EMA - long EMA
#   signal_line : EMA of the MACD line
#   histogram   : macd_line - signal_line
#
# macd() needs ema(). If ema() has not been loaded (for example ema.R was not
# sourced first), a copy identical to ema.R is defined here so this file also
# works on its own.

if (!exists("ema", mode = "function")) {
  ema <- function(data, period) {
    multiplier <- 2 / (period + 1)
    ema_values <- numeric(length(data))
    for (i in seq_along(data)) {
      if (i == 1) {
        ema_values[i] <- data[i]
      } else {
        ema_values[i] <- (data[i] - ema_values[i - 1]) * multiplier + ema_values[i - 1]
      }
    }
    return(ema_values)
  }
}

macd <- function(data, short_period, long_period, signal_period) {
  # Validate the inputs
  if (!is.numeric(data)) stop("`data` must be a numeric vector", call. = FALSE)
  periods <- c(short_period, long_period, signal_period)
  if (!is.numeric(periods) || any(is.na(periods)) || any(periods < 1)) {
    stop("All periods must be positive numbers", call. = FALSE)
  }

  # Calculate the short-term and long-term exponential moving averages (EMA)
  short_ema <- ema(data, short_period)
  long_ema  <- ema(data, long_period)

  # Calculate the MACD line
  macd_line <- short_ema - long_ema

  # Calculate the signal line (EMA of the MACD line)
  signal_line <- ema(macd_line, signal_period)

  # Calculate the histogram (the difference between the MACD line and the signal line)
  histogram <- macd_line - signal_line

  # Return the MACD line, signal line, and histogram as a list
  result <- list(macd_line = macd_line, signal_line = signal_line, histogram = histogram)
  return(result)
}
