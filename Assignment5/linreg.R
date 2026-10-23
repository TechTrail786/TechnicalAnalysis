# linreg.R - Simple Linear Regression (base R only, no libraries)
# Author: Safiya Muhammad | BDA400 Assignment 5
#
# linreg(regressionSource, regressionLength, regressionOffset)
#   regressionSource : numeric vector of data points
#   regressionLength : how many points back the regression looks
#   regressionOffset : points trimmed from the end (0 = regress up to the last point)
# Follows the assignment pseudocode exactly:
#   start_index = max(1, n - regressionLength + regressionOffset)
#   end_index   = min(n, n - regressionOffset)
# so with an offset of 0 the window runs from n - regressionLength to n.
# Returns list(slope, intercept, predicted_values); x is 1, 2, 3, ... within the window.

linreg <- function(regressionSource, regressionLength, regressionOffset) {
  # Validate the types
  if (!is.numeric(regressionSource)) {
    stop("`regressionSource` must be a numeric vector", call. = FALSE)
  }
  if (!is.numeric(regressionLength) || !is.numeric(regressionOffset) ||
      length(regressionLength) != 1 || length(regressionOffset) != 1 ||
      regressionOffset < 0) {
    stop("`regressionLength` and `regressionOffset` must be single numbers; offset >= 0",
         call. = FALSE)
  }

  # Calculate the total number of elements in the regressionSource
  n <- length(regressionSource)

  # Check if regressionLength is greater than the number of elements in regressionSource
  if (regressionLength > n) {
    stop("regressionLength cannot be greater than the number of elements in regressionSource",
         call. = FALSE)
  }

  # Check if regressionOffset is greater than or equal to regressionLength
  if (regressionOffset >= regressionLength) {
    stop("regressionOffset must be less than regressionLength", call. = FALSE)
  }

  # Calculate the starting and ending index for the regressionSource
  start_index <- max(1, n - regressionLength + regressionOffset)
  end_index   <- min(n, n - regressionOffset)

  # Extract the relevant portion of regressionSource
  source_subset <- regressionSource[start_index:end_index]
  if (length(source_subset) < 2) {
    stop("At least two data points are needed for a regression", call. = FALSE)
  }

  # Calculate the index values for the regression points
  index_values <- seq_along(source_subset)

  # Calculate the sum of index values and the sum of source_subset
  sum_index  <- sum(index_values)
  sum_source <- sum(source_subset)

  # Calculate the mean of index values and the mean of source_subset
  mean_index  <- sum_index / length(index_values)
  mean_source <- sum_source / length(source_subset)

  # Calculate the numerator and denominator for the linear regression formula
  numerator   <- sum((index_values - mean_index) * (source_subset - mean_source))
  denominator <- sum((index_values - mean_index)^2)

  # Calculate the slope and intercept of the linear regression line
  slope     <- numerator / denominator
  intercept <- mean_source - slope * mean_index

  # Calculate the predicted values for the regression points
  predicted_values <- slope * index_values + intercept

  # Return the slope, intercept, and predicted values as a list
  result <- list(slope = slope, intercept = intercept, predicted_values = predicted_values)
  return(result)
}
