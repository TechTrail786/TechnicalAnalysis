# crossover.R - Crossover signals (base R only, no libraries)
# Author: Safiya Muhammad | BDA400 Assignment 5
#
# crossover(arr1, arr2)
# Returns a character vector the same length as the inputs, following the
# assignment pseudocode:
#   "Up"   : arr1 crosses above arr2 here (arr1[i] > arr2[i] and arr1[i-1] <= arr2[i-1])
#   "Down" : arr1 crosses below arr2 here (arr1[i] < arr2[i] and arr1[i-1] >= arr2[i-1])
#   "None" : no crossing (always the case for the first element)
# NA values never produce a signal.

crossover <- function(arr1, arr2) {
  # Check if the length of both arrays is the same
  if (length(arr1) != length(arr2)) {
    stop("Both arrays should have the same length", call. = FALSE)
  }

  # Initialize a vector to store the crossover signals
  crossover_signals <- rep("None", length(arr1))

  # Check for crossovers at each data point
  if (length(arr1) >= 2) {
    for (i in 2:length(arr1)) {
      if (isTRUE(arr1[i] > arr2[i] && arr1[i - 1] <= arr2[i - 1])) {
        crossover_signals[i] <- "Up"
      } else if (isTRUE(arr1[i] < arr2[i] && arr1[i - 1] >= arr2[i - 1])) {
        crossover_signals[i] <- "Down"
      } else {
        crossover_signals[i] <- "None"
      }
    }
  }

  return(crossover_signals)
}
