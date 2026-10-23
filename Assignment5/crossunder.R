# crossunder.R - Crossunder signals (base R only, no libraries)
# Author: Safiya Muhammad | BDA400 Assignment 5
#
# crossunder(arr1, arr2)
# Returns a character vector the same length as the inputs, following the
# assignment pseudocode: the first element is "None"; every later element is
# "True" when arr1 crosses under arr2 (arr1[i] < arr2[i] and arr1[i-1] >= arr2[i-1])
# and "False" otherwise. NA values never produce a signal.

crossunder <- function(arr1, arr2) {
  # Check if the length of both arrays is the same
  if (length(arr1) != length(arr2)) {
    stop("Both arrays should have the same length", call. = FALSE)
  }

  # Initialize a vector to store the crossunder signals
  crossunder_signals <- rep("None", length(arr1))

  # Check for crossunder signals at each data point
  if (length(arr1) >= 2) {
    for (i in 2:length(arr1)) {
      if (isTRUE(arr1[i] < arr2[i] && arr1[i - 1] >= arr2[i - 1])) {
        crossunder_signals[i] <- "True"
      } else {
        crossunder_signals[i] <- "False"
      }
    }
  }

  return(crossunder_signals)
}
