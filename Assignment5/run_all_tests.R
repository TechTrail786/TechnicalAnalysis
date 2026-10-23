# run_all_tests.R - tests for all Assignment 5 indicator functions
# Author: Safiya Muhammad | BDA400 Assignment 5
#
# HOW TO RUN
#   1. Put this file and the nine indicator scripts in one folder.
#   2. In RStudio: Session > Set Working Directory > To Source File Location.
#   3. Click Source (or run source("run_all_tests.R")).
# Expected values were calculated independently in Python from the assignment
# pseudocode. Base R functions (sd, lm, filter) are used ONLY here, to cross-check
# the hand-written implementations; the indicator scripts use no library functions.

for (f in c("sma", "ema", "macd", "stdev", "linreg", "rsi", "stoch_rsi",
            "crossover", "crossunder")) {
  source(paste0(f, ".R"))
}

results <- data.frame(Test = character(), Passed = logical())
check <- function(name, ok) {
  ok <- isTRUE(ok)
  results <<- rbind(results, data.frame(Test = name, Passed = ok))
  cat(sprintf("%-60s %s\n", name, if (ok) "PASS" else "FAIL"))
}
same <- function(a, b) isTRUE(all.equal(a, b, tolerance = 1e-9))
fails <- function(expr) inherits(try(expr, silent = TRUE), "try-error")

data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)

cat("\n--- SMA ---\n")
print(sma(data, period = 3))
check("sma: template example", same(sma(data, 3), c(12.333333333333334, 15.666666666666666, 17.666666666666668, 20.0, 21.666666666666668, 23.666666666666668, 23.333333333333332)))
check("sma: matches stats::filter (cross-check)",
      same(sma(data, 3), as.numeric(stats::na.omit(stats::filter(data, rep(1/3, 3), sides = 1)))))
check("sma: period = 1 returns the data", same(sma(data, 1), data))
check("sma: period = length returns the mean", same(sma(data, 9), mean(data)))
check("sma: error when data shorter than period", fails(sma(c(1, 2), 3)))

cat("\n--- EMA ---\n")
print(ema(data, period = 3))
check("ema: template example", same(ema(data, 3), c(10.0, 11.0, 13.0, 16.5, 17.25, 19.625, 22.3125, 23.15625, 22.078125)))
check("ema: first value equals first data point", ema(data, 3)[1] == data[1])
check("ema: constant series stays constant", same(ema(rep(5, 6), 4), rep(5, 6)))
check("ema: same length as input", length(ema(data, 5)) == length(data))

cat("\n--- MACD ---\n")
m <- macd(c(100, 105, 110, 115, 120, 125, 130), short_period = 3, long_period = 5, signal_period = 2)
print(m)
check("macd: macd_line", same(m$macd_line, c(0.0, 0.8333333333333286, 1.8055555555555571, 2.662037037037038, 3.337191358024697, 3.83937757201646, 4.200210048010973)))
check("macd: signal_line", same(m$signal_line, c(0.0, 0.5555555555555524, 1.3888888888888888, 2.237654320987655, 2.9706790123456828, 3.549811385459534, 3.98341049382716)))
check("macd: histogram", same(m$histogram, c(0.0, 0.27777777777777624, 0.4166666666666683, 0.42438271604938294, 0.36651234567901403, 0.2895661865569257, 0.21679955418381303)))
check("macd: histogram = macd_line - signal_line", same(m$histogram, m$macd_line - m$signal_line))

cat("\n--- STDEV ---\n")
print(stdev(data))
check("stdev: template example", same(stdev(data), 4.94662873101157))
check("stdev: equals sd() * sqrt((n-1)/n) (cross-check)", same(stdev(data), sd(data) * sqrt(8 / 9)))
check("stdev: constant data gives 0", stdev(c(4, 4, 4)) == 0)

cat("\n--- LINREG ---\n")
l0 <- linreg(data, regressionLength = 5, regressionOffset = 0)
print(l0)
check("linreg: slope (offset 0)", same(l0$slope, 0.7428571428571429))
check("linreg: intercept (offset 0)", same(l0$intercept, 19.066666666666666))
check("linreg: predicted values (offset 0)", same(l0$predicted_values, c(19.80952380952381, 20.55238095238095, 21.295238095238094, 22.038095238095238, 22.78095238095238, 23.523809523809526)))
fit <- lm(y ~ x, data = data.frame(x = 1:6, y = data[4:9]))
check("linreg: matches lm() on the same window (cross-check)",
      same(c(l0$intercept, l0$slope), as.numeric(coef(fit))))
l1 <- linreg(data, 5, 1)
check("linreg: slope and intercept (offset 1)", same(c(l1$slope, l1$intercept), c(2.1, 17.0)))
check("linreg: perfect line has exact slope", same(linreg(c(2, 4, 6, 8, 10), 4, 0)$slope, 2))
check("linreg: error when length > data", fails(linreg(data, 20, 0)))
check("linreg: error when offset >= length", fails(linreg(data, 5, 5)))

cat("\n--- RSI ---\n")
rd <- c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62)
print(rsi(rd, period = 5))
check("rsi: template example", same(rsi(rd, 5), c(NA, NA, NA, NA, NA, 50.52631578947368, 68.92561983471074, 71.83520599250936, 78.21079319087286, 66.8596640606858)))
check("rsi: values are between 0 and 100", all(rsi(rd, 5)[!is.na(rsi(rd, 5))] >= 0 & rsi(rd, 5)[!is.na(rsi(rd, 5))] <= 100))
check("rsi: first `period` values are NA", all(is.na(rsi(rd, 5)[1:5])) && !is.na(rsi(rd, 5)[6]))
check("rsi: always rising gives 100", all(rsi(1:12, 5)[6:12] == 100))
check("rsi: always falling gives 0", all(rsi(12:1, 5)[6:12] == 0))
check("rsi: too little data gives NA with a warning",
      all(is.na(suppressWarnings(rsi(c(1, 2, 3), 5)))))

cat("\n--- STOCHASTIC RSI ---\n")
s <- stoch_rsi(rd, period = 5, k_period = 3, d_period = 3)
print(s)
check("stoch_rsi: k_line", same(s$k_line, c(NA, NA, NA, NA, NA, 0.47810419382362235, 0.8114375271569557, 0.7865625073149638)))
check("stoch_rsi: d_line", same(s$d_line, c(NA, NA, NA, NA, NA, 0.6920347427651806)))
check("stoch_rsi: non-NA %K values are within 0..1",
      all(s$k_line[!is.na(s$k_line)] >= 0 & s$k_line[!is.na(s$k_line)] <= 1))
s14 <- suppressWarnings(stoch_rsi(rd, 14, 3, 3))
check("stoch_rsi: template call (period 14, 10 points) returns NA, no crash",
      all(is.na(s14$k_line)) && all(is.na(s14$d_line)))

cat("\n--- CROSSOVER and CROSSUNDER ---\n")
arr1 <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
arr2 <- c(18, 20, 22, 18, 15, 12, 10, 11, 13)
print(crossover(arr1, arr2))
print(crossunder(arr1, arr2))
check("crossover: template example", identical(crossover(arr1, arr2), c("None", "None", "None", "Up", "None", "None", "None", "None", "None")))
check("crossunder: template example", identical(crossunder(arr1, arr2), c("None", "False", "False", "False", "False", "False", "False", "False", "False")))
a <- c(1, 3, 2, 1); b <- c(2, 2, 2, 2)
check("crossover: Up then Down detected", identical(crossover(a, b), c("None", "Up", "None", "Down")))
check("crossunder: detects the cross under",
      identical(crossunder(a, b), c("None", "False", "False", "True")))
check("crossover: NA never signals", identical(crossover(c(1, NA, 3), c(2, 2, 2)), c("None", "None", "None")))
check("crossover: error on different lengths", fails(crossover(1:3, 1:4)))
check("crossunder: error on different lengths", fails(crossunder(1:3, 1:4)))

cat("\n--- SUMMARY ---\n")
cat(sum(results$Passed), "of", nrow(results), "tests passed\n")
if (!all(results$Passed)) stop("Some tests failed", call. = FALSE)
cat("All tests passed.\n")
