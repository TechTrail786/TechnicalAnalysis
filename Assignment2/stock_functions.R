# stock_functions.R
# Assignment 2 - Technical Analysis using R (Preliminary Stage)
# Utility functions to load stock data and compute basic statistics.
# Usage: source("stock_functions.R")

required_packages <- c("quantmod", "TTR", "ggplot2")

install_missing_packages <- function(pkgs = required_packages) {
  missing <- pkgs[!pkgs %in% rownames(installed.packages())]
  if (length(missing) > 0) install.packages(missing)
  invisible(lapply(pkgs, library, character.only = TRUE))
}

# ---- 1. Import --------------------------------------------------------------

#' Read stock symbols from a text file (one symbol per line).
read_portfolio <- function(portfolio_file = "portfolio.txt") {
  symbols <- trimws(readLines(portfolio_file, warn = FALSE))
  symbols <- symbols[symbols != "" & !startsWith(symbols, "#")]
  unique(toupper(symbols))
}

#' Read portfolio.txt and download daily data for each symbol with quantmod.
#' Returns a named list of data frames (one per symbol) and, by default, also
#' creates one data frame per symbol (e.g. AAPL, MSFT) in `env`.
#' Columns: Date, Open, High, Low, Close, Volume, Adjusted.
load_stock_data <- function(portfolio_file = "portfolio.txt",
                            from = "2024-01-01",
                            to = Sys.Date(),
                            env = globalenv(),
                            assign_to_env = TRUE) {
  symbols <- read_portfolio(portfolio_file)
  stocks <- list()

  for (sym in symbols) {
    xt <- tryCatch(
      quantmod::getSymbols(sym, src = "yahoo", from = from, to = to,
                           auto.assign = FALSE),
      error = function(e) {
        warning("Could not download ", sym, ": ", conditionMessage(e))
        NULL
      }
    )
    if (is.null(xt)) next

    df <- data.frame(Date = as.Date(zoo::index(xt)), zoo::coredata(xt))
    names(df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
    df <- df[stats::complete.cases(df), ]
    rownames(df) <- NULL

    stocks[[sym]] <- df
    if (assign_to_env) assign(sym, df, envir = env)
  }

  if (length(stocks) == 0) stop("No stock data could be loaded.")
  stocks
}

# ---- 2. Statistics ----------------------------------------------------------

#' Mode of a numeric vector (most frequent value, rounded to `digits`).
#' If every value is unique the first value is returned and the mode is
#' not meaningful for continuous prices; this is noted in the report.
get_mode <- function(x, digits = 2) {
  x <- round(stats::na.omit(x), digits)
  counts <- table(x)
  as.numeric(names(counts)[which.max(counts)])
}

#' Basic statistics for one stock's data frame (uses the Close price).
#' Returns a list with:
#'   $summary        one-row data frame: mean, median, mode, sd, latest SMA
#'   $moving_average data frame: Date, Close, SMA (window `ma_window`)
calculate_statistics <- function(df, ma_window = 20) {
  close <- df$Close
  sma <- TTR::SMA(close, n = ma_window)

  list(
    summary = data.frame(
      Observations = length(close),
      Mean         = mean(close),
      Median       = stats::median(close),
      Mode         = get_mode(close),
      StdDev       = stats::sd(close),
      Latest_SMA   = utils::tail(sma, 1),
      MA_Window    = ma_window
    ),
    moving_average = data.frame(Date = df$Date, Close = close, SMA = sma)
  )
}

#' Apply calculate_statistics() to every stock in the list.
#' Returns list(by_stock = named list, table = combined summary data frame).
calculate_all_statistics <- function(stocks, ma_window = 20) {
  by_stock <- lapply(stocks, calculate_statistics, ma_window = ma_window)
  table <- do.call(rbind, lapply(names(by_stock), function(s) {
    cbind(Symbol = s, by_stock[[s]]$summary)
  }))
  rownames(table) <- NULL
  list(by_stock = by_stock, table = table)
}
