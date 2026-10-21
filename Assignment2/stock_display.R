# stock_display.R
# Assignment 2 - utilities to display stock data, statistics and charts.
# Requires stock_functions.R to be sourced first.

#' Print a Yahoo-Finance-style quote summary for one stock.
display_quote_summary <- function(df, symbol) {
  n <- nrow(df)
  last <- df[n, ]
  prev_close <- df$Close[n - 1]
  change <- last$Close - prev_close
  pct <- 100 * change / prev_close
  last_year <- df[df$Date > (last$Date - 365), ]

  cat(sprintf("\n%s  |  %s\n", symbol, format(last$Date)))
  cat(sprintf("Close: %.2f   Change: %+.2f (%+.2f%%)\n", last$Close, change, pct))
  cat(sprintf("Open: %.2f   Day range: %.2f - %.2f   Volume: %s\n",
              last$Open, last$Low, last$High, format(last$Volume, big.mark = ",")))
  cat(sprintf("Previous close: %.2f   52-week range: %.2f - %.2f\n",
              prev_close, min(last_year$Low), max(last_year$High)))
  invisible(NULL)
}

#' Show the first and last rows, structure and summary of a stock data frame.
display_stock_data <- function(df, symbol, n = 5) {
  cat("\n=====", symbol, "=====\n")
  cat("Rows:", nrow(df), " | From", format(min(df$Date)), "to", format(max(df$Date)), "\n")
  cat("\nFirst rows:\n");  print(utils::head(df, n), row.names = FALSE)
  cat("\nLast rows:\n");   print(utils::tail(df, n), row.names = FALSE)
  cat("\nColumn summary:\n"); print(summary(df[, -1]))
  invisible(NULL)
}

#' Display the loaded data for every stock.
display_all_stocks <- function(stocks, n = 5) {
  for (s in names(stocks)) {
    display_quote_summary(stocks[[s]], s)
    display_stock_data(stocks[[s]], s, n)
  }
  invisible(NULL)
}

#' Close price with its moving average.
plot_price_with_ma <- function(ma_df, symbol, ma_window = 20) {
  ggplot2::ggplot(ma_df, ggplot2::aes(x = Date)) +
    ggplot2::geom_line(ggplot2::aes(y = Close, colour = "Close")) +
    ggplot2::geom_line(ggplot2::aes(y = SMA, colour = paste0(ma_window, "-day SMA")),
                       na.rm = TRUE) +
    ggplot2::scale_colour_manual(values = c("grey40", "firebrick"), name = NULL) +
    ggplot2::labs(title = paste(symbol, "- Close price and moving average"),
                  x = NULL, y = "Price (USD)") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")
}

#' Daily trading volume.
plot_volume <- function(df, symbol) {
  ggplot2::ggplot(df, ggplot2::aes(x = Date, y = Volume)) +
    ggplot2::geom_col(fill = "steelblue") +
    ggplot2::labs(title = paste(symbol, "- Daily volume"), x = NULL, y = "Shares") +
    ggplot2::theme_minimal()
}

#' Compare all stocks, each rebased to 100 on its first day.
plot_normalised_comparison <- function(stocks) {
  long <- do.call(rbind, lapply(names(stocks), function(s) {
    d <- stocks[[s]]
    data.frame(Symbol = s, Date = d$Date, Index = 100 * d$Close / d$Close[1])
  }))
  ggplot2::ggplot(long, ggplot2::aes(Date, Index, colour = Symbol)) +
    ggplot2::geom_line() +
    ggplot2::labs(title = "Portfolio comparison (first day = 100)",
                  x = NULL, y = "Indexed close") +
    ggplot2::theme_minimal()
}

#' Bar chart of mean, median and mode of Close by stock.
plot_statistics_comparison <- function(stats_table) {
  long <- do.call(rbind, lapply(c("Mean", "Median", "Mode"), function(m) {
    data.frame(Symbol = stats_table$Symbol, Statistic = m, Value = stats_table[[m]])
  }))
  ggplot2::ggplot(long, ggplot2::aes(Symbol, Value, fill = Statistic)) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::labs(title = "Mean, median and mode of close price", x = NULL, y = "USD") +
    ggplot2::theme_minimal()
}
