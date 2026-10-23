# =============================================================================
# app.R - Portfolio Dashboard (R Shiny)
# BDA400 - Assignment 6: Technical Analysis using R, Visualization Phase
# Author: Safiya Muhammad
#
# An interactive dashboard that fetches stock data, draws line / candlestick /
# area charts, overlays Moving Averages, RSI and MACD (the indicators written
# from scratch in Assignment 5), and generates Buy / Sell / Hold signals from a
# trading rule that is annotated on the chart.
#
# HOW TO RUN
#   1. Open this file in RStudio and click "Run App"  (or run  shiny::runApp("app.R")).
#   2. The first run installs any missing packages (shiny, ggplot2, quantmod).
#   3. Pick a data source. "Yahoo Finance" needs an internet connection;
#      "Demo data" works offline; "Upload CSV" accepts your own price file.
#
# SECTIONS (matching the assignment tasks)
#   Step 1  Data collection and setup
#   Step 2  Visualizing stock data (Shiny skeleton, widgets, charts)
#   Step 3  Overlay technical indicators (with on/off switches)
#   Step 4  Trading rules and annotations
# =============================================================================


# =============================================================================
# STEP 1: DATA COLLECTION AND SETUP
# =============================================================================

# ---- 1a. Install (if missing) and load the necessary packages ---------------
required_packages <- c("shiny", "ggplot2", "quantmod")
missing_packages  <- required_packages[!required_packages %in% rownames(installed.packages())]
if (length(missing_packages) > 0) install.packages(missing_packages)

library(shiny)
library(ggplot2)
library(quantmod)    # only used to download Yahoo Finance data (getSymbols)

# ---- 1b. Technical indicators from Assignment 5 (base R, no libraries) ------
# Copies of sma(), ema(), macd(), rsi() and crossover() from Assignment 5.

sma <- function(data, period) {
  if (length(data) < period) {
    stop("Data length should be greater than or equal to the period", call. = FALSE)
  }
  sma_values <- numeric(length(data) - period + 1)
  for (i in seq_along(sma_values)) {
    sma_values[i] <- sum(data[i:(i + period - 1)]) / period
  }
  sma_values
}

ema <- function(data, period) {
  multiplier <- 2 / (period + 1)
  ema_values <- numeric(length(data))
  for (i in seq_along(data)) {
    ema_values[i] <- if (i == 1) data[i] else
      (data[i] - ema_values[i - 1]) * multiplier + ema_values[i - 1]
  }
  ema_values
}

macd <- function(data, short_period, long_period, signal_period) {
  macd_line   <- ema(data, short_period) - ema(data, long_period)
  signal_line <- ema(macd_line, signal_period)
  list(macd_line = macd_line, signal_line = signal_line,
       histogram = macd_line - signal_line)
}

rsi <- function(data, period) {
  n <- length(data)
  if (n < period + 1) return(rep(NA_real_, n))
  diff_values <- data[-1] - data[-n]
  gains  <- ifelse(diff_values > 0, diff_values, 0)
  losses <- ifelse(diff_values < 0, -diff_values, 0)
  avg_gain <- sum(gains[1:period]) / period
  avg_loss <- sum(losses[1:period]) / period
  rsi_values <- rep(NA_real_, n)
  for (i in (period + 1):n) {
    avg_gain <- (avg_gain * (period - 1) + gains[i - 1]) / period
    avg_loss <- (avg_loss * (period - 1) + losses[i - 1]) / period
    rsi_values[i] <- if (avg_loss == 0) { if (avg_gain == 0) 50 else 100 }
                     else 100 - 100 / (1 + avg_gain / avg_loss)
  }
  rsi_values
}

crossover <- function(arr1, arr2) {
  signals <- rep("None", length(arr1))
  if (length(arr1) >= 2) {
    for (i in 2:length(arr1)) {
      if (isTRUE(arr1[i] > arr2[i] && arr1[i - 1] <= arr2[i - 1])) signals[i] <- "Up"
      else if (isTRUE(arr1[i] < arr2[i] && arr1[i - 1] >= arr2[i - 1])) signals[i] <- "Down"
    }
  }
  signals
}

# sma() returns fewer values than its input, so pad the front with NA to line up with dates
sma_aligned <- function(x, period) {
  if (length(x) < period) return(rep(NA_real_, length(x)))
  c(rep(NA_real_, period - 1), sma(x, period))
}

# ---- 1c. Data sources --------------------------------------------------------
HISTORY_YEARS <- 8                                   # how much history is downloaded once

# Portfolio symbols: use portfolio.txt (from Assignment 2) if it sits next to this file
portfolio_symbols <- if (file.exists("portfolio.txt")) {
  s <- toupper(trimws(readLines("portfolio.txt", warn = FALSE)))
  s[s != ""]
} else c("AAPL", "MSFT", "GOOGL", "AMZN", "TSLA")
if (length(portfolio_symbols) == 0) portfolio_symbols <- c("AAPL", "MSFT", "GOOGL", "AMZN", "TSLA")

# Source 1: Yahoo Finance through quantmod, with error handling
fetch_yahoo <- function(symbol, from, to) {
  xt <- tryCatch(
    suppressWarnings(getSymbols(symbol, src = "yahoo", from = from, to = to,
                                auto.assign = FALSE)),
    error = function(e) {
      stop("Could not download '", symbol, "' from Yahoo Finance (", conditionMessage(e),
           "). Check the symbol and your internet connection, or choose 'Demo data'.",
           call. = FALSE)
    }
  )
  df <- data.frame(Date = as.Date(index(xt)),
                   Open = as.numeric(Op(xt)), High = as.numeric(Hi(xt)),
                   Low  = as.numeric(Lo(xt)), Close = as.numeric(Cl(xt)),
                   Volume = as.numeric(Vo(xt)))
  df[complete.cases(df), ]                           # drop rows with missing values
}

# Source 2: synthetic prices (offline demo, same shape as real data; fixed seed per symbol)
fetch_demo <- function(symbol, from, to) {
  days <- seq(as.Date(from), as.Date(to), by = "day")
  days <- days[!format(days, "%u") %in% c("6", "7")]         # weekdays only
  set.seed(sum(utf8ToInt(symbol)))                           # same symbol -> same series
  n      <- length(days)
  close  <- 100 + 150 * (sum(utf8ToInt(symbol)) %% 7) / 7
  close  <- close * exp(cumsum(rnorm(n, mean = 0.0004, sd = 0.017)))
  open   <- c(close[1], close[-n]) * exp(rnorm(n, 0, 0.004))
  high   <- pmax(open, close) * (1 + abs(rnorm(n, 0, 0.006)))
  low    <- pmin(open, close) * (1 - abs(rnorm(n, 0, 0.006)))
  data.frame(Date = days, Open = open, High = high, Low = low, Close = close,
             Volume = round(runif(n, 2e7, 8e7)))
}

# Source 3: a CSV file chosen by the user (needs Date, Open, High, Low, Close; Volume optional)
read_price_csv <- function(path) {
  df <- tryCatch(read.csv(path, stringsAsFactors = FALSE),
                 error = function(e) stop("Could not read the CSV file.", call. = FALSE))
  names(df) <- tools::toTitleCase(tolower(trimws(names(df))))
  needed <- c("Date", "Open", "High", "Low", "Close")
  if (!all(needed %in% names(df))) {
    stop("CSV must contain the columns: Date, Open, High, Low, Close (Volume optional).",
         call. = FALSE)
  }
  if (!"Volume" %in% names(df)) df$Volume <- NA_real_
  df$Date <- as.Date(df$Date)
  df <- df[, c("Date", needed[-1], "Volume")]
  df <- df[!is.na(df$Date) & complete.cases(df[, needed]), ]
  df[order(df$Date), ]
}

# Make sure a data frame is usable before it reaches the charts
check_prices <- function(df) {
  if (is.null(df) || nrow(df) < 5) stop("Not enough price data was returned.", call. = FALSE)
  if (any(df$High < df$Low, na.rm = TRUE)) stop("Data problem: some High values are below Low.", call. = FALSE)
  df
}

# Convert daily bars to weekly or monthly bars (Open = first, High = max, Low = min,
# Close = last, Volume = sum)
change_time_frame <- function(df, time_frame) {
  if (time_frame == "Daily") return(df)
  breaks <- if (time_frame == "Weekly") "week" else "month"
  groups <- split(df, cut(df$Date, breaks))
  groups <- groups[vapply(groups, nrow, integer(1)) > 0]
  out <- do.call(rbind, lapply(groups, function(g) {
    data.frame(Date = g$Date[nrow(g)], Open = g$Open[1], High = max(g$High),
               Low = min(g$Low), Close = g$Close[nrow(g)], Volume = sum(g$Volume))
  }))
  rownames(out) <- NULL
  out
}


# =============================================================================
# STEP 4 (helpers first): TRADING RULES
# Every rule returns one signal per bar: "Buy", "Sell" or "Hold".
# =============================================================================
generate_signals <- function(df, rule, p) {
  n <- nrow(df)
  signal <- rep("Hold", n)
  if (rule == "MA Crossover") {
    # Buy when the short MA crosses above the long MA, Sell when it crosses below
    x <- crossover(sma_aligned(df$Close, p$ma_short), sma_aligned(df$Close, p$ma_long))
  } else if (rule == "RSI Overbought/Oversold") {
    # Buy when RSI climbs back above the oversold level, Sell when it falls below overbought
    r  <- rsi(df$Close, p$rsi_period)
    up <- crossover(r, rep(p$rsi_oversold, n)) == "Up"
    dn <- crossover(r, rep(p$rsi_overbought, n)) == "Down"
    x  <- ifelse(up, "Up", ifelse(dn, "Down", "None"))
  } else {
    # MACD Crossover: Buy when the MACD line crosses above the signal line, Sell when below
    m <- macd(df$Close, p$macd_fast, p$macd_slow, p$macd_signal)
    x <- crossover(m$macd_line, m$signal_line)
    x[seq_len(min(p$macd_slow, n))] <- "None"          # ignore the EMA warm-up period
  }
  signal[x == "Up"]   <- "Buy"
  signal[x == "Down"] <- "Sell"
  signal
}


# =============================================================================
# STEP 2: VISUALIZING STOCK DATA - USER INTERFACE
# =============================================================================
ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { background: #f7f8fa; }
    .well { background: #ffffff; border: 1px solid #e3e6ea; }
    h4.panel-title { margin-top: 0; font-weight: 600; }
    .kpi { display: inline-block; min-width: 130px; margin: 0 10px 10px 0; padding: 8px 14px;
           background: #fff; border: 1px solid #e3e6ea; border-radius: 6px; }
    .kpi .label { font-size: 11px; color: #6b7280; text-transform: uppercase; }
    .kpi .value { font-size: 20px; font-weight: 600; }
    .up { color: #2e9e5b; } .down { color: #d64545; }
  "))),
  titlePanel("Portfolio Dashboard: Technical Analysis"),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Data", class = "panel-title"),
      selectInput("source", "Data source:",
                  choices = c("Yahoo Finance", "Demo data (offline)", "Upload CSV")),
      conditionalPanel("input.source == 'Upload CSV'",
        fileInput("csv_file", "CSV file (Date, Open, High, Low, Close, Volume):", accept = ".csv")),
      selectizeInput("symbol", "Stock symbol (type to add another):",
                     choices = portfolio_symbols, selected = portfolio_symbols[1],
                     options = list(create = TRUE)),
      dateRangeInput("date_range", "Select Date Range:",
                     start = Sys.Date() - 365, end = Sys.Date()),
      selectInput("time_frame", "Select Time Frame:", choices = c("Daily", "Weekly", "Monthly")),
      selectInput("chart_type", "Chart type:", choices = c("Candlestick", "Line", "Area")),

      hr(),
      h4("Technical indicators", class = "panel-title"),
      checkboxGroupInput("indicators", "Show on chart:",
                         choices = c("Moving Averages", "RSI", "MACD"),
                         selected = "Moving Averages"),
      conditionalPanel("input.indicators.indexOf('Moving Averages') > -1 || input.rule == 'MA Crossover'",
        fluidRow(
          column(6, numericInput("ma_short", "Short MA:", 20, min = 2, max = 200)),
          column(6, numericInput("ma_long",  "Long MA:",  50, min = 3, max = 400)))),
      conditionalPanel("input.indicators.indexOf('RSI') > -1 || input.rule == 'RSI Overbought/Oversold'",
        numericInput("rsi_period", "RSI period:", 14, min = 2, max = 100),
        fluidRow(
          column(6, numericInput("rsi_oversold",   "Oversold:",   30, min = 1, max = 49)),
          column(6, numericInput("rsi_overbought", "Overbought:", 70, min = 51, max = 99)))),
      conditionalPanel("input.indicators.indexOf('MACD') > -1 || input.rule == 'MACD Crossover'",
        fluidRow(
          column(4, numericInput("macd_fast",   "Fast:",   12, min = 2, max = 100)),
          column(4, numericInput("macd_slow",   "Slow:",   26, min = 3, max = 200)),
          column(4, numericInput("macd_signal", "Signal:",  9, min = 2, max = 100)))),

      hr(),
      h4("Trading rule", class = "panel-title"),
      selectInput("rule", "Rule:", choices = c("MA Crossover", "RSI Overbought/Oversold", "MACD Crossover")),
      checkboxInput("show_signals", "Annotate Buy / Sell signals on chart", TRUE),
      checkboxInput("show_hold", "Also label Hold bars", FALSE)
    ),

    mainPanel(
      width = 9,
      uiOutput("kpis"),
      tabsetPanel(
        tabPanel("Chart", plotOutput("stock_chart", height = "auto")),
        tabPanel("Signals", br(), uiOutput("signal_summary"), br(), tableOutput("signal_table")),
        tabPanel("Data", br(), tableOutput("data_table")),
        tabPanel("About", br(),
          p("Data: Yahoo Finance via quantmod, synthetic demo data, or your own CSV."),
          p("Indicators (SMA, EMA, MACD, RSI, crossover) are the base-R functions written in Assignment 5."),
          p("Trading rules: Buy when the rule's crossover turns up, Sell when it turns down, otherwise Hold."),
          p(strong("For learning only. This is not investment advice.")))
      )
    )
  )
)


# =============================================================================
# SERVER
# =============================================================================
server <- function(input, output, session) {

  # ---- Step 1: fetch the data (downloaded once per symbol, then cached) ------
  cache <- new.env()

  raw_data <- reactive({
    sym <- toupper(trimws(input$symbol))
    validate(need(nzchar(sym), "Enter a stock symbol."))
    to   <- Sys.Date()
    from <- to - 365 * HISTORY_YEARS

    key <- paste(input$source, sym, if (input$source == "Upload CSV") input$csv_file$name, sep = "|")
    if (!is.null(cache[[key]])) return(cache[[key]])

    df <- tryCatch({
      if (input$source == "Yahoo Finance") {
        withProgress(message = paste("Downloading", sym, "..."), value = 0.5,
                     check_prices(fetch_yahoo(sym, from, to)))
      } else if (input$source == "Demo data (offline)") {
        check_prices(fetch_demo(sym, from, to))
      } else {
        validate(need(!is.null(input$csv_file), "Choose a CSV file to upload."))
        check_prices(read_price_csv(input$csv_file$datapath))
      }
    }, error = function(e) {
      if (inherits(e, "shiny.silent.error")) stop(e)           # let validate() messages through
      showNotification(conditionMessage(e), type = "error", duration = 12)
      NULL
    })
    validate(need(!is.null(df), "No data to show. See the error message, or try 'Demo data'."))
    cache[[key]] <- df
    df
  })

  # ---- Step 2: filter by date range and time frame ---------------------------
  prices <- reactive({
    df <- raw_data()
    validate(need(input$date_range[1] < input$date_range[2], "Start date must be before the end date."))
    df <- df[df$Date >= input$date_range[1] & df$Date <= input$date_range[2], ]
    validate(need(nrow(df) >= 5, "Fewer than 5 trading days in this date range. Widen the range."))
    change_time_frame(df, input$time_frame)
  })

  # ---- Steps 3 and 4: indicators and trading signals -------------------------
  analysed <- reactive({
    df <- prices()
    n  <- nrow(df)
    validate(
      need(input$ma_short < input$ma_long, "Short MA must be smaller than long MA."),
      need(input$macd_fast < input$macd_slow, "MACD fast period must be smaller than slow."),
      need(input$rsi_oversold < input$rsi_overbought, "RSI oversold level must be below overbought.")
    )
    df$SMA_short <- sma_aligned(df$Close, input$ma_short)
    df$SMA_long  <- sma_aligned(df$Close, input$ma_long)
    df$RSI       <- rsi(df$Close, input$rsi_period)
    m <- macd(df$Close, input$macd_fast, input$macd_slow, input$macd_signal)
    df$MACD <- m$macd_line; df$MACD_signal <- m$signal_line; df$MACD_hist <- m$histogram
    df$Signal <- generate_signals(df, input$rule, list(
      ma_short = input$ma_short, ma_long = input$ma_long,
      rsi_period = input$rsi_period, rsi_oversold = input$rsi_oversold,
      rsi_overbought = input$rsi_overbought,
      macd_fast = input$macd_fast, macd_slow = input$macd_slow, macd_signal = input$macd_signal))
    df
  })

  # ---- Summary boxes ---------------------------------------------------------
  output$kpis <- renderUI({
    df <- analysed()
    n  <- nrow(df)
    chg <- if (n > 1) 100 * (df$Close[n] / df$Close[n - 1] - 1) else 0
    last_sig <- tail(df$Signal[df$Signal != "Hold"], 1)
    if (length(last_sig) == 0) last_sig <- "None yet"
    box <- function(label, value, cls = "") div(class = "kpi", div(class = "label", label),
                                                div(class = paste("value", cls), value))
    tagList(
      box("Symbol", toupper(trimws(input$symbol))),
      box("Last close", sprintf("$%.2f", df$Close[n])),
      box("Change", sprintf("%+.2f%%", chg), if (chg >= 0) "up" else "down"),
      box("Period high", sprintf("$%.2f", max(df$High))),
      box("Period low",  sprintf("$%.2f", min(df$Low))),
      box("Latest signal", last_sig, if (last_sig == "Buy") "up" else if (last_sig == "Sell") "down" else "")
    )
  })

  # ---- Step 2 and 3: the price chart plus indicator panels -------------------
  chart_height <- function() 430 + 170 * ("RSI" %in% input$indicators) +
                             190 * ("MACD" %in% input$indicators)

  output$stock_chart <- renderPlot({
    df <- analysed()
    ind <- input$indicators
    n <- nrow(df)

    # bar width in days so candles do not touch
    bar_w <- switch(input$time_frame, Daily = 0.35, Weekly = 2, Monthly = 9)
    df$dir <- ifelse(df$Close >= df$Open, "Up", "Down")
    cols <- c(Up = "#2e9e5b", Down = "#d64545",
              "Short MA" = "#1f77b4", "Long MA" = "#ff7f0e")
    ma_names <- c("Short MA", "Long MA")

    # --- price panel: chart type ---
    p <- ggplot(df, aes(x = Date))
    if (input$chart_type == "Candlestick") {
      p <- p +
        geom_segment(aes(xend = Date, y = Low, yend = High, colour = dir)) +
        geom_rect(aes(xmin = Date - bar_w, xmax = Date + bar_w,
                      ymin = pmin(Open, Close), ymax = pmax(Open, Close), fill = dir)) +
        scale_fill_manual(values = cols, guide = "none")
    } else if (input$chart_type == "Area") {
      base <- min(df$Close) * 0.98
      p <- p +
        geom_ribbon(aes(ymin = base, ymax = Close), fill = "#9ecae1", alpha = 0.6) +
        geom_line(aes(y = Close), colour = "#2b6a99", linewidth = 0.7)
    } else {
      p <- p + geom_line(aes(y = Close), colour = "#2b6a99", linewidth = 0.7)
    }

    # --- Step 3: moving averages overlaid on the price chart ---
    legend_breaks <- character(0)
    if ("Moving Averages" %in% ind) {
      lab_s <- paste0("SMA ", input$ma_short); lab_l <- paste0("SMA ", input$ma_long)
      p <- p +
        geom_line(aes(y = SMA_short, colour = "Short MA"), na.rm = TRUE, linewidth = 0.8) +
        geom_line(aes(y = SMA_long,  colour = "Long MA"),  na.rm = TRUE, linewidth = 0.8)
      legend_breaks <- ma_names
    }

    # --- Step 4: annotate the bars with the signals ---
    if (isTRUE(input$show_signals)) {
      buys  <- df[df$Signal == "Buy", ]
      sells <- df[df$Signal == "Sell", ]
      span  <- diff(range(c(df$Low, df$High)))
      if (nrow(buys) > 0) p <- p +
        geom_point(data = buys, aes(y = Low - 0.06 * span), shape = 24, size = 3.5,
                   fill = "#2e9e5b", colour = "#1c6b3d") +
        geom_text(data = buys, aes(y = Low - 0.125 * span, label = "Buy"),
                  colour = "#1c6b3d", fontface = "bold", size = 3.4)
      if (nrow(sells) > 0) p <- p +
        geom_point(data = sells, aes(y = High + 0.06 * span), shape = 25, size = 3.5,
                   fill = "#d64545", colour = "#8f2323") +
        geom_text(data = sells, aes(y = High + 0.125 * span, label = "Sell"),
                  colour = "#8f2323", fontface = "bold", size = 3.4)
      if (isTRUE(input$show_hold)) {
        holds <- df[df$Signal == "Hold", ]
        p <- p + geom_text(data = holds, aes(y = Close, label = "Hold"),
                           colour = "grey50", size = 2.3, vjust = 1.8, check_overlap = TRUE)
      }
    }

    p <- p +
      scale_colour_manual(values = cols, breaks = legend_breaks, name = NULL) +
      scale_y_continuous(labels = function(v) paste0("$", format(v, big.mark = ","))) +
      labs(title = paste0(toupper(trimws(input$symbol)), " - ", input$chart_type, " chart (",
                          tolower(input$time_frame), ")"),
           subtitle = paste("Trading rule:", input$rule), x = NULL, y = "Price") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top", plot.title = element_text(face = "bold"))

    panels <- list(p)
    xlim <- range(df$Date)
    p <- p + coord_cartesian(xlim = xlim)
    panels[[1]] <- p

    # --- Step 3: RSI panel ---
    if ("RSI" %in% ind) {
      panels[[length(panels) + 1]] <- ggplot(df, aes(Date, RSI)) +
        annotate("rect", xmin = min(df$Date), xmax = max(df$Date), ymin = input$rsi_overbought, ymax = 100,
                 fill = "#d64545", alpha = 0.10) +
        annotate("rect", xmin = min(df$Date), xmax = max(df$Date), ymin = 0, ymax = input$rsi_oversold,
                 fill = "#2e9e5b", alpha = 0.10) +
        geom_hline(yintercept = c(input$rsi_oversold, input$rsi_overbought),
                   linetype = "dashed", colour = "grey55") +
        geom_line(colour = "#7b3fa0", na.rm = TRUE, linewidth = 0.8) +
        scale_y_continuous(limits = c(0, 100), breaks = c(0, input$rsi_oversold, 50,
                                                          input$rsi_overbought, 100)) +
        coord_cartesian(xlim = xlim) +
        labs(x = NULL, y = paste0("RSI (", input$rsi_period, ")")) +
        theme_minimal(base_size = 13)
    }

    # --- Step 3: MACD panel ---
    if ("MACD" %in% ind) {
      df$hist_dir <- ifelse(df$MACD_hist >= 0, "pos", "neg")
      panels[[length(panels) + 1]] <- ggplot(df, aes(Date)) +
        geom_col(aes(y = MACD_hist, fill = hist_dir), width = bar_w * 2, alpha = 0.55) +
        scale_fill_manual(values = c(pos = "#2e9e5b", neg = "#d64545"), guide = "none") +
        geom_line(aes(y = MACD), colour = "#1f77b4", linewidth = 0.8) +
        geom_line(aes(y = MACD_signal), colour = "#ff7f0e", linewidth = 0.8) +
        geom_hline(yintercept = 0, colour = "grey50") +
        coord_cartesian(xlim = xlim) +
        labs(x = NULL, y = "MACD") +
        theme_minimal(base_size = 13)
    }

    # --- stack the panels with aligned axes (price panel is taller) ---
    # hide the repeated date labels on every panel except the bottom one
    if (length(panels) > 1) {
      for (i in seq_len(length(panels) - 1)) {
        panels[[i]] <- panels[[i]] + theme(axis.text.x = element_blank())
      }
    }
    if (length(panels) == 1) {
      print(panels[[1]])
    } else {
      grobs <- lapply(panels, ggplotGrob)
      stacked <- do.call(rbind, c(grobs, list(size = "max")))
      heights <- c(4, 1.5, 1.7)[seq_along(grobs)]
      panel_rows <- which(grepl("^panel", stacked$layout$name))
      stacked$heights[unique(stacked$layout$t[panel_rows])] <- grid::unit(heights, "null")
      grid::grid.newpage()
      grid::grid.draw(stacked)
    }
  }, height = function() chart_height(), res = 96)

  # ---- Step 4: signal tab ----------------------------------------------------
  output$signal_summary <- renderUI({
    df <- analysed()
    tab <- table(factor(df$Signal, levels = c("Buy", "Sell", "Hold")))
    div(
      strong("Rule: "), input$rule, br(),
      strong("Signals in this period: "),
      span(class = "up",   paste(tab["Buy"],  "Buy")),  " | ",
      span(class = "down", paste(tab["Sell"], "Sell")), " | ",
      span(paste(tab["Hold"], "Hold"))
    )
  })

  output$signal_table <- renderTable({
    df <- analysed()
    s  <- df[df$Signal != "Hold", c("Date", "Signal", "Close")]
    validate(need(nrow(s) > 0, "No Buy/Sell signals with these settings. Try other parameters or a longer date range."))
    s$Date  <- format(s$Date)
    s$Close <- sprintf("$%.2f", s$Close)
    s[order(s$Date, decreasing = TRUE), ]
  }, striped = TRUE, spacing = "s")

  output$data_table <- renderTable({
    df <- analysed()
    out <- tail(df[, c("Date", "Open", "High", "Low", "Close", "Volume", "Signal")], 50)
    out$Date   <- format(out$Date)
    out$Volume <- ifelse(is.na(out$Volume), "", format(round(out$Volume), big.mark = ",", trim = TRUE))
    out[order(out$Date, decreasing = TRUE), ]
  }, striped = TRUE, spacing = "s", digits = 2)
}

# =============================================================================
# Launch the app
# =============================================================================
shinyApp(ui, server)
