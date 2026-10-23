# Assignment 6: Portfolio Dashboard (R Shiny)

BDA400 / Data Science Tools and Techniques. Technical Analysis using R, Visualization Phase.
Author: Safiya Muhammad

An interactive dashboard that fetches stock data, draws line, candlestick and area charts, overlays technical indicators, and annotates Buy / Sell signals from a trading rule.

## Run it

1. Open `app.R` in RStudio and click **Run App**, or run `shiny::runApp("app.R")` from this folder.
2. The first run installs any missing packages (`shiny`, `ggplot2`, `quantmod`).
3. Choose a data source in the sidebar. **Yahoo Finance** needs internet; **Demo data (offline)** works without it; **Upload CSV** takes a file with `Date, Open, High, Low, Close` (and optionally `Volume`).

## Features

| Task | What the dashboard does |
|------|-------------------------|
| Data collection | Yahoo Finance through `quantmod`, plus demo data and CSV upload; errors show a clear message instead of crashing |
| Widgets | Symbol, date range, time frame (Daily / Weekly / Monthly), chart type, indicator switches and parameters |
| Charts | Candlestick, line and area charts |
| Indicators | Moving Averages (overlaid on the price chart), RSI and MACD panels, each switched on or off with a checkbox |
| Trading rules | MA Crossover, RSI Overbought/Oversold and MACD Crossover, all with adjustable parameters |
| Annotations | Buy and Sell markers and labels on the bars; optional Hold labels; a Signals tab lists every signal |

The indicators (`sma`, `ema`, `macd`, `rsi`, `crossover`) are the base-R functions written in Assignment 5.

## Files

- `app.R`: the complete dashboard, in sections for Steps 1 to 4
- `portfolio.txt`: the stock symbols from Assignment 2, used as the symbol list
- `SafiyaMuhammad_BDA400_A06.docx`: cover page

*For learning only. This is not investment advice.*
