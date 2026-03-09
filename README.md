# STAN — STock ANalyser

Interactive **R Shiny application** for analyzing the performance of **CAC 40 stocks**.

Developed by **Jean-Baptiste Knight Peterson**  
Master 2 Applied Mathematics & Statistics

---

## Overview

**STAN** is a web application built with **R Shiny** that allows users to explore and analyze the historical performance of stocks from the **CAC 40 index**.

The application provides:

- Financial indicators (latest price, volatility, CAGR)
- Performance analysis across multiple time horizons
- Linear regression of log-prices to estimate trends
- Interactive visualization with Plotly
- Data management via Yahoo Finance or CSV import

---

## Features

### Financial Indicators

- Latest closing price
- Date of last update
- Volatility of daily log returns
- CAGR (Compound Annual Growth Rate)

### Performance Analysis

Performance over different horizons:

- 1 Month
- 6 Months
- 1 Year
- 3 Years
- 5 Years

The app also computes:

- Linear regression on log(price)
- Growth rate estimation
- Residual analysis

---

### Interactive Visualization

The dashboard includes an interactive chart with:

- Log-scale price visualization
- Linear regression trend line
- ±σ and ±2σ bands around the regression
- Zoom, hover, and export features (Plotly)

---

### Data Management

The application allows:

- Downloading stock data from Yahoo Finance
- Updating a single stock
- Updating all stocks
- Importing CSV or Boursorama TXT files

---

## Project Structure

stan-stock-analyser
│
├── app.R
├── download_data.R
├── README.md
├── data/
│   ├── BNP.PA.csv
│   ├── MC.PA.csv
│   └── …
│
├── www/
│   ├── style.css
│   └── readme_style.css

---

## Installation

### Requirements

- R ≥ 4.0
- Recommended: RStudio

Install required packages:

```r
install.packages(c(
"shiny",
"shinydashboard",
"plotly",
"DT",
"dplyr",
"quantmod"
))


⸻

Run the Application

From R:

shiny::runApp()

Or open app.R in RStudio and click Run App.

⸻

Data Sources

Source	Method
Yahoo Finance	quantmod package
Boursorama	CSV / TXT import


⸻

Methodology

The application uses several financial indicators:
	•	Volatility: standard deviation of daily log returns
	•	CAGR: compound annual growth rate
	•	Linear regression on log-prices to estimate long-term trends

These tools help identify long-term growth patterns and deviations from expected trends.

⸻

Technologies
	•	R
	•	Shiny
	•	shinydashboard
	•	Plotly
	•	dplyr
	•	quantmod

⸻

Author

Jean-Baptiste Knight Peterson
Master 2 Applied Mathematics & Statistics

---
