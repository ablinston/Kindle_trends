## This script will read in data from the KDP dashboard and then analyse
# smoothed trends in sales and estimated royalties over time

library(readxl)
library(data.table)
library(dplyr)
library(ggplot2)
library(shiny)
library(tidyr)
library(plotly)
library(lubridate)
library(stringr)
library(DT)

# Set API key for priceR package
Sys.setenv("EXCHANGERATEHOST_ACCESS_KEY" = "07870c0610538ce81a1477b653bcd902")

# Load all functions
for (filename in list.files("R")) {
  source(file.path("R", filename))
}

series_info <- fread("data/series.csv")

# Initial categories for dropdown in cash accounting tab
cash_accounting_itemised_categories <- c("All")