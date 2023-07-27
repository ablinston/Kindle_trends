## This script will read in data from the KDP dashboard and then analyse
# smoothed trends in sales and estimated royalties over time

library(readxl)
library(data.table)
library(dplyr)
library(quantmod)
library(ggplot2)
library(shiny)
library(tidyr)
library(plotly)
library(lubridate)
library(stringr)
library(DT)

# Load all functions
for (filename in list.files("R")) {
  source(file.path("R", filename))
}

series_info <- fread("data/series.csv")

# Initial categories for dropdown in cash accounting tab
cash_accounting_itemised_categories <- c("All")