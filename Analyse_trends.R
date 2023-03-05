## This script will read in data from the KDP dashboard and then analyse
# smoothed trends in sales and estimated royalties over time

library(readxl)
library(data.table)
library(dplyr)
library(quantmod)
library(ggplot2)

# Inputs ------------------------------------------------------------------


# Data files should be exported from the KDP dashboard and saved in a single folder
kdp_data_location <- "F:/Writing - Book/Sales Data/KDP"

# Set the average royalty rate to use for KENP (in USD)
kenp_royalty_per_page_read <- 0.004561577

# Define how many days history to include in the final charts
days <- 120 


# Calculations ------------------------------------------------------------



# Read in data files
setwd(kdp_data_location)

files_list = list.files(pattern="*.xlsx")

# Loop through each file and add to data frame
for (file in files_list) {
  
  # If it's the first file, create the data table
  if (file == files_list[1]) {
    
    # All sales
    sales_data <- 
      read_excel(file, sheet = "Combined Sales") %>%
      as.data.table
    
    # Kindle unlimited page reads
    kenp_data <- 
      read_excel(file, sheet = "KENP Read") %>%
      as.data.table
    
  # If it's not the first file, attach to the old list
  } else {
    
    sales_data <- 
      rbindlist(
        list(sales_data,
             read_excel(file, sheet = "Combined Sales") %>%
               as.data.table),
        fill = TRUE
      )
    
    kenp_data <- 
      rbindlist(
        list(kenp_data,
             read_excel(file, sheet = "KENP Read") %>%
               as.data.table),
        fill = TRUE
      )
    
  }
  
}

# Remove any duplicate rows that have been included by mistake
sales_data_no_duplicates <-
  unique(sales_data, by = c("Royalty Date", "ASIN/ISBN", "Marketplace", "Royalty Type", "Transaction Type"))

kenp_data_no_duplicates <-
  unique(kenp_data, by = c("Date", "ASIN", "Marketplace"))
         
# Compute sales figures in GBP

# Get currencies needed for conversion
currency_conversions <- 
  getQuote(paste0("GBP", 
                  unique(sales_data_no_duplicates$Currency), "=X"))

currency_lookup <- 
  data.table(Currency = unique(sales_data_no_duplicates$Currency),
             XR = currency_conversions$Open)

# Merge exchange rates onto table
sales_data_no_duplicates <-
  merge(
    sales_data_no_duplicates,
    currency_lookup,
    by = "Currency"
  )

# Compute GBP sales
sales_data_no_duplicates[, GBP_royalty := Royalty / XR]
setnames(sales_data_no_duplicates, c("Royalty Date", "ASIN/ISBN"), c("Date", "ASIN"))

# Compute GBP KENP
kenp_data_no_duplicates[, kenp := fifelse(is.na(`Kindle Edition Normalized Pages (KENP) Read from KU and KOLL`),
                                          `Kindle Edition Normalized Page (KENP) Read`,
                                          `Kindle Edition Normalized Pages (KENP) Read from KU and KOLL`)]
kenp_data_no_duplicates[, GBP_royalty := kenp * kenp_royalty_per_page_read / currency_lookup$XR[currency_lookup$Currency == "USD"]]

# Sum all royalties for each date
total_royalties <- rbindlist(list(sales_data_no_duplicates[, .(Date, ASIN, Marketplace, GBP_royalty)],
                                  kenp_data_no_duplicates[, .(Date, ASIN, Marketplace, GBP_royalty)]))

# Fix format of date
total_royalties[, Date := as.Date(Date, "%Y-%m-%d")]

# Aggregate by date
aggregated_royalties <- total_royalties[, .(Royalty = sum(GBP_royalty)), keyby = Date]

# Compute 7-day moving average
aggregated_royalties[, Royalty_ma := frollmean(Royalty, n = 7, algo = "exact", align = "right")]

# Plot the moving average as a chart
ggplot(aggregated_royalties %>% tail(days),
       aes(x = Date, y = Royalty_ma)) +
  geom_line() +
  ylim(0, 70)
