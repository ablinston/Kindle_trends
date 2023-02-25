## This script will read in data from the KDP dashboard and then analyse
# smoothed trends in sales and estimated royalties over time

library(readxl)
library(data.table)
library(dplyr)

# Data files should be exported from the KDP dashboard and saved in a single folder
kdp_data_location <- "F:/Writing - Book/Sales Data/KDP"

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
    kenp_data2 <- 
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
         