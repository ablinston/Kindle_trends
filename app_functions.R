

load_kdp_files <- function(kdp_data_location){

  # Read in data files
  setwd(kdp_data_location)
  
  files_list = list.files(pattern="*.xlsx")
  
  # Loop through each file and add to data frame
  for (file in files_list) {
    
    showNotification(paste0("Loading file: ", file), id = "loading", duration = NULL)
    
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

  return(list(sales_data = sales_data, kenp_data = kenp_data))
  
}


# Process the data from the list to get royalties earned
process_data_for_royalties <- function(data_list, kenp_royalty_per_page_read) {

  # # For debugging
  # data_list <- load_kdp_files("F:/Writing - Book/Sales Data/KDP")
  # kenp_royalty_per_page_read <- 0.004561577
  
  # Remove any duplicate rows that have been included by mistake
  sales_data_no_duplicates <-
    unique(data_list$sales_data, by = c("Royalty Date", "ASIN/ISBN", "Marketplace", "Royalty Type", "Transaction Type"))
  
  kenp_data_no_duplicates <-
    unique(data_list$kenp_data, by = c("Date", "ASIN", "Marketplace"))
           
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
  
  # Remove free giveaway
  sales_data_no_duplicates <- sales_data_no_duplicates[Royalty != 0,]
  
  # Compute GBP sales
  sales_data_no_duplicates[, ":=" (GBP_royalty = Royalty / XR)]
  setnames(sales_data_no_duplicates, c("Royalty Date", "ASIN/ISBN", "Net Units Sold"), c("Date", "ASIN", "orders"))
  
  # Compute GBP KENP
  kenp_data_no_duplicates[, kenp := fifelse(is.na(`Kindle Edition Normalized Pages (KENP) Read from KU and KOLL`),
                                            `Kindle Edition Normalized Page (KENP) Read`,
                                            `Kindle Edition Normalized Pages (KENP) Read from KU and KOLL`)]
  kenp_data_no_duplicates[, GBP_royalty := kenp * kenp_royalty_per_page_read / currency_lookup$XR[currency_lookup$Currency == "USD"]]
  
  # Sum all royalties for each date
  all_data <- rbindlist(list(sales_data_no_duplicates[, .(Date, ASIN, Marketplace, GBP_royalty, orders)],
                             kenp_data_no_duplicates[, .(Date, ASIN, Marketplace, GBP_royalty, kenp)]),
                        fill = TRUE)
  
  # Fix format of date
  all_data[, Date := as.Date(Date, "%Y-%m-%d")]

  return(all_data)
  
}

# Define a function to plot a chart with a moving average royalty
moving_average_royalty_chart <- function(royalty_data, ma_days) {
  
  # Aggregate by date
  aggregated_royalties <- royalty_data[, .(Royalty = sum(GBP_royalty)), keyby = Date]
  
  # Compute 7-day moving average
  aggregated_royalties[, Royalty_ma := frollmean(Royalty, n = ma_days, algo = "exact", align = "right")]
  
  # Plot the moving average as a chart
  
  return(
    ggplot(aggregated_royalties,
           aes(x = Date, y = Royalty_ma)) +
      geom_line() +
      ylim(0, max(aggregated_royalties$Royalty_ma))
  )
  
}

