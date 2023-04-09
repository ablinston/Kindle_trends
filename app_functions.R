

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

  setkeyv(data_list$sales_data, NULL)
  setkeyv(data_list$kenp_data, NULL)

  # Remove any duplicate rows that have been included by mistake and remove free giveaways
  sales_data_no_duplicates <-
    data_list$sales_data %>%
    unique(by = c("Royalty Date", "Title", "Author Name", "ASIN/ISBN", "Marketplace", "Royalty Type", "Transaction Type", "Currency"),
           fromLast = TRUE) %>%
    .[
      # Royalty != 0,][
        , .(
          `Net Units Sold` = sum(`Net Units Sold`, na.rm = TRUE),
          Royalty = sum(Royalty, na.rm = TRUE),
          Currency = first(Currency)
        ),
        by = c("Royalty Date", "ASIN/ISBN", "Marketplace")]
  
  kenp_data_no_duplicates <-
    data_list$kenp_data %>%
    unique(by = c("Date", "Title", "Author Name", "ASIN", "Marketplace"),
           fromLast = TRUE) %>%
    .[, .(kenp = sum(`Kindle Edition Normalized Page (KENP) Read`, na.rm = TRUE) +
                              sum(`Kindle Edition Normalized Pages (KENP) Read from KU and KOLL`, na.rm = TRUE)),
                        by = c("Date", "ASIN", "Marketplace")]
  
  # Fix format of date
  sales_data_no_duplicates[, Date := as.Date(`Royalty Date`, "%Y-%m-%d")]
  kenp_data_no_duplicates[, Date := as.Date(Date, "%Y-%m-%d")]
  
  # Sort by date
  setorder(sales_data_no_duplicates, Date)
  setorder(kenp_data_no_duplicates, Date)
  
  # Fill in missing days
  sales_data_all_days <- 
    data.table(expand.grid(
      Date = seq(as.Date(min(sales_data_no_duplicates$Date, kenp_data_no_duplicates$Date)),
                 as.Date(max(sales_data_no_duplicates$Date, kenp_data_no_duplicates$Date)),
                 by = "days"
      ),
      `ASIN/ISBN` = unique(sales_data_no_duplicates$`ASIN/ISBN`),
      Marketplace = unique(sales_data_no_duplicates$Marketplace)
    )) %>%
    merge(sales_data_no_duplicates,
          by = c("Date", "ASIN/ISBN", "Marketplace"),
          all.x = TRUE)
  
  kenp_data_all_days <- 
    data.table(expand.grid(
      Date = seq(as.Date(min(sales_data_no_duplicates$Date, kenp_data_no_duplicates$Date)),
                 as.Date(max(sales_data_no_duplicates$Date, kenp_data_no_duplicates$Date)),
                 by = "days"
      ),
      ASIN = unique(kenp_data_no_duplicates$ASIN),
      Marketplace = unique(kenp_data_no_duplicates$Marketplace)
    )) %>%
    merge(kenp_data_no_duplicates,
          by = c("Date", "ASIN", "Marketplace"),
          all.x = TRUE)
  # Compute sales figures in GBP
  
  # Get currencies needed for conversion
  currency_conversions <- 
    getQuote(paste0("GBP", 
                    unique(sales_data_all_days$Currency), "=X"))
  
  currency_lookup <- 
    data.table(Currency = unique(sales_data_all_days$Currency),
               XR = currency_conversions$Open)
  
  # Merge exchange rates onto table
  sales_data_all_days <-
    merge(
      sales_data_all_days,
      currency_lookup,
      by = "Currency"
    )

  # Compute GBP sales
  sales_data_all_days[, ":=" (GBP_royalty = Royalty / XR)]
  setnames(sales_data_all_days, c("ASIN/ISBN", "Net Units Sold"), c("ASIN", "orders"))
  
  # Compute GBP KENP
  kenp_data_all_days[, GBP_royalty := kenp * kenp_royalty_per_page_read / currency_lookup$XR[currency_lookup$Currency == "USD"]]
  
  # Sum all royalties for each date
  all_data <- rbindlist(list(sales_data_all_days[, .(Date, ASIN, Marketplace, GBP_royalty, orders)],
                             kenp_data_all_days[, .(Date, ASIN, Marketplace, GBP_royalty, kenp)]),
                        fill = TRUE)

  # Aggregate the data to include one entry per day
  all_data <- 
    all_data[, .(GBP_royalty = sum(GBP_royalty, na.rm = TRUE),
                 orders = sum(orders, na.rm = TRUE),
                 kenp = sum(kenp, na.rm = TRUE)),
             by = c("Date", "ASIN", "Marketplace")]
  
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

