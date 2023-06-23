
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
      Date = seq(as.Date(min(c(sales_data_no_duplicates$Date, kenp_data_no_duplicates$Date))),
                 as.Date(max(c(sales_data_no_duplicates$Date, kenp_data_no_duplicates$Date))),
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
  # First check whether we can get currencies from net
  currency_lookup <- get_currency_lookup(paste0("GBP",
                                                unique(sales_data_all_days$Currency), "=X"))
  
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
  
  setorderv(all_data, cols = c("Marketplace", "Date"))
  
  return(all_data)
  
}
