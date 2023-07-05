
# Process the data from the list to get royalties earned
process_ams_data <- function(dataset, country_lookup) {

  # # For debugging
  dataset <- load_ams("F:/Writing - Book/Data/AMS")

  # Remove any duplicate rows that have been included by mistake and remove free giveaways
  data_no_duplicates <-
    dataset %>%
    unique(by = c("Date", "Start Date", "Campaign Name", "Currency"),
           fromLast = TRUE)
  
  # Fix format of date
  data_no_duplicates[, Date:= gsub(",", "", Date)][, Date:= gsub(" ", "-", Date)][, Date := as.Date(Date, "%b-%d-%Y")]
  
  # Get exchange rates
  currency_lookup <- get_currency_lookup(c("GBPUSD=X"))
  data_no_duplicates <- merge(data_no_duplicates, 
                              currency_lookup, 
                              by = "Currency", 
                              all.x = TRUE)
  
  # Format spend and convert to GBP
  data_no_duplicates[, AMS_Ads := -as.numeric(gsub("\\$", "", Spend)) / XR]
  
  # Sort by date
  setorder(data_no_duplicates, Date)

  # Get daily spend
  daily_data <- data_no_duplicates[, .(Marketplace = "Amazon.com",
                                       AMS_Ads = sum(AMS_Ads, na.rm = TRUE)),
                                   keyby = "Date"]

  # Get monthly spend per marketplace
  daily_data[, ":=" (Year = year(Date), Month = month(Date))]
  
  aggregated_data <- daily_data[, .(AMS_Ads = sum(AMS_Ads)),
                                keyby = c("Year", "Month", "Marketplace")]
  
  list(daily_ams_data = daily_data,
       ams_data = aggregated_data) %>%
    return()
  
}
