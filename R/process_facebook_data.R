
# Process the data from the list to get royalties earned
process_facebook_data <- function(dataset, country_lookup) {

  # # For debugging
  # dataset <- load_facebook("./private/facebook")
  # country_lookup <- fread("./data/country_lookup.csv")

  # Remove any duplicate rows that have been included by mistake and remove free giveaways
  data_no_duplicates <-
    dataset %>%
    unique(by = c("Campaign name", "Ad Set Name", "Country", "Day"),
           fromLast = TRUE)
  
  # Fix format of date
  data_no_duplicates[, Date := as.Date(Day, "%Y-%m-%d")]
  
  # Sort by date
  setorder(data_no_duplicates, Date)

  # Merge marketplace
  data_no_duplicates <- merge(data_no_duplicates, country_lookup, by = "Country")

  # Get monthly spend per marketplace
  data_no_duplicates[, ":=" (Year = year(Date), Month = month(Date))]
  
  daily_data <- data_no_duplicates[, .(Facebook_Ads = -sum(`Amount spent (GBP)`)),
                                   keyby = c("Date", "Marketplace")]
  
  aggregated_data <- data_no_duplicates[, .(Facebook_Ads = -sum(`Amount spent (GBP)`)),
                                        keyby = c("Year", "Month", "Marketplace")]
  
  return(list(daily_facebook_data = daily_data,
              facebook_data = aggregated_data))
  
}
