
# Process the data from the list to get royalties earned
process_ams_data <- function(dataset, country_lookup) {

  # # For debugging
  # dataset <- load_ams("F:/Writing - Book/Data/AMS")

  # Calculate the ASIN for those missing (set to Oblivion)
  setnames(dataset, "Advertised ASIN", "ASIN")
  dataset[is.na(ASIN), ASIN := "B087676DTB"]
  
  # Remove any duplicate rows that have been included by mistake and remove free giveaways
  data_no_duplicates <-
    dataset %>%
    unique(by = c("Date", "Start Date", "Campaign Name", "ASIN", "Currency"),
           fromLast = TRUE)
  
  # Get exchange rates
  currency_lookup <- get_currency_lookup(paste0("GBP", unique(data_no_duplicates$Currency), "=X"))
  data_no_duplicates <- merge(data_no_duplicates, 
                              currency_lookup, 
                              by = "Currency", 
                              all.x = TRUE)
  
  # Format spend and convert to GBP
  data_no_duplicates[, AMS_Ads := -as.numeric(gsub("[£$]", "", Spend, perl = TRUE)) / XR]
  
  # Sort by date
  setorder(data_no_duplicates, Date)
  
  # Compute marketplace
  data_no_duplicates[, Marketplace := fcase(Currency == "USD", "Amazon.com", 
                                            Currency == "GBP", "Amazon.co.uk", 
                                            default = "Unknown")]
  
  # Check there are no unknown marketplaces
  if (nrow(data_no_duplicates[Marketplace == "Uknown",]) > 0){
    stop("AMS ads data has unknown currencies")
  }

  # Get daily spend
  daily_data <- data_no_duplicates[, .(AMS_Ads = sum(AMS_Ads, na.rm = TRUE)),
                                   keyby = c("Date", "Marketplace", "ASIN")]

  # Get monthly spend per marketplace
  daily_data[, ":=" (Year = year(Date), Month = month(Date))]
  
  aggregated_data <- daily_data[, .(AMS_Ads = sum(AMS_Ads)),
                                keyby = c("Year", "Month", "Marketplace")]
  
  list(daily_ams_data = daily_data,
       ams_data = aggregated_data) %>%
    return()
  
}
