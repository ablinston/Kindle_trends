
# Process the data from the list to get royalties earned
process_ams_data <- function(dataset, exchange_rate_data) {

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
  data_no_duplicates <- merge(data_no_duplicates, 
                              exchange_rate_data, 
                              by = "Currency", 
                              all.x = TRUE)
  
  # Format spend and convert to GBP
  data_no_duplicates[, ":=" (AMS_Ads = -as.numeric(gsub("[£$]", "", Spend, perl = TRUE)) / XR,
                             AMS_Ads_Native_Curr = -as.numeric(gsub("[£$]", "", Spend, perl = TRUE)))]
  
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
  daily_data <- data_no_duplicates[, .(AMS_Ads = sum(AMS_Ads, na.rm = TRUE),
                                       AMS_clicks = sum(Clicks, na.rm = TRUE),
                                       AMS_orders = sum(`14 Day Total Orders (#)`, na.rm = TRUE),
                                       AMS_kenp = sum(`14 Day Total KENP Read (#)`, na.rm = TRUE),
                                       AMS_Ads_Native_Curr = sum(AMS_Ads_Native_Curr, na.rm = TRUE),
                                       kenp_royalty_pp = mean(`Estimated KENP royalties` / `14 Day Total KENP Read (#)`, na.rm = TRUE)),
                                   keyby = c("Date", "Marketplace", "ASIN")]

  # Ensure all dates are present
  daily_data <- merge(data.table(expand.grid(
    Date = seq(as.Date(min(c(
      daily_data$Date
    ))),
    as.Date(max(c(
      daily_data$Date
    ))),
    by = "days"),
    ASIN = unique(daily_data$ASIN),
    Marketplace = unique(daily_data$Marketplace)
    )),
    daily_data,
    on = c("Marketplace", "ASIN", "Date"),
    all.x = TRUE) %>%
    replace(is.na(.), 0) %>%
    as.data.table
  
  # Get monthly spend per marketplace
  daily_data[, ":=" (Year = year(Date), Month = month(Date))]
  
  aggregated_data <- daily_data[, .(AMS_Ads = sum(AMS_Ads)),
                                keyby = c("Year", "Month", "Marketplace")]
  
  list(daily_ams_data = daily_data,
       ams_data = aggregated_data) %>%
    return()
  
}
