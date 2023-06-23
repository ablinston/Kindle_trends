# This function processes raw bank data read in by the app.

aggregate_data_to_monthly <- function(processed_royalty_data,
                                      processed_bank_data,
                                      AMS_data){

  # Separate month and year
  processed_royalty_data[, ":=" (Year = year(Date),
                                 Month = month(Date))]
  
  # Aggregate income by month and year
  aggregated_royalty_data <- 
    processed_royalty_data[, .(KDP_Income = sum(GBP_royalty)),
                           keyby = c("Year", "Month")]
  
  # Aggregate the bank data
  aggregated_bank_data <- 
    processed_bank_data[, .(Income = sum(Amount[Category == "Income"]),
                Facebook_Ads = sum(Amount[Category == "Facebook Ads"]),
                AMS_Ads = sum(Amount[Category == "AMS Ads"]),
                Other_Ad_Costs = sum(Amount[Category == "Other Ad Costs"]),
                Other_Expenses = sum(Amount[Category == "Other expenses"]),
                Net_income = sum(Amount)),
            keyby = c("Month", "Year")]
  
  # Merge all data together
  merged_monthly_data <-
  aggregated_royalty_data %>%
    merge(AMS_data[, .(Year, Month, AMS_Ads)],
          by = c("Year", "Month"),
          all.x = TRUE) %>%
    merge(aggregated_bank_data[, -c("Income", "AMS_Ads"), with = FALSE],
          by = c("Year", "Month"),
          all.x = TRUE)
  
  # Compute final net income
  merged_monthly_data[, ":=" (Total_Ad_Spend = Facebook_Ads + AMS_Ads + Other_Ad_Costs,
                              Net_income = KDP_Income + AMS_Ads + Facebook_Ads + Other_Ad_Costs + Other_Expenses,
                              Net_regular_income = KDP_Income + Facebook_Ads + AMS_Ads + Other_Ad_Costs)
  ][order(Year, Month),]
  
  
  return(merged_monthly_data)
  
}



