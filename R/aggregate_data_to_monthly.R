# This function processes raw bank data read in by the app.

aggregate_data_to_monthly <- function(processed_royalty_data,
                                      processed_bank_data,
                                      AMS_data,
                                      facebook_data){

  # Separate month and year
  processed_royalty_data[, ":=" (Year = year(Date),
                                 Month = month(Date))]
  
  # Aggregate income by month and year
  aggregated_royalty_data <- 
    processed_royalty_data[, .(KDP_Income = sum(GBP_royalty)),
                           keyby = c("Marketplace", "Year", "Month")]
  
  # Aggregate the bank data
  aggregated_bank_data <- 
    processed_bank_data[, .(Income = sum(Amount[Category == "Income"]),
                Facebook_Ads = sum(Amount[Category == "Facebook Ads"]),
                AMS_Ads = sum(Amount[Category == "AMS Ads"]),
                Other_Ad_Costs = sum(Amount[Category == "Other Ad Costs"]),
                Other_Expenses = sum(Amount[Category == "Other expenses"]),
                Net_income = sum(Amount)),
            keyby = c("Month", "Year")]
  
  # Merge all data together at marketplace level
  merged_monthly_data <-
    aggregated_royalty_data %>%
    merge(AMS_data[, .(Year, Month, Marketplace, AMS_Ads)],
          by = c("Marketplace", "Year", "Month"),
          all.x = TRUE) %>%
    merge(facebook_data,
          by = c("Marketplace", "Year", "Month"),
          all.x = TRUE) 
  
  # Add a total onto the data
  
  merged_monthly_data <- 
    rbindlist(list(
      merged_monthly_data,
      merged_monthly_data[, .(Marketplace = "Total",
                              KDP_Income = sum(KDP_Income, na.rm = TRUE), 
                              AMS_Ads = sum(AMS_Ads, na.rm = TRUE),
                              Facebook_Ads = sum(Facebook_Ads, na.rm = TRUE)),
                          keyby = c("Year", "Month")]
    ),
    use.names = TRUE)
  
  # Merge on any additional expenses
  merged_monthly_data <- 
    merged_monthly_data %>% 
    merge(aggregated_bank_data[, -c("Income", "AMS_Ads", "Facebook_Ads"), with = FALSE] %>%
            .[, Marketplace := "Total"],
          by = c("Marketplace", "Year", "Month"),
          all.x = TRUE)
  
  # Replace an NA with 0
  merged_monthly_data[is.na(merged_monthly_data)] <- 0
  
  # Compute final net income
  merged_monthly_data[, ":=" (Total_Ad_Spend = Facebook_Ads + AMS_Ads + Other_Ad_Costs,
                              Net_income = KDP_Income + AMS_Ads + Facebook_Ads + Other_Ad_Costs + Other_Expenses,
                              Net_regular_income = KDP_Income + Facebook_Ads + AMS_Ads + Other_Ad_Costs)
  ][order(Marketplace, Year, Month),]
  
  # Format the outputs
  for (cols in colnames(merged_monthly_data)[-c(1:3)]) {
    if (is.numeric(merged_monthly_data %>% pull(cols))) {
      merged_monthly_data[, (cols) := formatC(get(cols), digits = 2, format = "f", big.mark = ",")]
    }
  }
  
  return(merged_monthly_data)
  
}



