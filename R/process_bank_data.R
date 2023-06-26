# This function processes raw bank data read in by the app.

process_bank_data <- function(dataset){

  # dataset <- copy(te)

  # Remove any duplicates
  dataset[, source := NULL]
  dataset <- unique(dataset)
  
  # Format the date column
  dataset[, Date := as.Date(Date, format = "%d/%m/%Y")]
  
  # Filter after the date started using account for business
  dataset <- dataset[Date > "2022-04-06"]
  dataset[, ":=" (Day = as.numeric(format(Date, "%d")),
                  Month = month(Date),
                  Year = year(Date))]
  
  setnames(dataset, "Amount (GBP)", "Amount")
  
  # Filter out any interal transfers & PAYE
  dataset <- dataset[!(`Counter Party` %in% c("Andrew Blinston",
                                              "Liu Jingzhe",
                                              "Jingzhe Liu",
                                              "Andrew Blinston & Liu Jingzhe",
                                              "Cheque",
                                              "B&Q")) &
                       !(`Spending Category` %in% c("HOLIDAYS", "EATING_OUT", "ENTERTAINMENT")),]
  

  # # Reclassify any income that is paid in the first few days of the month as belonging to the previous month
  # dataset[Amount > 0 & Day <= 6, Month := Month - 1]
  # # Amazon pays two months in arrears so change the month
  # dataset[Amount > 0, Month := Month - 2]
  # dataset[Month == 0, ":=" (Month = 12, Year = Year - 1)]
  # dataset[Month == -1, ":=" (Month = 11, Year = Year - 1)]
  # dataset[Month == -2, ":=" (Month = 10, Year = Year - 1)]
  
  # Classify spend and income
  dataset[, Category := "Other expenses"]
  dataset[Amount > 0, Category := "Income"]
  dataset[grepl("FACEBOOK", Reference, ignore.case = TRUE), Category := "Facebook Ads"]
  dataset[grepl("AMZN AD", Reference, ignore.case = TRUE), Category := "AMS Ads"]
  book_selling_costs <- c("Bookfunnel", "BOOKBAR", "FUSSY", "BOOKBUB")
  for (item in book_selling_costs) {
    dataset[grepl(item, Reference, ignore.case = TRUE), Category := "Other Ad Costs"]
  }
  return(dataset)
  
}


