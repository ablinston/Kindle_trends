# This function validates whether the KDP payment data has the columns the rest of the code needs

validate_payment_data <- function(datatable){

  # Edit the columns needed here
  sales_columns <- c("Sales Period - Start Date",
                     "Sales Period - End Date",
                     "Marketplace",
                     "Date",
                     "Currency...15",
                     "Accrued Royalty",
                     "Tax Withholding",
                     "Adjustments",
                     "Net Earnings",
                     "Source",
                     "FX Rate",
                     "Currency...8",
                     "Payout Amount")
  
  # Check the columns are as expected
  for (col_name in sales_columns) {
    if (!(col_name %in% colnames(datatable))) {
      stop(paste0(col_name, " is missing from a sales data file"))
    }
  }
  
  # If we get to here without errors, can continue
  return(NULL)
  
}
