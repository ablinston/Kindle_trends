
# Process the data from the list to get royalties earned
process_kdp_payment_data <- function(data) {

  # For debugging
  data <- load_kdp_payment_files("F:/Writing - Book/Data/KDP - payments")

  # Compute the month of sales
  data[, sales_month := str_sub(`Sales Period - Start Date`, 6, 7)]
  
  return(all_data)
  
}
