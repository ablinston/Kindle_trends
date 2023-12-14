
# Process the data from the list to get royalties earned
process_kdp_payment_data <- function(data) {

  # For debugging
  # data <- load_kdp_payment_files("F:/Writing - Book/Data/KDP - payments")

  # Compute custom columns
  data[, ":=" (
    start_date = as.Date(`Sales Period - Start Date`, "%Y-%m-%d"),
    end_date = as.Date(`Sales Period - End Date`, "%Y-%m-%d")
  )]
  
  # Fill in NAs with valid values above where data is missing
  data[, Marketplace := zoo::na.locf(Marketplace, na.rm = FALSE), by = cumsum(!is.na(Marketplace))]
  data[, start_date := zoo::na.locf(start_date, na.rm = FALSE), by = cumsum(!is.na(start_date))]
  data[, end_date := zoo::na.locf(end_date, na.rm = FALSE), by = cumsum(!is.na(end_date))]
  
  # Remove duplicates
  data <- data %>%
    unique(by = c("start_date", "end_date", "Marketplace", "royalty_source"),
           fromLast = TRUE)
  
  # Remove spaces from column names
  new_col_names <- gsub(" ", "_", colnames(data))
  
  # Use setnames to update column names
  setnames(data, old = names(data), new = new_col_names)
  
  return(data)
  
}


