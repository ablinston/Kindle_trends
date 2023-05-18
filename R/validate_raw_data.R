# This function validates whether the KDP data has the columns the rest of the code needs

validate_raw_data <- function(datatable,
                              sales = FALSE,
                              kenp = FALSE){

  # Check we know the type of data
  if (!sales & !kenp) {
    stop("validate_raw_data function needs either sales or kenp to be TRUE")
  }
  
  # Edit the columns needed here
  sales_columns <- c("Royalty Date", "Title", "Author Name", "ASIN/ISBN", "Marketplace", "Royalty Type", "Transaction Type", "Currency")
  
  # Check the columns are as expected
  if (sales) {
    for (col_name in sales_columns) {
      if (!(col_name %in% colnames(datatable))) {
        stop(paste0(col_name, " is missing from a sales data file"))
      }
    }
  }
  
  kenp_columns <- c("Date", "Title", "Author Name", "ASIN", "Marketplace")
  variable_kenp_columns <- c("Kindle Edition Normalized Page (KENP) Read", 
                             "Kindle Edition Normalized Pages (KENP) Read from KU and KOLL")
  
  if (kenp) {
    for (col_name in kenp_columns) {
      if (!(col_name %in% colnames(datatable))) {
        stop(paste0(col_name, " is missing from a sales data file"))
      }
    }
    if (!(variable_kenp_columns[1] %in% colnames(datatable)) &
        (!(variable_kenp_columns[2] %in% colnames(datatable)))) {
          stop("KENP column missing from a data file")
        }
  }

  # If we get to here without errors, can continue
  return(NULL)
  
}
