
# Calculate the read-through of a series based on read data and the series info
calculate_readthrough <- function(dataset, series_data) {

  # # For debugging
  # dataset <- te %>% filter(Date > "2023-01-02")
  # series_data <- fread("data/series.csv")

  # Set the order for rolling sums
  setorder(dataset, Date)
  
  # Compute rolling sum of all book sales and page reads
  ASINs <- series_data$ASIN
  
  # Loop through each series
  for (series in unique(series_data$series)) {
    
    ASIN <- series_data[series == series & book == 1,]$ASIN
    
    # Calculate rolling sums of first book
    output_data <-
      dataset[, .(col1 = sum(get(paste0("orders_", ASIN)), na.rm = TRUE),
                  col2 = sum(get(paste0("kenp_", ASIN)), na.rm = TRUE)),
              keyby = "Marketplace"]
    
    setnames(output_data, c("col1", "col2"), c(paste0("sum_orders_", ASIN), paste0("sum_kenp_", ASIN)))
    
    # Loop through each book starting with the second
    for (book_no in 2:max(series_data[series == series,]$book)){
      
      previous_ASIN <- ASIN
      ASIN <- series_data[series == series & book == book_no,]$ASIN
      
      # Compute sum of book sales/reads
      temp_data <-
        dataset[, .(col1 = sum(get(paste0("orders_", ASIN)), na.rm = TRUE),
                    col2 = sum(get(paste0("kenp_", ASIN)), na.rm = TRUE)),
                keyby = "Marketplace"]
        
      setnames(temp_data, c("col1", "col2"), c(paste0("sum_orders_", ASIN), paste0("sum_kenp_", ASIN)))
      
      # Merge with output data
      
      output_data <- output_data %>%
        merge(temp_data,
              by = "Marketplace",
              all = TRUE)
      rm(temp_data)
      
      # Compute daily read-through
      output_data[, (paste0("Sales_Series", series, "_Book", book_no - 1, "_Book", book_no, "_readthrough")) := 
                    100 * (get(paste0("sum_orders_", ASIN)) / get(paste0("sum_orders_", previous_ASIN)))]
      output_data[, (paste0("KU_Series", series, "_Book", book_no - 1, "_Book", book_no, "_readthrough")) := 
                    100 * ((get(paste0("sum_kenp_", ASIN)) / series_data[series == series & book == book_no,]$kenp_length) /
                             (get(paste0("sum_kenp_", previous_ASIN)) / series_data[series == series & book == (book_no - 1),]$kenp_length))]
      
    }
    
    output_data[, ":=" (Sales_Sample_Size = get(paste0("sum_orders_", series_data[series == series & book == 1,]$ASIN)),
                        KU_Sample_Size = get(paste0("sum_kenp_", series_data[series == series & book == 1,]$ASIN)) / 
                         series_data[series == series & book == 1,]$kenp_length)]
    
    setnames(output_data, c("Sales_Sample_Size", "KU_Sample_Size"), paste0(c("Sales", "KU"), "_Series", series, "_Sample_Size"))
    
    # Output data for series
    if (exists("final_data")) {
      final_data <- final_data %>%
        merge(output_data,
              by = "Marketplace",
              all = TRUE)
    } else {
      final_data <- output_data
    }
    
    rm(output_data)
  }
  
  # Order by sales so most useful marketplaces at top
  setorderv(final_data, cols = "Sales_Series1_Sample_Size", order = -1)

  # Only select columns of interest
  final_data[, c("Marketplace", 
                 sort(colnames(final_data)[str_detect(colnames(final_data), "_readthrough") | 
                                             str_detect(colnames(final_data), "Sample_")])),
             with = FALSE] %>%
    return()
  
}
