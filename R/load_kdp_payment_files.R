# This function reads in xlsx files and combines them into a data table
# It uses historic data where available to save time in reading in files

load_kdp_payment_files <- function(kdp_payment_data_location){

  # Read in data files
  
  files_list = list.files(kdp_payment_data_location, pattern="*.xlsx")
  
  # Load existing data file if it exists
  if (file.exists("data/saved_raw_payment_data.Rds")) {
    
    existing_data <- readRDS("data/saved_raw_payment_data.Rds")
    
    # Make a list of files already processed within this data
    existing_data_filenames <- unique(existing_data$source)
    
  } else {
    existing_data <- NULL
    existing_data_filenames <- NULL
  }
  
  # Loop through each file and add to data frame
  for (file in files_list) {
    
    # showNotification(paste0("Processing file: ", file), id = "loading", duration = NULL)
    
    # If it's not already in the data then we process the file, otherwise skip
    if (!(file %in% existing_data_filenames)) {
      
      # All payments data
      new_payment_data <- 
        read_excel(file.path(kdp_payment_data_location, file), sheet = "Payments") %>%
        as.data.table
      
      validate_payment_data(new_payment_data)
      
      # Sort duplicate currency fields and source
      new_payment_data <- new_payment_data %>%
        rename(paid_currency = `Currency...15`,
               Currency = `Currency...8`,
               royalty_source = Source)
      
      # If it's the first file, create the data table
      if (!exists("payment_data")) {
        
        # All payments
        payment_data <- new_payment_data

        # Add an identifier of the filename the data is from
        payment_data$source <- file

        
        # If it's not the first file, attach to the old list
      } else {
        
        new_payment_data[, source := file]
        
        payment_data <- 
          rbindlist(
            list(payment_data,
                 new_payment_data),
            fill = TRUE
          )

      }
      
      rm(new_payment_data)
      
    }
    
  }

  # Add any new data to the existing data
  if(exists("payment_data")) {
    if (!is.null(existing_data)) {
      
      payment_data <- rbindlist(
        list(existing_data,
             payment_data),
        fill = TRUE
      )
      
    } 
  } else {
    
    payment_data <- existing_data
    
  }
  
  # Remove any files that are no longer in the data folder
  if (!is.null(existing_data)) {
    
    payment_data <- payment_data[source %in% files_list,]
    
  }
  
  # Save the data as a new existing data file
  saveRDS(payment_data,
          "data/saved_raw_payment_data.Rds")

  return(payment_data)
  
}
