# This function reads in csv files and combines them into a data table
# It uses historic data where available to save time in reading in files

load_facebook <- function(data_location){

  # Read in data files
  
  files_list = list.files(data_location, pattern="*.csv")
  
  # Load existing data file if it exists
  if (file.exists("private/saved_raw_facebook.Rds")) {
    
    existing_data <- readRDS("private/saved_raw_facebook.Rds")
    
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
      
      # Output stage at
      message(paste0("Processing: ", file))
      
      # All sales
      new_data <- 
        fread(file.path(data_location, file))
      
      # Remove blank row which is total amounts
      new_data <- new_data[!is.na(`Campaign name`),]
      
      # validate_raw_data(new_sales_data, sales = TRUE)

      # If it's the first file, create the data table
      if (nrow(new_data) > 0) {
        if (!exists("dataset")) {
          
          # All sales
          dataset <- new_data
          
          # Add an identifier of the filename the data is from
          dataset$source <- file
          
          # If it's not the first file, attach to the old list
        } else {
          
          new_data[, source := file]
          
          dataset <- 
            rbindlist(
              list(dataset,
                   new_data),
              fill = TRUE
            )
        }
      }

      rm(new_data)
      
    }
    
  }

  # Add any new data to the existing data
  if(exists("dataset")) {
    if (!is.null(existing_data)) {
      
      dataset <- rbindlist(
        list(existing_data,
             dataset),
        fill = TRUE
      )
      
    } 
  } else {
    
    dataset <- existing_data
    
  }
  
  # Remove any files that are no longer in the data folder
  if (!is.null(existing_data)) {
    dataset <- dataset[source %in% files_list,]
  }
  
  # Save the data as a new existing data file
  saveRDS(dataset,
          "private/saved_raw_facebook.Rds")
  
  return(dataset)
  
}
