# This function reads in xlsx files and combines them into a data table
# It uses historic data where available to save time in reading in files

load_kdp_files <- function(kdp_data_location){

  # Read in data files
  
  files_list = list.files(kdp_data_location, pattern="*.xlsx")
  
  # Load existing data file if it exists
  if (file.exists("data/saved_raw_sales_data.Rds")) {
    
    existing_data <- readRDS("data/saved_raw_sales_data.Rds")
    
    # Make a list of files already processed within this data
    existing_data_filenames <- unique(c(existing_data$sales_data$source, existing_data$kenp_data$source))
    
  } else {
    existing_data <- NULL
    existing_data_filenames <- NULL
  }
  
  # Loop through each file and add to data frame
  for (file in files_list) {
    
    # showNotification(paste0("Processing file: ", file), id = "loading", duration = NULL)
    
    # If it's not already in the data then we process the file, otherwise skip
    if (!(file %in% existing_data_filenames)) {
      
      # All sales
      new_sales_data <- 
        read_excel(file.path(kdp_data_location, file), sheet = "Combined Sales") %>%
        as.data.table
      
      validate_raw_data(new_sales_data, sales = TRUE)
      
      # Kindle unlimited page reads
      new_kenp_data <- 
        read_excel(file.path(kdp_data_location, file), sheet = "KENP Read") %>%
        as.data.table
      
      validate_raw_data(new_kenp_data, kenp = TRUE)
      
      # If it's the first file, create the data table
      if (!exists("sales_data")) {
        
        # All sales
        sales_data <- new_sales_data
        
        # Kindle unlimited page reads
        kenp_data <- new_kenp_data
        
        # Add an identifier of the filename the data is from
        sales_data$source <- file
        kenp_data$source <- file
        
        # If it's not the first file, attach to the old list
      } else {
        
        new_sales_data[, source := file]
        
        sales_data <- 
          rbindlist(
            list(sales_data,
                 new_sales_data),
            fill = TRUE
          )
        
        new_kenp_data[, source := file]
        
        kenp_data <- 
          rbindlist(
            list(kenp_data,
                 new_kenp_data),
            fill = TRUE
          )
        
      }
      
      rm(new_sales_data, new_kenp_data)
      
    }
    
  }

  # Add any new data to the existing data
  if(exists("sales_data")) {
    if (!is.null(existing_data)) {
      
      sales_data <- rbindlist(
        list(existing_data$sales_data,
             sales_data),
        fill = TRUE
      )
      
      kenp_data <- rbindlist(
        list(existing_data$kenp_data,
             kenp_data),
        fill = TRUE
      )
    } 
  } else {
    
    sales_data <- existing_data$sales_data
    kenp_data <- existing_data$kenp_data
    
  }
  
  # Remove any files that are no longer in the data folder
  if (!is.null(existing_data)) {
    
    sales_data <- sales_data[source %in% files_list,]
    kenp_data <- kenp_data[source %in% files_list,]
    
  }
  
  # Save the data as a new existing data file
  saveRDS(list(sales_data = sales_data,
               kenp_data = kenp_data),
          "data/saved_raw_sales_data.Rds")
  

  return(list(sales_data = sales_data, kenp_data = kenp_data))
  
}
