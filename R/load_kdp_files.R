

load_kdp_files <- function(kdp_data_location){

  # Read in data files
  setwd(kdp_data_location)
  
  files_list = list.files(pattern="*.xlsx")
  
  # Loop through each file and add to data frame
  for (file in files_list) {
    
    showNotification(paste0("Loading file: ", file), id = "loading", duration = NULL)
    
    # If it's the first file, create the data table
    if (file == files_list[1]) {
      
      # All sales
      sales_data <- 
        read_excel(file, sheet = "Combined Sales") %>%
        as.data.table
      
      # Kindle unlimited page reads
      kenp_data <- 
        read_excel(file, sheet = "KENP Read") %>%
        as.data.table
      
    # If it's not the first file, attach to the old list
    } else {
      
      sales_data <- 
        rbindlist(
          list(sales_data,
               read_excel(file, sheet = "Combined Sales") %>%
                 as.data.table),
          fill = TRUE
        )
      
      kenp_data <- 
        rbindlist(
          list(kenp_data,
               read_excel(file, sheet = "KENP Read") %>%
                 as.data.table),
          fill = TRUE
        )
      
    }
    
  }

  return(list(sales_data = sales_data, kenp_data = kenp_data))
  
}
