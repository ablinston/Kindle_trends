

# When the load data button is pressed, read in the KDP data excel files
observeEvent(input$load, {
  
  # KDP data
  data_output$raw_data <- load_kdp_files(input$data_path)
  
  showNotification("Processing data...", id= "loading", duration = NULL)
  
  data_output$combined_data <- process_data_for_royalties(data_output$raw_data,
                                                          input$kenp_royalty_per_page_read)

  # Bank statements
  data_output$raw_bank_data <- load_statements(input$bank_data_path)
  data_output$bank_data <- process_bank_data(data_output$raw_bank_data)

  # AMS data
  data_output$raw_ams_data <- load_ams(input$ams_data_path)
  ams_data <- process_ams_data(data_output$raw_ams_data)
  data_output$ams_data <- ams_data$ams_data
  data_output$daily_ams_data <- ams_data$daily_ams_data
  rm(ams_data)

  # Facebook ad data
  data_output$raw_facebook_data <- load_facebook(input$facebook_data_path)
  facebook_data <- process_facebook_data(data_output$raw_facebook_data,
                                         fread("./data/country_lookup.csv"))
  data_output$facebook_data <- facebook_data$facebook_data
  data_output$daily_facebook_data <- facebook_data$daily_facebook_data
  rm(facebook_data)
  
  # Add data to any inputs needed
  output$bank_data_categories <- renderUI({
    selectInput("cash_accounting_categories",
                "Select categories", 
                choices = c("All", unique(data_output$bank_data$Category)),
                selected = "All")
  })
  
  # Merge the spend data to royalty data
  data_output$combined_data <- 
    data_output$combined_data %>%
    merge(data_output$daily_facebook_data,
          by = c("Date", "Marketplace", "ASIN"),
          all.x = TRUE) %>%
    merge(
      data_output$daily_ams_data %>%
        select(Date, Marketplace, ASIN, AMS_Ads),
      by = c("Date", "Marketplace", "ASIN"),
      all.x = TRUE
    )

  # Replace NA ad spend with 0
  data_output$combined_data$Facebook_Ads[is.na(data_output$combined_data$Facebook_Ads) &
                                           data_output$combined_data$Date <= max(data_output$daily_facebook_data$Date)] <- 0
  data_output$combined_data$AMS_Ads[is.na(data_output$combined_data$AMS_Ads) &
                                           data_output$combined_data$Date <= max(data_output$daily_ams_data$Date)] <- 0
  
  # Calculate the ku sales as a propotion of full book reads
  data_output$combined_data <- merge(data_output$combined_data, series_info[, .(ASIN, kenp_length)], by = "ASIN")
  data_output$combined_data[, ku_sales := kenp / kenp_length
                              ][, kenp_length := NULL]
  
  removeNotification("loading")

})

