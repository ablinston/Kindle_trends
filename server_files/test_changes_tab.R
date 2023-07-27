
# Get the readthrough at the various cut offs
observe({
  # Check whether the royalty data exists
  req(input$changes_start_date, input$changes_change_date, input$changes_end_date, data_output$combined_data_wide)

  # Calculate readthrough changes before and after
  output$readthrough_before_change <- renderDataTable({
    data_output$combined_data_wide[Date >= input$changes_start_date &
                                     Date < input$changes_change_date,] %>%
      calculate_readthrough(series_info) %>%
      data.frame(row.names = 1) %>%
      round(2) %>%
      t()
  }, rownames = TRUE)
  
  output$readthrough_after_change <- renderDataTable({
    data_output$combined_data_wide[Date >= input$changes_change_date &
                                     Date < input$changes_end_date,] %>%
      calculate_readthrough(series_info) %>%
      data.frame(row.names = 1) %>%
      round(2) %>%
      t()
  })
  # Aggregate by date
  aggregated_royalties <- data_output$combined_data[, .(
    Royalty = sum(GBP_royalty),
    AMS_Ads = sum(AMS_Ads),
    Facebook_Ads = sum(Facebook_Ads),
    Sales_royalty = sum(GBP_royalty_sales),
    KU_royalty = sum(GBP_royalty_ku),
    orders = sum(orders),
    ku_sales = sum(ku_sales)),
  keyby = c("Date", "Marketplace")
  ]
  
  # Calculate changes in gross and net royalties
  aggregated_royalties <- rbindlist(list(
    copy(aggregated_royalties)[, .(
      Royalty = sum(Royalty),
      AMS_Ads = sum(AMS_Ads),
      Facebook_Ads = sum(Facebook_Ads),
      Sales_royalty = sum(Sales_royalty),
      KU_royalty = sum(KU_royalty),
      orders = sum(orders),
      ku_sales = sum(ku_sales)),
      keyby = c("Date")
    ][, Marketplace := "All"],
    aggregated_royalties
  ), use.names = TRUE)
    
  aggregated_royalties[, Net_Royalty := Royalty + AMS_Ads + Facebook_Ads]
  
  output$royalties_before_change <- renderDataTable({
    aggregated_royalties[Date >= input$changes_start_date &
                           Date < input$changes_change_date,
    ][, .(
      Mean_Daily_Gross_Royalty = mean(Royalty),
      Mean_Daily_Net_Royalty = mean(Net_Royalty, na.rm = TRUE),
      Mean_Daily_KU_Royalty = mean(KU_royalty),
      Mean_Daily_Sales_Royalty = mean(Sales_royalty)
    ),
    keyby = "Marketplace"][order(-Mean_Daily_Gross_Royalty),]  %>%
      data.frame(row.names = 1) %>%
      round(2) %>%
      t()
  })
  
  output$royalties_after_change <- renderDataTable({
    aggregated_royalties[Date >= input$changes_change_date &
                           Date < input$changes_end_date,
    ][, .(
      Mean_Daily_Gross_Royalty = mean(Royalty),
      Mean_Daily_Net_Royalty = mean(Net_Royalty, na.rm = TRUE),
      Mean_Daily_KU_Royalty = mean(KU_royalty),
      Mean_Daily_Sales_Royalty = mean(Sales_royalty)
    ),
    keyby = "Marketplace"][order(-Mean_Daily_Gross_Royalty),]  %>%
      data.frame(row.names = 1) %>%
      round(2) %>%
      t()
  })
  
})


