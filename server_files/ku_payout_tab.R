
# Filter the data ready for the charts
observe({
  # Check whether the royalty data exists
  req(data_output$kdp_payment_data, 
      data_output$combined_data,
      input$ku_payout_marketplace, 
      input$historic_months_ku)

  # filter the marketplace we want
  dt <- data_output$kdp_payment_data %>%
    .[Marketplace == input$ku_payout_marketplace &
        royalty_source == "KENP Royalties",] %>%
    .[order(-start_date),] %>%
    .[1:input$historic_months_ku,] %>%
    .[, .(start_date, end_date, Currency, Accrued_Royalty)]
  
  # Check the currency
  currency <- unique(dt$Currency)
  
  # Get the combined data for KU reads and payouts for a given marketplace
  kenp_dt <- data_output$combined_data %>%
    .[Marketplace == input$ku_payout_marketplace & 
        Date >= min(dt$start_date) &
        Date <= max(dt$end_date),] %>%
    .[, .(Date, kenp)]
  
  kenp_dt[, ":=" (start_date = Date,
                  end_date = Date)]
  
  # Merge the data then aggregate
  all_data <- rbindlist(
    list(
      dt,
      kenp_dt
    ),
    fill= TRUE
  )
  
  # Compute year and month for aggregation
  all_data[, ":=" (
    year = year(start_date),
    month = month(start_date)
  )]
  
  agg_dt <- all_data[, .(Royalty = sum(Accrued_Royalty, na.rm = TRUE),
                         kenp = sum(kenp, na.rm = TRUE),
                         End_date = max(end_date)),
                     keyby = c("year", "month")]
  
  payout_colname <- paste0(currency, "_per_kenp")
  
  agg_dt[, (payout_colname) := Royalty / kenp]
  
  # Create chart to show the royalty over time
  output$ku_payout_chart <- renderPlotly({
    agg_dt %>%
      ggplot(aes(x = End_date, y = get(payout_colname))) +
      geom_line() + 
      geom_hline(yintercept = mean(agg_dt %>% pull(payout_colname))) +
      ylab(payout_colname)
    
  })
  
})



