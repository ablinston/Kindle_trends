
# Filter the data ready for the charts
observe({
  # Check whether the royalty data exists
  if(input$historic_months > 0 & !is.null(data_output$combined_data)) {
    # browser()
    data_output$filtered_data <-
      data_output$combined_data[Date >= (max(Date) - (input$historic_months) * 30) &
                                  ASIN %in% series_info$ASIN,]

    data_output$wide_all_markets <-
      data_output$combined_data[, .(orders = sum(orders, na.rm = TRUE),
                                    kenp = sum(kenp, na.rm = TRUE)),
                                by = c("Date", "ASIN")] %>%
      pivot_wider(
        id_cols = c("Date"),
        names_from = c("ASIN"),
        values_from = c("orders", "kenp")
      ) %>%
      replace(is.na(.), 0) %>%
      as.data.table
    
    data_output$wide_split_markets <-
      data_output$combined_data[, ":=" (orders = fifelse(is.na(orders), 0, orders),
                                        kenp = fifelse(is.na(kenp), 0, kenp))] %>%
      pivot_wider(
        id_cols = c("Date", "Marketplace"),
        names_from = c("ASIN"),
        values_from = c("orders", "kenp")
      ) %>%
      replace(is.na(.), 0) %>%
      as.data.table
    
    # Order the data ready for rolling sums
    setorderv(data_output$wide_all_markets, cols = c("Date"))
    setorderv(data_output$wide_split_markets, cols = c("Marketplace", "Date"))
    
    # Go through the books in the series and compute read-through
    for (book in series_info$book) {
      
      # Save the ASIN of the book in question
      asin <- series_info$ASIN[series_info$book == book]
      
      # Compute the historic rolling sum
      data_output$wide_all_markets[, (paste0("sum_order_", asin)) := frollsum(get(paste0("orders_", asin)),
                                                                              input$rolling_sum_days,
                                                                              algo = "exact",
                                                                              align = "right")
      ][, (paste0("sum_kenp_", asin)) := frollsum(get(paste0("kenp_", asin)),
                                                  input$rolling_sum_days,
                                                  algo = "exact",
                                                  align = "right")]
      
      data_output$wide_split_markets[, (paste0("sum_order_", asin)) := frollsum(get(paste0("orders_", asin)),
                                                                                input$rolling_sum_days,
                                                                                algo = "exact",
                                                                                align = "right"),
                                     by = c("Marketplace")
      ][, (paste0("sum_kenp_", asin)) := frollsum(get(paste0("kenp_", asin)),
                                                  input$rolling_sum_days,
                                                  algo = "exact",
                                                  align = "right"),
        by = c("Marketplace")]

      # If not the first book in the series, compute the read-through rate                  
      if (book > 1) {
        prior_asin <- series_info$ASIN[series_info$book == (book - 1)]
        
        data_output$wide_all_markets[, (paste0("sales_readthrough_", asin)) := get(paste0("sum_order_", asin)) / get(paste0("sum_order_", prior_asin))
        ][, (paste0("ku_readthrough_", asin)) := (
          (get(paste0("sum_kenp_", asin)) / series_info$kenp_length[series_info$book == book]) / 
            (get(paste0("sum_kenp_", prior_asin)) / series_info$kenp_length[series_info$book == book - 1]))
          ][, (paste0("ku_sample_size_", asin)) := (get(paste0("sum_kenp_", prior_asin)) / series_info$kenp_length[series_info$book == book - 1])
            ][, (paste0("sales_sample_size_", asin)) := get(paste0("sum_order_", prior_asin))]
        
        data_output$wide_split_markets[, (paste0("sales_readthrough_", asin)) := get(paste0("sum_order_", asin)) / get(paste0("sum_order_", prior_asin)),
                                     by = c("Marketplace")
        ][, (paste0("ku_readthrough_", asin)) := (
          (get(paste0("sum_kenp_", asin)) / series_info$kenp_length[series_info$book == book]) / 
            (get(paste0("sum_kenp_", prior_asin)) / series_info$kenp_length[series_info$book == book - 1])),
          by = c("Marketplace")
          ][, (paste0("ku_sample_size_", asin)) := (get(paste0("sum_kenp_", prior_asin)) / series_info$kenp_length[series_info$book == book - 1]),
            by = c("Marketplace")
          ][, (paste0("sales_sample_size_", asin)) := get(paste0("sum_order_", prior_asin)),
            by = c("Marketplace")]
      }
    }
    
    # Combined the data to allow filtering
    if (!is.null(data_output$wide_all_markets)) {
      data_output$wide_all_markets[, Marketplace := "All"]
      data_output$wide_combined <-
        rbindlist(list(data_output$wide_all_markets,
                       data_output$wide_split_markets),
                  fill = TRUE)
    }
  }
  # browser()
})


# Create the charts for different books
output$chart_all_books_all_countries <- renderPlotly({
  if(input$historic_months > 0 & !is.null(data_output$filtered_data)) {
    data_output$filtered_data %>%
      moving_average_royalty_chart(input$ma_days, include_net = TRUE) %>%
      layout(title = "All individual books, All countries")
  }
})

output$chart_oblivion_all_countries <- renderPlotly({
  if(input$historic_months > 0 & !is.null(data_output$filtered_data)) {
    data_output$filtered_data[ASIN == 'B087676DTB',] %>%
      moving_average_royalty_chart(input$ma_days, ku_prop = TRUE) %>%
      layout(title = "Oblivion, All countries")
  }
})

output$chart_all_books_USA <- renderPlotly({
  if(input$historic_months > 0 & !is.null(data_output$filtered_data)) {
    data_output$filtered_data[Marketplace == 'Amazon.com',] %>%
      moving_average_royalty_chart(input$ma_days, include_net = TRUE) %>%
      layout(title = "All individual books, USA")
  }
})

output$chart_oblivion_USA <- renderPlotly({
  if(input$historic_months > 0 & !is.null(data_output$filtered_data)) {
    data_output$filtered_data[Marketplace == 'Amazon.com' & ASIN == 'B087676DTB',] %>%
      moving_average_royalty_chart(input$ma_days, ku_prop = TRUE) %>%
      layout(title = "Oblivion, USA")
  }
})

output$chart_all_books_UK <- renderPlotly({
  if(input$historic_months > 0 & !is.null(data_output$filtered_data)) {
    data_output$filtered_data[Marketplace == 'Amazon.co.uk',] %>%
      moving_average_royalty_chart(input$ma_days, include_net = TRUE) %>%
      layout(title = "All individual books, UK")
  }
})

output$chart_oblivion_UK <- renderPlotly({
  if(input$historic_months > 0 & !is.null(data_output$filtered_data)) {
    data_output$filtered_data[Marketplace == 'Amazon.co.uk' & ASIN == 'B087676DTB',] %>%
      moving_average_royalty_chart(input$ma_days, ku_prop = TRUE) %>%
      layout(title = "Oblivion, UK")
  }
})

output$chart_all_books_Aus <- renderPlotly({
  if(input$historic_months > 0 & !is.null(data_output$filtered_data)) {
    data_output$filtered_data[Marketplace == 'Amazon.com.au',] %>%
      moving_average_royalty_chart(input$ma_days, include_net = TRUE) %>%
      layout(title = "All individual books, Australia")
  }
})

output$chart_oblivion_Aus <- renderPlotly({
  if(input$historic_months > 0 & !is.null(data_output$filtered_data)) {
    data_output$filtered_data[Marketplace == 'Amazon.com.au' & ASIN == 'B087676DTB',] %>%
      moving_average_royalty_chart(input$ma_days, ku_prop = TRUE) %>%
      layout(title = "Oblivion, Australia")
  }
})

output$chart_all_books_Can <- renderPlotly({
  if(input$historic_months > 0 & !is.null(data_output$filtered_data)) {
    data_output$filtered_data[Marketplace == 'Amazon.ca',] %>%
      moving_average_royalty_chart(input$ma_days, include_net = TRUE) %>%
      layout(title = "All individual books, Canada")
  }
})

output$chart_oblivion_Can <- renderPlotly({
  if(input$historic_months > 0 & !is.null(data_output$filtered_data)) {
    data_output$filtered_data[Marketplace == 'Amazon.ca' & ASIN == 'B087676DTB',] %>%
      moving_average_royalty_chart(input$ma_days, ku_prop = TRUE) %>%
      layout(title = "Oblivion, Canada")
  }
})

output$chart_all_books_ROW <- renderPlotly({
  if(input$historic_months > 0 & !is.null(data_output$filtered_data)) {
    data_output$filtered_data[!(Marketplace %in% c('Amazon.com', 'Amazon.co.uk', 'Amazon.ca', 'Amazon.com.au')),] %>%
      moving_average_royalty_chart(input$ma_days, include_net = TRUE) %>%
      layout(title = "All individual books, Rest of world")
  }
})

output$chart_oblivion_ROW <- renderPlotly({
  if(input$historic_months > 0 & !is.null(data_output$filtered_data)) {
    data_output$filtered_data[!(Marketplace %in% c('Amazon.com', 'Amazon.co.uk', 'Amazon.ca', 'Amazon.com.au')) & ASIN == 'B087676DTB',] %>%
      moving_average_royalty_chart(input$ma_days, ku_prop = TRUE) %>%
      layout(title = "Oblivion, Rest of world")
  }
})


