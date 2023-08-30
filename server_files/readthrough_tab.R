

# Get data ready for charts
observe({
  # Check whether the royalty data exists
  req(data_output$combined_data, input$rolling_sum_days)
  # browser()
  
  # WROTE THIS CODE TO EVENTUALLY ALLOW MULTIPLE SERIES AND REMOVE LOOPS. IT WORKS
  
  # data_output$combined_data_readthrough <- 
  #   rbindlist(list(data_output$combined_data[, .(Date, ASIN, Marketplace, orders, kenp)],
  #                  data_output$combined_data[, .(orders = sum(orders, na.rm = TRUE),
  #                                                kenp = sum(kenp, na.rm = TRUE)),
  #                                            by = c("Date", "ASIN")][, Marketplace := "All"]),
  #             use.names = TRUE) %>%
  #   # Merge on series info
  #   .[series_info[, .(ASIN, series, book, kenp_length)], on = "ASIN"]
  # 
  # # Set order for data for correct rolling sums
  # setorderv(data_output$combined_data_readthrough, c("ASIN", "Marketplace", "Date"))
  # 
  # # Compute rolling sums for readthrough
  # data_output$combined_data_readthrough[, ":=" (
  #   order_rollsum = frollsum(
  #     orders,
  #     n = input$rolling_sum_days,
  #     algo = "exact",
  #     align = "right"
  #   ),
  #   ku_rollsum = frollsum(
  #     kenp,
  #     n = input$rolling_sum_days,
  #     algo = "exact",
  #     align = "right"
  #   ) / kenp_length
  # ),
  # keyby = c("ASIN", "Marketplace")]
  # 
  # # Set new order for book and series readthrough calcs
  # setorderv(data_output$combined_data_readthrough, c("Marketplace", "Date", "series", "book"))
  # 
  # # Calculate the readthrough for each book using leads
  # data_output$combined_data_readthrough[, ":=" (prior_book_order_rollsum = shift(order_rollsum, n = 1, type = "lag"),
  #                                               prior_book_ku_rollsum = shift(ku_rollsum, n = 1, type = "lag")),
  #                                       keyby = c("Marketplace", "Date", "series")
  # # Calculate readthrough
  # ][, ":=" (sales_readthrough = order_rollsum / prior_book_order_rollsum,
  #           ku_readthrough = ku_rollsum / prior_book_ku_rollsum)]
  
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
  
  # browser()
})

# Filter the read-through data based on selections
observe({
  
  req(data_output$wide_combined, input$historic_days_readthrough, input$rolling_sum_days, input$readthrough_filter)

  data_output$readthrough_filtered <-
    data_output$wide_combined[(Date >= max(Date) - input$historic_days_readthrough) &
                                (Marketplace == input$readthrough_filter),]
  
})


output$table_readthrough <- renderTable({
  if(input$historic_days_readthrough > 0 & !is.null(data_output$combined_data)) {
    
    # Create a summary of all the read through rates for all markets
    pivot_cols_1 <- colnames(data_output$wide_combined)[grepl("_readthrough_", colnames(data_output$wide_combined))]
    pivot_cols_2 <- colnames(data_output$wide_combined)[grepl("_sample_size_", colnames(data_output$wide_combined))]

    cbind(
      data_output$wide_combined[, c("Date", "Marketplace", pivot_cols_1), with = FALSE] %>%
        pivot_longer(
          cols = pivot_cols_1,
          names_to = c("channel", "ASIN"),
          names_sep = "_readthrough_",
          values_to = "Read-through rate"
        ) %>%
        filter(Date == max(Date)),
      data_output$wide_combined[, c("Date", "Marketplace", pivot_cols_2), with = FALSE] %>%
        pivot_longer(
          cols = pivot_cols_2,
          names_to = c("channel", "ASIN"),
          names_sep = "_sample_size_",
          values_to = "Sample Size"
        ) %>%
        filter(Date == max(Date)) %>%
        select(`Sample Size`)
    ) %>%
      mutate(
        `Read-through rate` = round(100 * `Read-through rate`, 1),
        `Sample Size` = round(`Sample Size`, 0)) %>%
      merge(series_info[, c("ASIN", "name", "book")],
            by = "ASIN") %>%
      filter(`Sample Size` > 10) %>%
      arrange(book, channel, Marketplace) %>%
      rename_with(toupper) %>%
      select(NAME, MARKETPLACE, CHANNEL, `READ-THROUGH RATE`, `SAMPLE SIZE`)
  }
})


output$chart_VL_sales_readthrough_all <- renderPlotly({
  if(input$historic_days_readthrough > 0 & !is.null(data_output$combined_data)) {
    temp <- data_output$readthrough_filtered %>%
      ggplot(aes(x = Date, y = sales_readthrough_B08766L2BZ)) +
      geom_line() +
      ylim(0, max(data_output$wide_all_markets$sales_readthrough_B08766L2BZ)) +
      ggtitle("Sales read-through")
    
    ggplotly(temp)
  }
})

output$chart_VL_ku_readthrough_all <- renderPlotly({
  if(input$historic_days_readthrough > 0 & !is.null(data_output$combined_data)) {
    temp <- data_output$readthrough_filtered %>%
      ggplot(aes(x = Date, y = ku_readthrough_B08766L2BZ)) +
      geom_line() +
      ylim(0, max(data_output$wide_all_markets$ku_readthrough_B08766L2BZ)) +
      ggtitle("KU read-through")
    
    ggplotly(temp)
  }
})

output$chart_fate_sales_readthrough_all <- renderPlotly({
  if(input$historic_days_readthrough > 0 & !is.null(data_output$combined_data)) {
    temp <- data_output$readthrough_filtered %>%
      ggplot(aes(x = Date, y = sales_readthrough_B09GPMRTF7)) +
      geom_line() +
      ylim(0, max(data_output$wide_all_markets$sales_readthrough_B09GPMRTF7)) +
      ggtitle("Sales read-through")
    
    ggplotly(temp)
  }
})

output$chart_fate_ku_readthrough_all <- renderPlotly({
  if(input$historic_days_readthrough > 0 & !is.null(data_output$combined_data)) {
    temp <- data_output$readthrough_filtered %>%
      ggplot(aes(x = Date, y = ku_readthrough_B09GPMRTF7)) +
      geom_line() +
      ylim(0, max(data_output$wide_all_markets$ku_readthrough_B09GPMRTF7)) +
      ggtitle("KU read-through")
    
    ggplotly(temp)
  }
})

output$chart_revenge_sales_readthrough_all <- renderPlotly({
  if(input$historic_days_readthrough > 0 & !is.null(data_output$combined_data)) {
    temp <- data_output$readthrough_filtered %>%
      ggplot(aes(x = Date, y = sales_readthrough_B0BHR5YXXV)) +
      geom_line() +
      ylim(0, max(data_output$wide_all_markets$sales_readthrough_B0BHR5YXXV)) +
      ggtitle("Sales read-through")
    
    ggplotly(temp)
  }
})

output$chart_revenge_ku_readthrough_all <- renderPlotly({
  if(input$historic_days_readthrough > 0 & !is.null(data_output$combined_data)) {
    temp <- data_output$readthrough_filtered %>%
      ggplot(aes(x = Date, y = ku_readthrough_B0BHR5YXXV)) +
      geom_line() +
      ylim(0, max(data_output$wide_all_markets$ku_readthrough_B0BHR5YXXV)) +
      ggtitle("KU read-through")
    
    ggplotly(temp)
  }
})

