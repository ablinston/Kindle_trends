

# Get data ready for charts
observe({
  # Check whether the royalty data exists
  req(data_output$combined_data, input$rolling_sum_days)
  browser()
  # WROTE THIS CODE TO EVENTUALLY ALLOW MULTIPLE SERIES AND REMOVE LOOPS. IT WORKS
  data_output$combined_data_readthrough <-
    rbindlist(list(data_output$combined_data[, .(Date, ASIN, Marketplace, orders, kenp)],
                   data_output$combined_data[, .(orders = sum(orders, na.rm = TRUE),
                                                 kenp = sum(kenp, na.rm = TRUE)),
                                             by = c("Date", "ASIN")][, Marketplace := "All"]),
              use.names = TRUE) %>%
    # Merge on series info
    .[series_info, on = "ASIN"]

  # Set order for data for correct rolling sums
  setorderv(data_output$combined_data_readthrough, c("ASIN", "Marketplace", "Date"))

  # Compute rolling sums for readthrough
  data_output$combined_data_readthrough[, ":=" (
    order_rollsum = frollsum(
      orders,
      n = input$rolling_sum_days,
      algo = "exact",
      align = "right"
    ),
    ku_rollsum = frollsum(
      kenp,
      n = input$rolling_sum_days,
      algo = "exact",
      align = "right"
    ) / kenp_length
  ),
  keyby = c("ASIN", "Marketplace")]

  # Set new order for book and series readthrough calcs
  setorderv(data_output$combined_data_readthrough, c("Marketplace", "Date", "series", "book"))

  # Calculate the readthrough for each book using leads
  data_output$combined_data_readthrough[, ":=" (prior_book_order_rollsum = shift(order_rollsum, n = 1, type = "lag"),
                                                prior_book_ku_rollsum = shift(ku_rollsum, n = 1, type = "lag")),
                                        keyby = c("Marketplace", "Date", "series")
  # Calculate readthrough
  ][, ":=" (sales_readthrough = order_rollsum / prior_book_order_rollsum,
            ku_readthrough = ku_rollsum / prior_book_ku_rollsum)]
  
  # Calculate rolling sums for AMS ads
  setorderv(data_output$raw_ams_data, c("ASIN", "Date"))
  
  # Compute the rolling sums
  data_output$combined_data_readthrough <-
    # Ensure all dates present
    data.table(expand.grid(
      Date = seq(as.Date(min(c(data_output$daily_ams_data$Date))),
                 as.Date(max(c(data_output$daily_ams_data$Date))),
                 by = "days"
      ),
      ASIN = unique(data_output$daily_ams_data$ASIN),
      Marketplace = unique(data_output$daily_ams_data$Marketplace)
    )) %>%
    .[data_output$daily_ams_data, on = c("Date", "ASIN", "Marketplace")] %>%
    replace(is.na(.), 0) %>%
    as.data.table %>%
    .[, ":=" (AMS_orders_rollingsum = frollsum(AMS_orders,
                                               n = input$rolling_sum_days,
                                               algo = "exact",
                                               align = "right"),
              AMS_kenp_rollingsum = frollsum(AMS_kenp,
                                             n = input$rolling_sum_days,
                                             algo = "exact",
                                             align = "right"),
              AMS_clicks_rollingsum = frollsum(AMS_clicks,
                                               n = input$rolling_sum_days,
                                               algo = "exact",
                                               align = "right"),
              AMS_Ads_rollingsum = frollsum(AMS_Ads,
                                            n = input$rolling_sum_days,
                                            algo = "exact",
                                            align = "right")),
      keyby = c("ASIN", "Marketplace")] %>%
    # Right join onto readthrough data
    merge(data_output$combined_data_readthrough,
          by = c("Date", "ASIN", "Marketplace"),
          all.y = TRUE)
  
  # Calculate the ROI, starting with the last books of a series
  # USED OVERALL KENP ROYALTY AS ONE FROM DATA LOOKS INCORRECT FOR SOME DATES
  
  data_output$combined_data_readthrough[, c("ku_return_lead", "sales_return_lead") := 0]
  
  setorderv(data_output$combined_data_readthrough, c("Marketplace", "Date", "series", "book"))
  
  # Calculate book 1 reads for overall readthrough and the earnings for each book in turn
  for (book_no in c(max(data_output$combined_data_readthrough$book):2)) {
    
    # Shift the book 1 info onto the book
    data_output$combined_data_readthrough[, ":=" (
      book1_order_rollsum = shift(order_rollsum, n = (book_no - 1), type = "lag"),
      book1_ku_rollsum = shift(ku_rollsum, n = (book_no - 1), type = "lag")
    ),
    keyby = c("Marketplace", "Date", "series")]
    
    # Calculate the profit of this book from a book 1 sale
    data_output$combined_data_readthrough[book == book_no, ":="
                                          (
                                            sales_return = (order_rollsum / book1_order_rollsum * sale_royalty) + sales_return_lead,
                                            ku_return = (
                                              ku_rollsum / book1_ku_rollsum * input$kenp_royalty_per_page_read * kenp_length
                                            ) + ku_return_lead
                                          )]
    
    # Shift the data onto the prior book in the series
    data_output$combined_data_readthrough[, ":=" (
      ku_return_lead = shift(ku_return, n = 1, type = "lead"),
      sales_return_lead = shift(sales_return, n = 1, type = "lead")
    ),
    keyby = c("Marketplace", "Date", "series")]
  }
  
  data_output$combined_data_readthrough[, c("book1_order_rollsum", "book1_ku_rollsum", "sales_return", "ku_return") := NULL]

  # Make final calculation of expected earnings for book 1 advertising
  data_output$combined_data_readthrough[book == 1,
                                        AMS_expected_earnings_per_click := 
                                          (AMS_orders_rollingsum / AMS_clicks_rollingsum) * (sale_royalty + sales_return_lead) +
                                          ((AMS_kenp_rollingsum / kenp_length) / AMS_clicks_rollingsum) * (kenp_length * input$kenp_royalty_per_page_read + ku_return_lead)]
  
  for (book in c(max(series_info$book)):2) {
      data_output$combined_data_readthrough[book == book,
                                             ":=" (ku_return = ku_readthrough * (input$kenp_royalty_per_page_read * kenp_length + ku_return_lead),
                                                   sales_return = sales_readthrough * (sale_royalty + sales_return_lead))
      ][, ":=" (ku_return = fifelse(is.na(ku_return), 0, ku_return),
                sales_return = fifelse(is.na(sales_return), 0, sales_return))]
      
      # Combine the info with the prior book in the series
      data_output$combined_data_readthrough[, ":=" (ku_return_lead = shift(ku_return, n = 1, type = "lead"),
                                                     sales_return_lead = shift(sales_return, n = 1, type = "lead"))]
  }
  
  data_output$combined_data_readthrough[book == 1,
                                         ":=" (ku_return = ku_readthrough * (input$kenp_royalty_per_page_read * AMS_kenp + ku_return_lead),
                                               sales_return = sales_readthrough * (sale_royalty * AMS_orders + sales_return_lead))
  ][, ":=" (ku_return = fifelse(is.na(ku_return), 0, ku_return),
            sales_return = fifelse(is.na(sales_return), 0, sales_return))
    ][ ":=" ()]
  
  ## OLD CODE AFTER HERE. NEEDS CLEANING UP
  
  
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
  
  browser()
})

# Filter the read-through data based on selections
observe({
  
  req(data_output$wide_combined, input$historic_days_readthrough, input$rolling_sum_days, input$readthrough_filter)

  data_output$readthrough_filtered <-
    data_output$wide_combined[(Date >= max(Date) - input$historic_days_readthrough) &
                                (Marketplace == input$readthrough_filter),]
  
})

# Get sales readthrough rates
output$chart_sales_readthrough_all <- renderPlotly({
  
  req(data_output$combined_data_readthrough, input$historic_days_readthrough, input$readthrough_filter)
  
  dt <- data_output$combined_data_readthrough[Marketplace == input$readthrough_filter & 
                                              (Date >= max(Date) - input$historic_days_readthrough),]
  plt <- plot_ly(data = dt,
          x = ~ Date,
          y = ~ sales_readthrough,
          color = ~ name,
          type = 'scatter',
          mode = 'lines') %>%
    layout(yaxis = list(title = "Sales Readthrough",
                        range = c(0, max(dt$sales_readthrough))),
           title = "Sales Readthrough")
  
  rm(dt)
  
  return(plt)
})

# Get ku readthrough rates
output$chart_ku_readthrough_all <- renderPlotly({
  
  req(data_output$combined_data_readthrough, input$historic_days_readthrough, input$readthrough_filter)
  
  dt <- data_output$combined_data_readthrough[Marketplace == input$readthrough_filter & 
                                                (Date >= max(Date) - input$historic_days_readthrough),]
  plt <- plot_ly(data = dt,
                 x = ~ Date,
                 y = ~ ku_readthrough,
                 color = ~ name,
                 type = 'scatter',
                 mode = 'lines') %>%
    layout(yaxis = list(title = "KU Readthrough",
                        range = c(0, max(dt$sales_readthrough))),
           title = "KU Readthrough")
  
  rm(dt)
  
  return(plt)
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

