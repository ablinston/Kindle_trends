

# Get data ready for charts
observe({
  # Check whether the royalty data exists
  req(data_output$combined_data, input$rolling_sum_days, input$rolling_sum_days_conversion, 
      data_output$daily_ams_data, data_output$currency_lookup)

  # WROTE THIS CODE TO EVENTUALLY ALLOW MULTIPLE SERIES AND REMOVE LOOPS. IT WORKS
  dt <-
    rbindlist(list(data_output$combined_data[, .(Date, ASIN, Marketplace, orders, kenp)],
                   data_output$combined_data[, .(orders = sum(orders, na.rm = TRUE),
                                                 kenp = sum(kenp, na.rm = TRUE)),
                                             by = c("Date", "ASIN")][, Marketplace := "All"]),
              use.names = TRUE) %>%
    # Merge on series info
    .[series_info, on = "ASIN"]

  # Set order for data for correct rolling sums
  setorderv(dt, c("ASIN", "Marketplace", "Date"))

  # Compute rolling sums for readthrough
  dt[, ":=" (
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
  setorderv(dt, c("Marketplace", "Date", "series", "book"))

  # Calculate the readthrough for each book using leads
  dt[, ":=" (prior_book_order_rollsum = shift(order_rollsum, n = 1, type = "lag"),
             prior_book_ku_rollsum = shift(ku_rollsum, n = 1, type = "lag")),
     keyby = c("Marketplace", "Date", "series")
  # Calculate readthrough
  ][, ":=" (sales_readthrough = fifelse(prior_book_order_rollsum == 0, NA, order_rollsum / prior_book_order_rollsum),
            ku_readthrough = fifelse(prior_book_ku_rollsum == 0, NA, ku_rollsum / prior_book_ku_rollsum))]

  # Calculate rolling sums for AMS ads
  setorderv(data_output$raw_ams_data, c("ASIN", "Date"))

  # Compute the rolling sums for AMS conversion rates
  data_output$combined_data_readthrough <-
    merge(dt,
          data_output$daily_ams_data,
          by = c("Date", "ASIN", "Marketplace"),
          all.x = TRUE) %>%
    .[, ":=" (AMS_orders_rollingsum = frollsum(AMS_orders,
                                               n = input$rolling_sum_days_conversion,
                                               algo = "exact",
                                               align = "right"),
              AMS_kenp_rollingsum = frollsum(AMS_kenp,
                                             n = input$rolling_sum_days_conversion,
                                             algo = "exact",
                                             align = "right"),
              AMS_clicks_rollingsum = frollsum(AMS_clicks,
                                               n = input$rolling_sum_days_conversion,
                                               algo = "exact",
                                               align = "right"),
              AMS_Ads_Native_Curr_rollingsum = frollsum(AMS_Ads_Native_Curr,
                                            n = input$rolling_sum_days_conversion,
                                            algo = "exact",
                                            align = "right")),
      keyby = c("ASIN", "Marketplace")]

  rm(dt)
  
  # Calculate the ROI, starting with the last books of a series
  # USED OVERALL KENP ROYALTY AS ONE FROM DATA LOOKS INCORRECT FOR SOME DATES

  data_output$combined_data_readthrough[, c("ku_return_lead", "sales_return_lead") := 0]

  setorderv(data_output$combined_data_readthrough, c("Marketplace", "Date", "series", "book"))

  # Calculate book 1 reads for overall readthrough and the earnings for each book in turn
  for (book_no in c(max(data_output$combined_data_readthrough$book):2)) {

    # Calculate the profit of this book from a book 1 sale
    data_output$combined_data_readthrough[book == book_no, ":="
                                          (
                                            sales_return = fifelse(sales_readthrough > 1, 1, sales_readthrough) * (sale_royalty + sales_return_lead),
                                            ku_return = fifelse(ku_readthrough > 1, 1, ku_readthrough) * (input$kenp_royalty_per_page_read * kenp_length + ku_return_lead)
                                          )]

    # Shift the data onto the prior book in the series
    data_output$combined_data_readthrough[, ":=" (
      ku_return_lead = shift(ku_return, n = 1, type = "lead"),
      sales_return_lead = shift(sales_return, n = 1, type = "lead")
    ),
    keyby = c("Marketplace", "Date", "series")]
  }

  data_output$combined_data_readthrough[, c("sales_return", "ku_return") := NULL]

  # Make final calculation of expected earnings for book 1 advertising
  data_output$combined_data_readthrough[book == 1, ":=" (
    AMS_sales_conversion_rate = fifelse(AMS_clicks_rollingsum == 0, NA, AMS_orders_rollingsum / AMS_clicks_rollingsum),
    AMS_ku_conversion_rate = fifelse(kenp_length == 0 | AMS_clicks_rollingsum == 0, NA, (AMS_kenp_rollingsum / kenp_length) / AMS_clicks_rollingsum),
    AMS_conversion_rate = fifelse(AMS_clicks_rollingsum == 0, NA, (AMS_orders_rollingsum / AMS_clicks_rollingsum) +
      ((AMS_kenp_rollingsum / kenp_length) / AMS_clicks_rollingsum)),
    sales_profit_per_conversion = sale_royalty + sales_return_lead,
    ku_profit_per_conversion = (kenp_length * input$kenp_royalty_per_page_read + ku_return_lead)
  )][book == 1, ":=" (
    AMS_expected_earnings_per_click = fifelse(AMS_clicks_rollingsum == 0, NA, 
      (AMS_orders_rollingsum / AMS_clicks_rollingsum) * (sale_royalty + sales_return_lead) +
      ((AMS_kenp_rollingsum / kenp_length) / AMS_clicks_rollingsum) * (kenp_length * input$kenp_royalty_per_page_read + ku_return_lead)),
    AMS_actual_CPC = fifelse(AMS_clicks_rollingsum == 0, NA, -AMS_Ads_Native_Curr_rollingsum / AMS_clicks_rollingsum))]

})

output$AMS_ASIN_filter_menu <- renderUI({
  
  req(data_output$combined_data_readthrough)

  selectInput("AMS_ASIN_filter",
              label = "Select ASIN",
              choices = 
                data_output$combined_data_readthrough %>%
                filter(!is.na(AMS_actual_CPC)) %>%
                pull(ASIN) %>%
                unique)
  
})




# Get readthrough rates
observe({
  
  req(data_output$combined_data_readthrough, input$readthrough_first_date, input$readthrough_last_date, input$readthrough_filter)
  
  dt <- data_output$combined_data_readthrough[Marketplace == input$readthrough_filter & 
                                                (Date <= input$readthrough_last_date) &                                                 
                                                (Date >= input$readthrough_first_date),]
  
  output$chart_ku_readthrough_all <- renderPlotly({
    plt <- plot_ly(data = dt,
                   x = ~ Date,
                   y = ~ ku_readthrough,
                   color = ~ name,
                   type = 'scatter',
                   mode = 'lines') %>%
      layout(yaxis = list(title = "KU Readthrough",
                          range = c(0, max(dt$sales_readthrough))),
             title = "KU Readthrough") 
  })
  
  # Get KU readthrough
  output$chart_ku_readthrough_vl <- renderPlotly({
    
    intervals <- binomial_confidence_interval(dt[ASIN == "B08766L2BZ",]$ku_readthrough, dt[ASIN == "B08766L2BZ",]$prior_book_ku_rollsum)
    
    plot_ly(data = dt[ASIN == "B08766L2BZ"],
            x = ~ Date) %>%
      add_trace(y = ~ku_readthrough,
                type = 'scatter',
                mode = 'lines',
                name = "VL KU Readthrough") %>%
      add_ribbons(ymin = ~intervals$Lower,
                  ymax = ~intervals$Upper,
                  name = "VL 95% confidence interval",
                  color = I("blue"),
                  opacity = 0.3)
    
  })
  
  # Get sales readthrough rates
  output$chart_sales_readthrough_all <- renderPlotly({
    
    plt <- plot_ly(data = dt,
                   x = ~ Date,
                   y = ~ sales_readthrough,
                   color = ~ name,
                   type = 'scatter',
                   mode = 'lines') %>%
      layout(yaxis = list(title = "Sales Readthrough",
                          range = c(0, max(dt$sales_readthrough))),
             title = "Sales Readthrough")
  })
  
  # Get Viridian Legion Sales readthrough
  output$chart_sales_readthrough_vl <- renderPlotly({
    
    intervals <- binomial_confidence_interval(dt[ASIN == "B08766L2BZ",]$sales_readthrough, dt[ASIN == "B08766L2BZ",]$prior_book_order_rollsum)
    
    plot_ly(data = dt[ASIN == "B08766L2BZ"],
            x = ~ Date) %>%
      add_trace(y = ~sales_readthrough,
                type = 'scatter',
                mode = 'lines',
                name = "VL Sales Readthrough") %>%
      add_ribbons(ymin = ~intervals$Lower,
                  ymax = ~intervals$Upper,
                  name = "VL 95% confidence interval",
                  color = I("blue"),
                  opacity = 0.3)
    
  })

})

# Get AMS US Ad performance chart
observe({

  req(data_output$combined_data_readthrough, input$readthrough_first_date, input$readthrough_last_date, input$AMS_ASIN_filter)

  data_output$dt <- 
    data_output$combined_data_readthrough[Marketplace == "Amazon.com" &
                                            ASIN == input$AMS_ASIN_filter &
                                            book == 1 &
                                            (Date <= input$readthrough_last_date) &                                                 
                                            (Date >= input$readthrough_first_date),] %>%
    as.data.frame()

  output$chart_AMS_USA <- renderPlotly({
   
    req(data_output$combined_data_readthrough, data_output$dt)

    plot_ly(data = data_output$dt,
                   x = ~ Date) %>%
      add_trace(y = ~AMS_expected_earnings_per_click,
                type = 'scatter',
                mode = 'lines',
                name = "AMS profit per click") %>%
      add_trace(y = ~AMS_actual_CPC,
                type = 'scatter',
                mode = 'lines',
                name = "AMS cost per click") %>%
      layout(yaxis = list(title = "$",
                          range = c(0, max(data_output$dt$AMS_actual_CPC))),
             title = "AMS USA performance (rolling averages)")
    
  })
  
  output$chart_AMS_USA_underlying <- renderPlotly({
    
    req(data_output$combined_data_readthrough, data_output$dt)

    intervals <- binomial_confidence_interval(
      data_output$dt$AMS_conversion_rate, 
      data_output$dt$AMS_clicks_rollingsum)
      
    plot_ly(data = data_output$dt,
                   x = ~ Date) %>%
      add_trace(y = ~AMS_conversion_rate,
                type = 'scatter',
                mode = 'lines',
                name = "AMS conversion rate",
                yaxis = "y1",
                color = I("blue")) %>%
      add_ribbons(ymin = ~intervals$Lower,
                  ymax = ~intervals$Upper,
                  name = "Conversion rate approx 95% confidence interval",
                  color = I("blue"),
                  opacity = 0.3) %>%
      add_trace(y = ~sales_profit_per_conversion,
                type = 'scatter',
                mode = 'lines',
                name = "Profit per sale conversion",
                yaxis = "y2") %>%
      add_trace(y = ~ku_profit_per_conversion,
                type = 'scatter',
                mode = 'lines',
                name = "Profit per KU conversion",
                yaxis = "y2") %>%
      # add_trace(y = ~AMS_sales_conversion_rate,
      #           type = 'scatter',
      #           mode = 'lines',
      #           name = "AMS sales conversion rate",
      #           yaxis = "y1") %>%
      # add_trace(y = ~AMS_ku_conversion_rate,
      #           type = 'scatter',
      #           mode = 'lines',
      #           name = "AMS KU conversion rate",
      #           yaxis = "y1") %>%
      layout(yaxis = list(title = "Rate",
                          range = c(0, 2 * max(data_output$dt$AMS_conversion_rate)),
                          side = "left"),
             yaxis2 = list(
               title = "$",
               range = c(0, max(data_output$dt[,c("ku_profit_per_conversion", "sales_profit_per_conversion")])),
               side = "right",
               overlaying = "y"  # Align with the first y-axis
             ),
             title = "AMS USA underlying performance (rolling averages)")
    
  })

})

output$table_readthrough <- renderTable({
  
  req(data_output$combined_data_readthrough)
  
  dt <- data_output$combined_data_readthrough[Date == max(data_output$combined_data_readthrough$Date) &
                                                Marketplace %in% c("All", "Amazon.com", "Amazon.co.uk", "Amazon.com.au", "Amazon.ca") &
                                                !is.na(sales_readthrough),
                                              ][, ":=" (`Sales Readthrough` = round(100 * sales_readthrough, 1),
                                                        `Sales Sample Size` = round(prior_book_order_rollsum, 0),
                                                        `KU Readthrough` = round(100 * ku_readthrough, 1),
                                                        `KU Sample Size` = round(prior_book_ku_rollsum, 1))]
  
  return(dt[, .(Marketplace, name, `Sales Readthrough`, `Sales Sample Size`, `KU Readthrough`, `KU Sample Size`)])
  
})

