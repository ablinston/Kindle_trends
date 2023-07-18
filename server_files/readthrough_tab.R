
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

