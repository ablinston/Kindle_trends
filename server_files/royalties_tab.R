
# Filter the data ready for the charts
observe({
  # Check whether the royalty data exists
  req(input$historic_months, data_output$combined_data, input$series_dropdown)

  # See if we want all series or a selection
  if (input$series_dropdown == "All") {
    data_output$filtered_data <-
      data_output$combined_data[Date >= (max(Date) - (input$historic_months) * 30),]
  } else {
    data_output$filtered_data <-
      data_output$combined_data[Date >= (max(Date) - (input$historic_months) * 30) &
                                  series == input$series_dropdown,]
  }
  

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


