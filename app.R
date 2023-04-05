## This script will read in data from the KDP dashboard and then analyse
# smoothed trends in sales and estimated royalties over time

library(readxl)
library(data.table)
library(dplyr)
library(quantmod)
library(ggplot2)
library(shiny)

source("C:/Users/Andy/Documents/Kindle_trends/app_functions.R")

ui <- navbarPage(
  "KDP royalty analyser",
  tabPanel("Data",
           textInput("data_path", label = "Path to read data files from", value = "F:/Writing - Book/Sales Data/KDP"),
           actionButton("load", "Load KDP Data"),
           br(),
           h1("Outputs"),
           h2("Output Settings"),
           numericInput("historic_days", "Days of history to view", value = 120),
           numericInput("ma_days", "Days to take moving average across", value = 14),
           numericInput("kenp_royalty_per_page_read", "USD royalty per KENP read", value = 0.004561577),
           h2("Charts"),
           fluidRow(
             column(6, plotOutput("chart_all_books_all_countries")),
             column(6, plotOutput("chart_oblivion_all_countries"))
           ),
           fluidRow(
             column(6, plotOutput("chart_all_books_USA")),
             column(6, plotOutput("chart_oblivion_USA"))
           ),
           fluidRow(
             column(6, plotOutput("chart_all_books_UK")),
             column(6, plotOutput("chart_oblivion_UK"))
           ),
           fluidRow(
             column(6, plotOutput("chart_all_books_ROW")),
             column(6, plotOutput("chart_oblivion_ROW"))
           ),
  )
)

server <- function(input, output) {
  
  data_output <- reactiveValues()
  
  # When the load data button is pressed, read in the KDP data excel files
  observeEvent(input$load, {
    
    data_output$raw_data <- load_kdp_files(input$data_path)
    
    showNotification("Processing data...", id= "loading", duration = NULL)
    
    data_output$combined_data <- process_data_for_royalties(data_output$raw_data,
                                                            input$kenp_royalty_per_page_read)
    removeNotification("loading")
    
  })
  
  # Filter the data ready for the charts
  observe({
    # Check whether the royalty data exists
    if(input$historic_days > 0 & !is.null(data_output$combined_data)) {
      data_output$filtered_data <-
        data_output$combined_data[Date >= (max(Date) - input$historic_days),]
    }
  })
  
  # Create the charts for different books
  output$chart_all_books_all_countries <- renderPlot({
    if(input$historic_days > 0 & !is.null(data_output$filtered_data)) {
      data_output$filtered_data %>%
        moving_average_royalty_chart(input$ma_days) +
        ggtitle("All books, All countries")
    }
  })
  
  output$chart_oblivion_all_countries <- renderPlot({
    if(input$historic_days > 0 & !is.null(data_output$filtered_data)) {
      data_output$filtered_data[ASIN == 'B087676DTB',] %>%
        moving_average_royalty_chart(input$ma_days) +
        ggtitle("Oblivion, All countries")
    }
  })
  
  output$chart_all_books_USA <- renderPlot({
    if(input$historic_days > 0 & !is.null(data_output$filtered_data)) {
      data_output$filtered_data[Marketplace == 'Amazon.com',] %>%
        moving_average_royalty_chart(input$ma_days) +
        ggtitle("All books, USA")
    }
  })
  
  output$chart_oblivion_USA <- renderPlot({
    if(input$historic_days > 0 & !is.null(data_output$filtered_data)) {
      data_output$filtered_data[Marketplace == 'Amazon.com' & ASIN == 'B087676DTB',] %>%
        moving_average_royalty_chart(input$ma_days) +
        ggtitle("Oblivion, USA")
    }
  })
  
  output$chart_all_books_UK <- renderPlot({
    if(input$historic_days > 0 & !is.null(data_output$filtered_data)) {
      data_output$filtered_data[Marketplace == 'Amazon.co.uk',] %>%
        moving_average_royalty_chart(input$ma_days) +
        ggtitle("All books, UK")
    }
  })
  
  output$chart_oblivion_UK <- renderPlot({
    if(input$historic_days > 0 & !is.null(data_output$filtered_data)) {
      data_output$filtered_data[Marketplace == 'Amazon.co.uk' & ASIN == 'B087676DTB',] %>%
        moving_average_royalty_chart(input$ma_days) +
        ggtitle("Oblivion, UK")
    }
  })
  
  output$chart_all_books_ROW <- renderPlot({
    if(input$historic_days > 0 & !is.null(data_output$filtered_data)) {
      data_output$filtered_data[!(Marketplace %in% c('Amazon.com', 'Amazon.co.uk')),] %>%
        moving_average_royalty_chart(input$ma_days) +
        ggtitle("All books, Rest of world")
    }
  })
  
  output$chart_oblivion_ROW <- renderPlot({
    if(input$historic_days > 0 & !is.null(data_output$filtered_data)) {
      data_output$filtered_data[!(Marketplace %in% c('Amazon.com', 'Amazon.co.uk')) & ASIN == 'B087676DTB',] %>%
        moving_average_royalty_chart(input$ma_days) +
        ggtitle("Oblivion, Rest of world")
    }
  })
  
}

shinyApp(ui = ui, server = server)


