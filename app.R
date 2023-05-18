## This script will read in data from the KDP dashboard and then analyse
# smoothed trends in sales and estimated royalties over time

source("global.R")

ui <- navbarPage(
  "KDP royalty analyser",
  tabPanel("Data",
           textInput("data_path", label = "Path to read data files from", value = "F:/Writing - Book/Sales Data/KDP"),
           actionButton("load", "Load KDP Data"),
           br()
           ),
  tabPanel("Royalties",
           fluidRow(
             column(4, numericInput("historic_days", "Days of history to view", value = 120)),
             column(4, numericInput("ma_days", "Days to take moving average across", value = 14)),
             column(4, numericInput("kenp_royalty_per_page_read", "USD royalty per KENP read", value = 0.004561577))
           ),
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
  ),
  tabPanel("Read-through",
           fluidRow(
             column(6, numericInput("rolling_sum_days", "Prior X days for read-through", value = 90)),
             column(6, numericInput("historic_days_readthrough", "History to view", value = 120))
             ),
           h2("Summary"),
           tableOutput("table_readthrough"),
           br(),
           radioButtons("readthrough_filter", "Read-through marketplace", 
                        choices = c("All", "Amazon.com", "Amazon.co.uk", "Amazon.com.au", "Amazon.ca"), 
                        selected = "All"),
           br(),
           h2("Viridian Legion"),
           fluidRow(
             column(6, plotOutput("chart_VL_sales_readthrough_all")),
             column(6, plotOutput("chart_VL_ku_readthrough_all"))
           ),
           h2("Fate Of The Slayer"),
           fluidRow(
             column(6, plotOutput("chart_fate_sales_readthrough_all")),
             column(6, plotOutput("chart_fate_ku_readthrough_all"))
           ),
           h2("Slayer's Revenge"),
           fluidRow(
             column(6, plotOutput("chart_revenge_sales_readthrough_all")),
             column(6, plotOutput("chart_revenge_ku_readthrough_all"))
           )
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
  
  # Filter the read-through data based on selections
  observe({
    if(!is.null(data_output$combined_data) &
       input$historic_days_readthrough > 0 &
       input$rolling_sum_days > 0 &
       !is.na(input$readthrough_filter)){

      data_output$readthrough_filtered <-
        data_output$wide_combined[(Date >= max(Date) - input$historic_days_readthrough) &
                                    (Marketplace == input$readthrough_filter),]
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


  output$chart_VL_sales_readthrough_all <- renderPlot({
    if(input$historic_days_readthrough > 0 & !is.null(data_output$combined_data)) {
      data_output$readthrough_filtered %>%
        ggplot(aes(x = Date, y = sales_readthrough_B08766L2BZ)) +
        geom_line() +
        ylim(0, max(data_output$wide_all_markets$sales_readthrough_B08766L2BZ)) +
        ggtitle("Sales read-through")
    }
  })

  output$chart_VL_ku_readthrough_all <- renderPlot({
    if(input$historic_days_readthrough > 0 & !is.null(data_output$combined_data)) {
      data_output$readthrough_filtered %>%
        ggplot(aes(x = Date, y = ku_readthrough_B08766L2BZ)) +
        geom_line() +
        ylim(0, max(data_output$wide_all_markets$ku_readthrough_B08766L2BZ)) +
        ggtitle("KU read-through")
    }
  })

  output$chart_fate_sales_readthrough_all <- renderPlot({
    if(input$historic_days_readthrough > 0 & !is.null(data_output$combined_data)) {
      data_output$readthrough_filtered %>%
        ggplot(aes(x = Date, y = sales_readthrough_B09GPMRTF7)) +
        geom_line() +
        ylim(0, max(data_output$wide_all_markets$sales_readthrough_B09GPMRTF7)) +
        ggtitle("Sales read-through")
    }
  })

  output$chart_fate_ku_readthrough_all <- renderPlot({
    if(input$historic_days_readthrough > 0 & !is.null(data_output$combined_data)) {
      data_output$readthrough_filtered %>%
        ggplot(aes(x = Date, y = ku_readthrough_B09GPMRTF7)) +
        geom_line() +
        ylim(0, max(data_output$wide_all_markets$ku_readthrough_B09GPMRTF7)) +
        ggtitle("KU read-through")
    }
  })

  output$chart_revenge_sales_readthrough_all <- renderPlot({
    if(input$historic_days_readthrough > 0 & !is.null(data_output$combined_data)) {
      data_output$readthrough_filtered %>%
        ggplot(aes(x = Date, y = sales_readthrough_B0BHR5YXXV)) +
        geom_line() +
        ylim(0, max(data_output$wide_all_markets$sales_readthrough_B0BHR5YXXV)) +
        ggtitle("Sales read-through")
    }
  })

  output$chart_revenge_ku_readthrough_all <- renderPlot({
    if(input$historic_days_readthrough > 0 & !is.null(data_output$combined_data)) {
      data_output$readthrough_filtered %>%
        ggplot(aes(x = Date, y = ku_readthrough_B0BHR5YXXV)) +
        geom_line() +
        ylim(0, max(data_output$wide_all_markets$ku_readthrough_B0BHR5YXXV)) +
        ggtitle("KU read-through")
    }
  })
  
}


shinyApp(ui = ui, server = server)
