## This script will read in data from the KDP dashboard and then analyse
# smoothed trends in sales and estimated royalties over time

source("global.R")

ui <- navbarPage(
  "KDP royalty analyser",
  tabPanel("Data",
           textInput("data_path", label = "Path to read KDP sales files from", value = "F:/Writing - Book/Data/KDP - orders"),
           textInput("payment_data_path", label = "Path to read KDP payment files from", value = "F:/Writing - Book/Data/KDP - payments"),
           textInput("bank_data_path", label = "Path to read data files from", value = "F:/Writing - Book/Data/Bank"),
           textInput("ams_data_path", label = "Path to read data files from", value = "F:/Writing - Book/Data/AMS"),
           textInput("facebook_data_path", label = "Path to read data files from", value = "F:/Writing - Book/Data/Facebook"),
           numericInput("kenp_royalty_per_page_read", "USD royalty per KENP read", value = 0.0041),
           actionButton("load", "Load & Process Data"),
           br()
           ),
  tabPanel("Royalties",
           fluidRow(
             column(3, uiOutput("series_dropdown_menu")),
             column(3, sliderInput("historic_months", "Months of history to view", min = 1, max = 50, value = 4)),
             column(3, sliderInput("ma_days", "Days to take moving average across", min = 1, max = 28, value = 7))
           ),
           h2("Charts"),
           fluidRow(
             column(6, plotlyOutput("chart_all_books_all_countries")),
             column(6, plotlyOutput("chart_oblivion_all_countries"))
           ),
           fluidRow(br(), br()),
           fluidRow(
             column(6, plotlyOutput("chart_all_books_USA")),
             column(6, plotlyOutput("chart_oblivion_USA"))
           ),
           fluidRow(br(), br()),
           fluidRow(
             column(6, plotlyOutput("chart_all_books_UK")),
             column(6, plotlyOutput("chart_oblivion_UK"))
           ),
           fluidRow(br(), br()),
           fluidRow(
             column(6, plotlyOutput("chart_all_books_Aus")),
             column(6, plotlyOutput("chart_oblivion_Aus"))
           ),
           fluidRow(br(), br()),
           fluidRow(
             column(6, plotlyOutput("chart_all_books_Can")),
             column(6, plotlyOutput("chart_oblivion_Can"))
           ),
           fluidRow(br(), br()),
           fluidRow(
             column(6, plotlyOutput("chart_all_books_ROW")),
             column(6, plotlyOutput("chart_oblivion_ROW"))
           ),
  ),
  tabPanel("KU Payouts",
           fluidRow(
             column(3, radioButtons(
               "ku_payout_marketplace",
               "Marketplace",
               choices = c(
                 "Amazon.com",
                 "Amazon.co.uk",
                 "Amazon.com.au",
                 "Amazon.ca"
               ),
               selected = "Amazon.com"
             )),
             column(3, sliderInput("historic_months_ku", "Months of history to view", min = 1, max = 50, value = 20))
           ),
           h2("Chart"),
           fluidRow(
             plotlyOutput("ku_payout_chart")
           )
  ),
  tabPanel("Read-through",
           fluidRow(
             column(4, numericInput("rolling_sum_days", "Prior X days for read-through", value = 90)),
             column(4, numericInput("rolling_sum_days_conversion", "Prior X days for conversion rates", value = 21)),
             column(4, numericInput("historic_days_readthrough", "History to view", value = 120))
             ),
           tabsetPanel(tabPanel("Table",
                    h2("Summary"),
                    tableOutput("table_readthrough")),
           tabPanel(
             "Read-through",
             radioButtons(
               "readthrough_filter",
               "Read-through marketplace",
               choices = c(
                 "All",
                 "Amazon.com",
                 "Amazon.co.uk",
                 "Amazon.com.au",
                 "Amazon.ca"
               ),
               selected = "All"
             ),
             br(),
             fluidRow(br(),
                      column(
               6, plotlyOutput("chart_sales_readthrough_all")
             ),
             column(
               6, plotlyOutput("chart_ku_readthrough_all")
             ))
           ),
           tabPanel("AMS",
                    br(),
                    fluidRow(column(3, uiOutput("AMS_ASIN_filter_menu"))),
                    fluidRow(column(
                      6, plotlyOutput("chart_AMS_USA")
                    ),
                    column(
                      6, plotlyOutput("chart_AMS_USA_underlying")
                    )),
                    fluidRow())
  )),
  tabPanel("Test Changes",
           fluidRow(
             column(4, dateInput("changes_start_date", "Start date", value = "2022-04-06")),
             column(4, dateInput("changes_change_date", "Date of change", value = "2023-07-18")),
             column(4, dateInput("changes_end_date", "End date", value = Sys.Date()))
           ),
           h2("Readthrough Before"),
           fluidRow(
             column(12, DTOutput("readthrough_before_change"))
           ),
           h2("Readthrough After"),
           fluidRow(
             column(12, DTOutput("readthrough_after_change"))
           ),
           h2("Royalties Before"),
           fluidRow(
             column(12, DTOutput("royalties_before_change"))
           ),
           h2("Royalties After"),
           fluidRow(
             column(12, DTOutput("royalties_after_change"))
           )
  ),
  tabPanel("Net Income",
           fluidRow(
           ),
           h2("Total"),
           fluidRow(
             column(12, tableOutput("net_income_total"))
           ),
           h2("USA"),
           fluidRow(
             column(12, tableOutput("net_income_usa"))
           ),
           h2("UK"),
           fluidRow(
             column(12, tableOutput("net_income_uk"))
           ),
           h2("Aus"),
           fluidRow(
             column(12, tableOutput("net_income_aus"))
           )
  ),
  tabPanel("Cash Accounting",
           fluidRow(
             column(6, dateInput("cash_accounting_start", "Start date", value = "2023-04-06")),
             column(6, dateInput("cash_accounting_end", "End date", value = Sys.Date()))
           ),
           fluidRow(
           ),
           h2("Total"),
           fluidRow(
             column(12, tableOutput("net_cash_income_total"))
           ),
           h2("Monthly"),
           fluidRow(
             column(12, tableOutput("net_cash_income"))
           ),
           h2("Itemised"),
           fluidRow(
             column(6, uiOutput("bank_data_categories"))
           ),
           fluidRow(
             column(12, tableOutput("net_cash_all"))
           )
  )
)

server <- function(input, output) {
  
  data_output <- reactiveValues()
  
  for (file_name in list.files("server_files/", pattern = ".R")) {
    source(file.path("server_files", file_name), local = TRUE)
  }
  
}


shinyApp(ui = ui, server = server)
