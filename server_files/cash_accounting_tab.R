

# Filter the bank data for the cash accounting tab
observe({
  if (!is.null(input$cash_accounting_start) & 
      !is.null(input$cash_accounting_end) &
      !is.null(data_output$bank_data) &
      !is.null(input$cash_accounting_categories)) {
    
    data_output$bank_data_filtered <- data_output$bank_data[Date >= input$cash_accounting_start &
                                                              Date <= input$cash_accounting_end,][order(Date),]
    # Create cash accounting numbers from bank data
    output$net_cash_income <- renderTable(
      data_output$bank_data_filtered[, .(
        Gross_Income = sum(Amount[Category == "Income"]),
        Facebook_Ads = sum(Amount[Category == "Facebook Ads"]),
        AMS_Ads = sum(Amount[Category == "AMS Ads"]),
        Other_Ad_Costs = sum(Amount[Category == "Other Ad Costs"]),
        Other_Expenses = sum(Amount[Category == "Other expenses"]),
        Net_income = sum(Amount)
      ),
      keyby = c("Year", "Month")] %>%
        format_output_table(), 
      align = "r"
    )
    output$net_cash_income_total <- renderTable(
      data_output$bank_data_filtered[, .(
        Gross_Income = sum(Amount[Category == "Income"]),
        Facebook_Ads = sum(Amount[Category == "Facebook Ads"]),
        AMS_Ads = sum(Amount[Category == "AMS Ads"]),
        Other_Ad_Costs = sum(Amount[Category == "Other Ad Costs"]),
        Other_Expenses = sum(Amount[Category == "Other expenses"]),
        Net_income = sum(Amount)
      ),
      keyby = ""] %>%
        format_output_table(), 
      align = "r"
    )
    browser
    if (input$cash_accounting_categories == "All"){
      output$net_cash_all <- renderTable({
        data_output$bank_data_filtered[, .(Date, `Counter Party`, Reference, Category, Amount)][, Date := as.character(Date)]
      },
      align = "r"
      )
    } else {
      output$net_cash_all <- renderTable({
        data_output$bank_data_filtered[Category == input$cash_accounting_categories][, .(Date, `Counter Party`, Reference, Category, Amount)][, Date := as.character(Date)]
      },
      align = "r"
      )
    }
  }
})
