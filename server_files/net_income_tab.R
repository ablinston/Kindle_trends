
# Merge all datasets to produce net income figures
observe({
  if(!is.null(data_output$combined_data) &
     !is.null(data_output$bank_data) &
     !is.null(data_output$ams_data)){
    # Overall
    data_output$net_income <-
      aggregate_data_to_monthly(data_output$combined_data[Date >= "2022-06-01",],
                                data_output$bank_data,
                                data_output$ams_data,
                                data_output$facebook_data)
    output$net_income_total <- renderTable({data_output$net_income[Marketplace == "Total", -c("Marketplace"), with = FALSE]},
                                           align = "r")
    output$net_income_usa <- renderTable({data_output$net_income[Marketplace == "Amazon.com", -c("Marketplace"), with = FALSE]},
                                         align = "r")
    output$net_income_uk <- renderTable({data_output$net_income[Marketplace == "Amazon.co.uk", -c("Marketplace"), with = FALSE]},
                                        align = "r")
    output$net_income_aus <- renderTable({data_output$net_income[Marketplace == "Amazon.com.au", -c("Marketplace"), with = FALSE]},
                                         align = "r")
  }
})
  

  