
# Define a function to plot a chart with a moving average royalty
moving_average_royalty_chart <- function(royalty_data, ma_days, include_net = FALSE, ku_prop = FALSE) {

  # Aggregate by date
  # Facebook Ad spend is duplicated across ASINs and negative, so just take the min
  aggregated_royalties <- royalty_data[, .(Royalty = sum(GBP_royalty),
                                           AMS_Ads = sum(AMS_Ads),
                                           Facebook_Ads = min(Facebook_Ads),
                                           Sales_royalty = sum(GBP_royalty_sales),
                                           KU_royalty = sum(GBP_royalty_ku),
                                           orders = sum(orders),
                                           ku_sales = sum(ku_sales)), 
                                       keyby = Date]
  
  # Compute n-day moving average
  aggregated_royalties[, ":=" (Gross_Royalty_ma = frollmean(Royalty, n = ma_days, algo = "exact", align = "center"),
                               AMS_Ads_ma = frollmean(AMS_Ads, n = ma_days, algo = "exact", align = "center"),
                               Facebook_Ads_ma = frollmean(Facebook_Ads, n = ma_days, algo = "exact", align = "center"),
                               Sales_Royalty_ma = frollmean(Sales_royalty, n = ma_days, algo = "exact", align = "center"),
                               KU_Royalty_ma = frollmean(KU_royalty, n = ma_days, algo = "exact", align = "center"),
                               ku_sales_rollsum = frollsum(ku_sales, n = ma_days, algo = "exact", align = "center"),
                               order_rollsum = frollsum(orders, n = ma_days, algo = "exact", align = "center")
                               )][, ":="(
                                 Net_Royalties_ma = Gross_Royalty_ma + AMS_Ads_ma + Facebook_Ads_ma,
                                 KU_Read_Order_Proportion_ma = ku_sales_rollsum / (ku_sales_rollsum + order_rollsum)
                               )]
  
  # If we want to include net costs in the chart, then do calculations
  if (include_net == TRUE) {
    
    # Convert the data to long format for grouping in chart
    columns_to_include <- c("Gross_Royalty_ma", "AMS_Ads", "Facebook_Ads", "Net_Royalties_ma", "Sales_Royalty_ma", "KU_Royalty_ma")
    
    aggregated_royalties_long <- 
      aggregated_royalties %>%
      select(all_of(c("Date", columns_to_include))) %>%
      pivot_longer(cols = all_of(columns_to_include),
                   names_to = "Measure",
                   values_to = "GBP_Amount")
      
    # Plot the moving average as a chart
    
    chart <- ggplot() +
      geom_line(data = aggregated_royalties_long %>% filter(Measure %in% c("Gross_Royalty_ma", "Net_Royalties_ma")),
                aes(x = Date, y = GBP_Amount, group = Measure, color = Measure)) +
      geom_line(data = aggregated_royalties_long %>% filter(Measure %in% c("Sales_Royalty_ma", "KU_Royalty_ma")),
                aes(x = Date, y = GBP_Amount, group = Measure, color = Measure),
                linetype = "dashed") +
      geom_bar(data = aggregated_royalties_long %>% filter(Measure %in% c("AMS_Ads", "Facebook_Ads")),
               aes(x = Date, y = GBP_Amount, fill = Measure, color = Measure), stat = "identity") +
      ylim(min(c(aggregated_royalties$Facebook_Ads, aggregated_royalties$AMS_Ads)), max(aggregated_royalties$Gross_Royalty_ma))

  } else if (ku_prop){
    # If we want the KU proportion then add it as a secondary axis. Use plotly directly
    
    # Add first line for royalties
    chart <- plot_ly() %>%
      add_trace(x = aggregated_royalties$Date, 
                y = aggregated_royalties$Gross_Royalty_ma,
                mode = "lines",
                type = "scatter",
                name = "Royalty") %>%
    # Add the KU sales proportion
      add_trace(x = aggregated_royalties$Date,
                y = aggregated_royalties$KU_Read_Order_Proportion_ma,
                mode = "lines",
                type = "scatter",
                yaxis = "y2",
                name = "KU_Reads") %>%
    # Format the final chart
      layout(
        yaxis2 = list(
          tickfont = list(color = "red"),
          overlaying = "y",
          side = "right",
          title = "KU_Read_Order_Proportion_ma",
          range = c(0, 1)),
        xaxis = list(title="Date"),
        yaxis = list(title="Gross_Royalty_ma", range = c(0, max(aggregated_royalties$Gross_Royalty_ma, na.rm = TRUE)))) %>%
      layout(plot_bgcolor='#e5ecf6',
           xaxis = list(
             zerolinecolor = '#ffff',
             zerolinewidth = 2,
             gridcolor = 'ffff'),
           yaxis = list(
             zerolinecolor = '#ffff',
             zerolinewidth = 2,
             gridcolor = 'ffff')
    )
    
    # multiplier <- max(aggregated_royalties$Gross_Royalty_ma, na.rm = TRUE)
    #   
    # chart <-  ggplot(aggregated_royalties, aes(x = Date)) +
    #   geom_line(aes(y = Gross_Royalty_ma)) +
    #   geom_line(aes(y = KU_Read_Order_Proportion_ma * multiplier), color = "red") +
    #   scale_y_continuous(
    #     name = "Gross Royalty ma",
    #     limits = c(0, max(aggregated_royalties$Gross_Royalty_ma, na.rm = TRUE)),
    #     sec.axis = sec_axis(~ . / multiplier,
    #                         name = "KU Reads as proportion of all reads ma")
    #   ) 
      
    return(chart)
    
  } else {
    
    # Plot the moving average as a chart
    chart <- ggplot(aggregated_royalties,
                    aes(x = Date, y = Gross_Royalty_ma)) +
      geom_line() +
      ylim(0, max(aggregated_royalties$Gross_Royalty_ma, na.rm = TRUE))
    
  }
  
  return(
    ggplotly(chart) %>%
      layout(margin = list(t = 50, b = 30))

  )
  
}

