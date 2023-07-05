
# Define a function to plot a chart with a moving average royalty
moving_average_royalty_chart <- function(royalty_data, ma_days, include_net = FALSE) {

  # Aggregate by date
  # Ad spend is duplicated across ASINs and negative, so just take the min
  aggregated_royalties <- royalty_data[, .(Royalty = sum(GBP_royalty),
                                           AMS_Ads = min(AMS_Ads),
                                           Facebook_Ads = min(Facebook_Ads)), 
                                       keyby = Date]
  
  # Compute n-day moving average
  aggregated_royalties[, ":=" (Gross_Royalty_ma = frollmean(Royalty, n = ma_days, algo = "exact", align = "center"),
                               AMS_Ads_ma = frollmean(AMS_Ads, n = ma_days, algo = "exact", align = "center"),
                               Facebook_Ads_ma = frollmean(Facebook_Ads, n = ma_days, algo = "exact", align = "center")
                               )][,
                                 Net_Royalties_ma := Gross_Royalty_ma + AMS_Ads_ma + Facebook_Ads_ma
                               ]
  
  # If we want to include net costs in the chart, then do calculations
  if (include_net == TRUE) {
    
    # Convert the data to long format for grouping in chart
    columns_to_include <- c("Gross_Royalty_ma", "AMS_Ads_ma", "Facebook_Ads_ma", "Net_Royalties_ma")
    
    aggregated_royalties_long <- 
      aggregated_royalties %>%
      select(all_of(c("Date", columns_to_include))) %>%
      pivot_longer(cols = columns_to_include,
                   names_to = "Measure",
                   values_to = "GBP_Amount")
      
    # Plot the moving average as a chart
    chart <- ggplot(aggregated_royalties_long,
                    aes(x = Date, y = GBP_Amount, group = Measure, color = Measure)) +
      geom_line() +
      ylim(min(c(aggregated_royalties$Facebook_Ads, aggregated_royalties$AMS_Ads)), max(aggregated_royalties$Gross_Royalty_ma))
    
  } else {
    
    # Plot the moving average as a chart
    chart <- ggplot(aggregated_royalties,
                    aes(x = Date, y = Gross_Royalty_ma)) +
      geom_line() +
      ylim(0, max(aggregated_royalties$Gross_Royalty_ma))
    
  }
  
  return(
    ggplotly(chart) %>%
      layout(margin = list(t = 50, b = 30))
  )
  
}

