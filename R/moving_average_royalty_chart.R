
# Define a function to plot a chart with a moving average royalty
moving_average_royalty_chart <- function(royalty_data, ma_days) {
  
  # Aggregate by date
  aggregated_royalties <- royalty_data[, .(Royalty = sum(GBP_royalty)), keyby = Date]
  
  # Compute 7-day moving average
  aggregated_royalties[, Royalty_ma := frollmean(Royalty, n = ma_days, algo = "exact", align = "right")]
  
  # Plot the moving average as a chart
  
  return(
    ggplot(aggregated_royalties,
           aes(x = Date, y = Royalty_ma)) +
      geom_line() +
      ylim(0, max(aggregated_royalties$Royalty_ma))
  )
  
}

