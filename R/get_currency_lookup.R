# This function processes raw bank data read in by the app.

get_currency_lookup <- function(currencies){ # example c("GBPGBP=X", GBPUSD=X")

  currency_error <- try({priceR::exchange_rate_latest(currency = "GBP")})

  # If error, then grab currencies from a backup
  if (class(currency_error) %in% c("error", "try-error")) {
    warning("Unable to get live exchange rates. Using backup.")
    currency_lookup <- fread("data/exchange_rates.csv")
    # Otherwise, get them from the web
  } else {
    
    old_currency_lookup <- fread("data/exchange_rates.csv")
    
    currency_conversions <- 
      priceR::exchange_rate_latest(currency = "GBP")
    
    colnames(currency_conversions) <- c("Currency", "XR")
    
    currency_lookup <- 
      currency_conversions %>%
      filter(Currency %in% currencies) %>%
      as.data.table
    
    # Save in case of error next time
    fwrite(bind_rows(currency_lookup,
                     old_currency_lookup %>% filter(!(Currency %in% currency_lookup$Currency))),
           "data/exchange_rates.csv")
  }
  
  return(currency_lookup)
  
}



