# This function processes raw bank data read in by the app.

get_currency_lookup <- function(currencies){ # example c("GBPGBP=X", GBPUSD=X")

  currency_error <- try({getQuote(currencies)})
  
  # If error, then grab currencies from a backup
  if (class(currency_error) %in% c("error", "try-error")) {
    warning("Unable to get live exchange rates. Using backup.")
    currency_lookup <- fread("data/exchange_rates.csv")
    # Otherwise, get them from the web
  } else {
    currency_conversions <- 
      getQuote(currencies)
    
    currency_lookup <- 
      data.table(Currency = currencies,
                 XR = currency_conversions$Open)
    
    # Save in case of error next time
    fwrite(currency_lookup, "data/exchange_rates.csv")
  }
  
  return(currency_lookup)
  
}



