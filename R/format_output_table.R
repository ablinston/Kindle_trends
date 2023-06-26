
format_output_table <- function(dataset){

  # Format the outputs
  for (cols in colnames(dataset)[!(colnames(dataset) %in% c("Year", "Month", "Marketplace"))]) {
    if (is.numeric(dataset %>% pull(cols))) {
      dataset[, (cols) := formatC(get(cols), digits = 2, format = "f", big.mark = ",")]
    }
  }
  dataset[ , ":=" (Year = as.integer(Year),
                   Month = as.integer(Month))]
  
  return(dataset)
  
}
