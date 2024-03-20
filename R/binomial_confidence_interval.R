# This function takes a proportion and a sample size and calculates an exact binomial 
# confidence interval while dealing with NAs

binomial_confidence_intervals <- function(
    proportions,
    sample_sizes,
    confidence_level = 0.95) {
  
  # Replace any NAs
  proportions[is.na(proportions)] <- 0
  sample_sizes[is.na(sample_sizes)] <- 0
  
  # Calculate the intervals
  intervals <- Hmisc::binconf(
    proportions * sample_sizes, 
    sample_sizes, 
    alpha = 1 - confidence_level,
    method = "exact") %>%
    as.data.frame()
  
  # Deal with NAs by converting to 0
  intervals %>%
    mutate(Upper = if_else(Lower == 0 & Upper == 1, 0, Upper)) %>%
    return()

}