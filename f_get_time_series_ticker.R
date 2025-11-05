library(alphavantager)
library(tidyverse)

get_time_series_ticker <- function(ticker = "EUNL.DE", update = F) {
  data_path <- paste0("data/alphavantage_data_", ticker, ".RDS")
  
  # Cache Check
  if(file.exists(data_path) & !update) {
    cat("Reading Data from cache ", data_path, "\n")
    # Cache lesen
    historical_data <- readRDS(data_path)
    
    return(historical_data)
  } else {
    
    cat("Writing Data to cache ", data_path, "\n")
    
    # API abfragen
    readLines("api_key_alphavantage.txt") %>% 
      av_api_key()
    
    # Verwende TIME_SERIES_MONTHLY_ADJUSTED, um Dividenden und Splits zu berücksichtigen
    historical_data <- 
      av_get(
        symbol = ticker,
        av_fun = "TIME_SERIES_MONTHLY_ADJUSTED",
        outputsize = "full" 
      ) %>%
      rename(date = timestamp) %>%
      arrange(date)
    
    saveRDS(historical_data, data_path)
    
    return(historical_data)
  }
}