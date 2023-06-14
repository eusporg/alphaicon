library(data.table)
library(lubridate)

options(scipen=999)

# Tried the function on FTSE dataframe
# ftse <- fread("FTSE-100.csv")
# ftse[, up_to_date := ymd(up_to_date)]

# Loading previously generated dataframe
exchange_rates <- fread("exchange_rates.csv")

currency_list <- colnames(exchange_rates)[-1]

# The country studied should be put in a function argument ("UK" for the UK, "DK" for Denmark and "RU" for Russia); it is required for handling unknown currencies accordingly (e.g., when converting British data, unknown currencies turn to GBP, and when converting Russian data unknown currencies turn to RUB)
exchange <- function(x, date, currency, country="UK") {
  
  date <- ymd(date)
  
  currency <- toupper(currency)
  
  # If it is already USD, just return the value
  if (currency == "USD") {
    
    return(x)
    
  # Checks if the currencies is in the list of known currencies  
  } else if (currency %in% colnames(exchange_rates)) {
    
    cur_name <- currency
    
  # Otherwise, sets the currencie based on the country studied
  } else {
    
    if (country == "UK") {
      
      cur_name <- "GBP"
      
    } else if (country == "DK") {
      
      cur_name <- "DKK"
      
    } else if (country == "RU") {
      
      cur_name <- "RUB"
      
    } else {
      
      stop("Wrong country attribute! Use either UK, DK, or RU.")
      
    }
    
  }
  
  # Generating starting date from the report date  
  start_date <- date - years(1) + days(1)
  
  # Searching for slice of exchange_rates dataset for the interested period (1 year prior to report date)
  exchange_df <- exchange_rates[date >= start_date & date <= date]
  
  # Get the average exchange rate for the given period
  currency_mean <- mean(exchange_df[[cur_name]], na.rm=T)
  
  # Converting to USD
  return(x / currency_mean)
  
}

# THE FUNCTION IS NOT VECTORIZED
# Therefore, it needs to be wrapped in mapply function to work with a vector

# The example below converts currencies for the FTSE-100 accounts dataframe
#ftse[, `:=`(V1 = NULL,
#            CompanyCategory = NULL,
#            t_assets = round(mapply(exchange, t_assets, up_to_date, currency)),
#            c_assets = round(mapply(exchange, c_assets, up_to_date, currency)),
#            net_assets = round(mapply(exchange, net_assets, up_to_date, currency)),
#            t_liabilities = round(mapply(exchange, t_liabilities, up_to_date, currency)),
#            c_liabilities = round(mapply(exchange, c_liabilities, up_to_date, currency)),
#            t_equity = NULL,
#            year = year(up_to_date))][, `:=`(currency = NULL,
#                                             up_to_date = NULL)]
#fwrite(ftse, "FTSE-100_finalized_USD.csv")
