### library
require(quantmod)
require(data.table)

GrabStockData <- function(list_of_stock) {
  
  ### loop over each stock 
  ### load price, synchronize names, and add the index (date) to data.table
  ### then put all into data.table because i like it
  
  dataUse <- lapply(list_of_stock, function(x) {
    
    temp <- suppressWarnings(getSymbols(x, src = "yahoo", env = NULL))
    
    names(temp) <- c("open", "high", "low", "close", "volumne", "adjusted")
    data.table(date = index(temp), temp)
  })
  
  ### naming each table
  names(dataUse) <- list_of_stock
  dataUse
  
}


SubsetDataByCandance <- function(dataIn, start_date, end_date, candance) {
  ### restrict date range
  temp_data <- dataIn[date >= as.Date(start_date), , ]
  temp_data <- temp_data[date <= as.Date(end_date), , ]
  
  if (candance == "month") {
    ### extract the first date of each month
    temp_data[, .SD[1], by = list(year(date), month(date))]
    
  } else if (candance == "quarter"){
    temp_data[, .SD[1], by = list(year(date), quarter(date))]
  } else if (candance == "year"){
    temp_data[, .SD[1], by = list(year(date))]
  } else if (candance == "date") {
    ### create a yera field for later use
    temp_data$year <- year(date)
  } else {
    stop ("candance not supported")
  }
}


FixedAmountPeriodically <- function(dataUse, stock, start_date, end_date, candance, fixed_amount) {
  
  subsetData <- SubsetDataByCandance(dataUse[[stock]], start_date, end_date, candance)
  
  subsetData$purchase_price <- subsetData$close
  subsetData$purchase_quantity = fixed_amount/subsetData$close
  subsetData$purchase_value = fixed_amount
  subsetData$sell_price = as.numeric(subsetData[nrow(subsetData), close,])
  subsetData$sell_value = subsetData$purchase_quantity * subsetData$sell_price
  subsetData$profit = subsetData$sell_value - subsetData$purchase_value
  
  number_of_year <- time_length(difftime(min(max(dataUse[[stock]]$date), as.Date(end_date)), 
                                         max(min(dataUse[[stock]]$date), as.Date(start_date))), "years")
  
  
  roi <-  sum(subsetData$sell_value)/ sum(subsetData$purchase_value)
  arr <-  ((sum(subsetData$sell_value))/sum(subsetData$purchase_value))^(1/number_of_year) - 1
  
  list(data = subsetData,
       data.table(stock = stock,
                  start_date = start_date, 
                  end_date = end_date, 
                  number_of_year = number_of_year, 
                  investment = sum(subsetData$purchase_value),
                  return = sum(subsetData$sell_value),
                  roi = roi, 
                  arr = arr))
  
}

FixedValuePeriodically <- function(dataUse, stock, start_date, end_date, candance, fixed_value) {
  
  subsetData <- SubsetDataByCandance(dataUse[[stock]], start_date, end_date, candance)
  
  subsetData$purchase_price <- subsetData$close
  
  subsetData$ideal_value <- (1:nrow(subsetData)) * fixed_value
  
  subsetData[, purchase_value:= 0] 
  subsetData[, purchase_quantity:= 0]
  subsetData[, cum_purchase_quantity:= 0]

  subsetData[1, purchase_value:= fixed_value, ] 
  subsetData[1, purchase_quantity:= fixed_value/purchase_price, ] 

  for (i in 2:nrow(subsetData)) {
    sell_price <- subsetData[i, close, ]
    cum_quantity <- sum(subsetData[1:(i-1), purchase_quantity,])
    cum_value <- sell_price * cum_quantity
    
    subsetData[i-1, cum_purchase_quantity := cum_quantity]

    
    subsetData[i, purchase_value := (subsetData[i, ideal_value, ] - cum_value), ]
    subsetData[i, purchase_quantity := (subsetData[i, purchase_value, ]/ subsetData[i, purchase_price, ]), ]
    
  }
  
  
  number_of_year <- time_length(difftime(min(max(dataUse[[stock]]$date), as.Date(end_date)), 
                                         max(min(dataUse[[stock]]$date), as.Date(start_date))), "years")
  
  total_return <- sum(subsetData$purchase_quantity) * subsetData[nrow(subsetData), close, ]
  
  roi <-  total_return/ sum(subsetData$purchase_value)
  arr <-  ((total_return)/sum(subsetData$purchase_value))^(1/number_of_year) - 1
  
  list(data = subsetData,
       data.table(stock = stock,
                  start_date = start_date, 
                  end_date = end_date, 
                  number_of_year = number_of_year, 
                  investment = sum(subsetData$purchase_value),
                  return = total_return,
                  roi = roi, 
                  arr = arr))
  
}



