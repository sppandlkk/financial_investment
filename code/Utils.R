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


FixedAmountPeriodically <- function(subsetData, fixed_amount) {
  
  
  subsetData$purchase_price <- subsetData$close
  subsetData$purchase_quantity = fixed_amount/subsetData$close
  subsetData$purchase_value = fixed_amount
  subsetData$sell_price = as.numeric(subsetData[nrow(subsetData), close,])
  subsetData$sell_value = subsetData$purchase_quantity * subsetData$sell_price
  subsetData$profit = subsetData$sell_value - subsetData$purchase_value
  
  number_of_year <- length(unique(subsetData$year))
  
  
  roi <-  sum(subsetData$sell_value)/ sum(subsetData$purchase_value)
  arr <-  ((sum(subsetData$sell_value))/sum(subsetData$purchase_value))^(1/number_of_year) - 1
  
  list(data = subsetData,
       result = data.table(method = "fixed_amount",
                           number_of_year = number_of_year, 
                           investment = sum(subsetData$purchase_value),
                           return = sum(subsetData$sell_value),
                           roi = roi, 
                           arr = arr))
    
}

FixedValuePeriodically <- function(subsetData, 
                                   fixed_value, 
                                   annual_growth_rate = 0,
                                   max_capacity = 9999) {
  
  subsetData$purchase_price <- subsetData$close
  
  ### count # of year
  number_of_year <- length(unique(subsetData$year))
  growth_adjusted_table <- data.table(year = sort(unique(subsetData$year)),
                                      growth_adjusted_value = fixed_value * (1 + annual_growth_rate)^(0: (number_of_year -1) ))
  
  subsetData <- merge(subsetData, growth_adjusted_table, by = "year")
  subsetData$ideal_value <- cumsum(subsetData$growth_adjusted_value)

  
  subsetData[, purchase_value:= 0, ] 
  subsetData[, purchase_quantity:= 0, ]
  subsetData[, cum_purchase_quantity:= 0, ]

  subsetData[1, purchase_value:= fixed_value, ] 
  subsetData[1, purchase_quantity:= fixed_value/purchase_price, ] 
  subsetData[1, cum_purchase_quantity:= purchase_quantity, ]
  
  for (i in 2:nrow(subsetData)) {
    sell_price <- subsetData[i, close, ]
    previous_cum_quantity <- subsetData[i-1, cum_purchase_quantity,]
    cum_value <- sell_price * previous_cum_quantity
    
    subsetData[i, purchase_value := min((subsetData[i, ideal_value, ] - cum_value), subsetData[i, growth_adjusted_value, ] * max_capacity), ]
    current_purchase_quantity <- (subsetData[i, purchase_value, ]/ subsetData[i, purchase_price, ])
    subsetData[i, purchase_quantity := current_purchase_quantity, ]
    
    subsetData[i, cum_purchase_quantity := previous_cum_quantity + purchase_quantity, ]
  }
  
  
  
  
  total_return <- sum(subsetData$purchase_quantity) * subsetData[nrow(subsetData), close, ]
  
  roi <-  total_return/ sum(subsetData$purchase_value)
  arr <-  ((total_return)/sum(subsetData$purchase_value))^(1/number_of_year) - 1
  
  list(data = subsetData,
       result = data.table(method = "fixed_value",
                           number_of_year = number_of_year, 
                           investment = sum(subsetData$purchase_value),
                           return = total_return,
                           roi = roi, 
                           arr = arr,
                           max_capacity = max_capacity))
           
}


CompareFixedAmountWithFixedValue <- function(stocks,
                                             start_date,
                                             end_date,
                                             candance,
                                             max_capacity_list) {
  
  do.call(rbind,
         lapply(stocks, function(s) {
           
           subsetData <- SubsetDataByCandance(dataUse[[s]], start_date, end_date, candance)
           
           a1 <- FixedAmountPeriodically(subsetData = subsetData,
                                         fixed_amount = 1000)
           a1$result$max_capacity = ""
           
           fixed_value_result <- do.call(rbind, lapply(max_capacity_list, function(m) {
             FixedValuePeriodically(subsetData = subsetData,
                                    fixed_value = 1000,
                                    max_capacity = m)$result
           }))
           
           
           data.table(stock = s, rbind(a1$result, fixed_value_result))
           
         })
  )
  
}
