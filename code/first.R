rm(list=ls())
source("./code/Utils.R")
library(lubridate)
stocks <- c("BND", "VOO", "VTI", "VWO", "BLV") 
dataUse <- GrabStockData(stocks)


### comparing fixed amount and fixed value
do.call(rbind,
  lapply(stocks, function(s) {
    
    subsetData <- SubsetDataByCandance(dataUse[[s]], "2010-01-01", "2020-12-31", "month")
    
    a1 <- FixedAmountPeriodically(subsetData = subsetData,
                                   fixed_amount = 1000)[[2]]
    a1$method <- "fixed_amount"
  
    a2 <- FixedValuePeriodically(subsetData = subsetData,
                            fixed_value = 1000)[[2]]
    a2$method <- "fixed_value"
    
    data.table(stock = s, rbind(a1, a2))
    
  })
)
   

