rm(list=ls())
source("./code/Utils.R")
library(lubridate)
stocks <- c("VOO", "VTI", "VUG", "BND", "BLV") 
dataUse <- GrabStockData(stocks)


### comparing fixed amount and fixed value
ans <- do.call(rbind,
  lapply(stocks, function(s) {
    
    subsetData <- SubsetDataByCandance(dataUse[[s]], "2011-01-01", "2019-12-31", "month")
    
    a1 <- FixedAmountPeriodically(subsetData = subsetData,
                                   fixed_amount = 1000)
    # a1$result$sd_purchase_value <- sd(a1$data$purchase_value)
    a1$result$method <- "fixed_amount"
    
  
    a2 <- FixedValuePeriodically(subsetData = subsetData,
                            fixed_value = 1000)
    # a2$result$sd_purchase_value <- sd(a2$data$purchase_value)
    a2$result$method <- "fixed_value"
    
    data.table(stock = s, rbind(a1$result, a2$result))
    
  })
)
ans

VOO <- SubsetDataByCandance(dataUse[["VOO"]], "2011-01-01", "2019-12-31", "month")
rr <- FixedValuePeriodically(subsetData = VOO,
                       fixed_value = 1000)$data
hist(rr$purchase_value, breaks = 50, main = "")
summary(rr$purchase_value)
