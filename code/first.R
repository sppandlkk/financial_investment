rm(list=ls())
source("./code/Utils.R")
library(lubridate)
stocks <- c("VOO", "VTI", "VUG", "BND", "BLV") 
dataUse <- GrabStockData(stocks)


### comparing fixed amount and fixed value

result <- CompareFixedAmountWithFixedValue(stocks,
                                           "2011-01-01",
                                           "2019-12-31",
                                           "month",
                                           c(1, 1.5, 2, 999))
result

###
VOO <- SubsetDataByCandance(dataUse[["VOO"]], "2011-01-01", "2019-12-31", "month")
rr <- FixedValuePeriodically(subsetData = VOO,
                       fixed_value = 1000,
                       max_capacity = 2)$data
FixedValuePeriodically(subsetData = VOO,
                             fixed_value = 1000,
                             max_capacity = 2)$result


hist(rr$purchase_value, breaks = 50, main = "")
summary(rr$purchase_value)
