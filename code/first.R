rm(list=ls())
source("./code/Utils.R")
library(lubridate)
stocks <- c("VOO", "VTI", "VUG", "BND", "BLV", "VIX") 
dataUse <- GrabStockData(stocks)

### VIX is not working.... try loading a local version
VIX <- read.csv("./data/VIX.csv")
VIX$date <- as.Date(VIX$date)
VIX <- data.table(VIX)
dataUse[["VIX"]] <- VIX



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
                             fixed_value = 1300,
                             annual_growth_rate = 0.05,
                             max_capacity = 1.2)$data
### this setting can generally produce better results
FixedValuePeriodically(subsetData = VOO,
                             fixed_value = 1350,
                            annual_growth_rate = 0.05,
                             max_capacity = 1.2)$result



FixedAmountPeriodically(subsetData = VOO,
                       fixed_amount = 1000)$result
                       

