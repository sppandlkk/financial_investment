rm(list=ls())
source("./code/Utils.R")
library(lubridate)
stocks <- c("BND", "VOO", "VTI", "VWO") 
dataUse <- GrabStockData(stocks)





ans1 <- sapply(stocks, function(s) {
  FixedAmountPeriodically(dataUse = dataUse,
                                 stock = s,
                                 start_date = "2005-01-01",
                                 end_date = "2020-01-01",
                                 candance = "month",
                                 fixed_amount = 1000)[[2]]
  
  
})
   




ans2 <- sapply(stocks, function(s) {
  FixedValuePeriodically(dataUse = dataUse,
                          stock = s,
                          start_date = "2005-01-01",
                          end_date = "2020-01-01",
                          candance = "month",
                          fixed_value = 1000)[[2]]
  
  
})

ans1
ans2




dd <-FixedValuePeriodically(dataUse = dataUse,
                       stock = "VOO",
                       start_date = "2005-01-01",
                       end_date = "2020-01-01",
                       candance = "month",
                       fixed_value = 1000)[[1]]

### how about we reverse engineering
stock <- dataUse[["VOO"]]
bond <- 

