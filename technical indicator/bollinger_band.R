library(quantmod)
library(ggplot2)

start=as.Date('2016-01-01')
end=Sys.Date()
getMetals('XAG', src='google', from=start, to=end)

XAG_BBAND=BBands(XAGUSD)
#LOOK BACK TO 90 DAYS
start_date=Sys.Date()-90

BBAND_01=XAG_BBAND[index(XAG_BBAND)>=start_date]

lowest_prices_1=which(BBAND_01$mavg==min(BBAND_01$mavg))
lowest_prices_2=which(BBAND_01$mavg==min(BBAND_01$mavg[-lowest_prices_1]))

head(BBAND_01)
