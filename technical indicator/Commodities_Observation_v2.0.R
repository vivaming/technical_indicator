library(TTR)
library(tseries)
library(Quandl)
library(caret)
library(xts)
library(quantmod)
library(tidyquant)

source("/Users/mingzhang/Documents/R/dev/technical_indicator/technical indicator/CommodityBaynes_V0.1.R")



##############################################################  SILVER  ##############################################################

Silver=Quandl("CHRIS/CME_SI1", api_key="GixSX89oiCWDRyS3B-Dy", type='xts')


SilverCurrentDate=dim(Silver)[1]
SilverLookBack500Days=SilverCurrentDate-500

SilverCurrent=Silver[SilverLookBack500Days:SilverCurrentDate, ]

SilverCurrent$Close=SilverCurrent$Settle
chartSeries(SilverCurrent$Close, TA=' addRSI(16);addCCI();',theme = 'white')

#ADX
SilverHLC=Silver[SilverLookBack500Days:SilverCurrentDate, ]
SilverHLC=na.fill(SilverHLC, "extend")
SilverHLC <- ADX(SilverHLC,  n = 20, maType = "EMA", wilder = FALSE)[, c("ADX")]
SilverHLC$High=rep(20, dim(SilverHLC)[1])
SilverHLC$Low=rep(10, dim(SilverHLC)[1])
addTA(SilverHLC, col = c("black", "green", "brown"))

#MACD
SilverMACD=MACD(SilverClose)
SilverMACD$Zero=rep(0, dim(SilverMACD)[1])
addTA(SilverMACD, col=c("black", "red", "grey"))


#CUSTOMIZED RSI
SilverSellRSI=RSI(SilverCloseRecent, n = 16)
SilverSellRSI$Benchmark=rep(50, dim(SilverSellRSI)[1])
addTA(SilverSellRSI, col=c(1,2))

SilverBuyRSI=RSI(SilverCloseRecent, n = 16)
SilverBuyRSI$Benchmark=rep(40, dim(SilverBuyRSI)[1])
addTA(SilverBuyRSI, col=c(1,3))

