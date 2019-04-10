library(TTR)
library(tseries)
library(Quandl)
library(caret)
library(xts)
library(quantmod)
library(tidyquant)

source("/Users/mingzhang/Documents/R/dev/technical_indicator/technical indicator/CommodityBaynes_V0.2.R")


##############################################################  SILVER  ##############################################################

Silver=Quandl("CHRIS/CME_SI1", api_key="GixSX89oiCWDRyS3B-Dy", type='xts')

SilverClose=Silver$Settle

SilverCurrentDate=dim(SilverClose)[1]
SilverLookback500Days=SilverCurrentDate-500

SilverCloseRecent=SilverClose[SilverLookback500Days:SilverCurrentDate, ]

# NewPrice=data.frame(date='2019-03-21', Settle=16.0)
# NewPrice=xts(NewPrice[,2], as.Date(NewPrice[,1]))
# SilverCloseRecent=rbind(SilverCloseRecent, NewPrice)

#RSI buy RSI_n=16 Oversold 35
#RSI sell RSI_n=12 Oversold 70

chartSeries(SilverCloseRecent, TA='addMACD(); addBBands(); addRSI(16); addCCI(); addDPO(20);addDPO(3); addROC()', theme = 'white')
SilverSellRSI=RSI(SilverCloseRecent, n = 12)
SilverSellRSI$Benchmark=rep(70, dim(SilverSellRSI)[1])
addTA(SilverSellRSI, col=c(1,2))

SilverBuyRSI=RSI(SilverCloseRecent, n = 16)
SilverBuyRSI$Benchmark=rep(35, dim(SilverBuyRSI)[1])
addTA(SilverBuyRSI, col=c(1,3))



SilverCurrent=Silver[SilverLookback500Days:SilverCurrentDate, c(1, 2, 3, 6, 7)]


SilverCurrent$Close=SilverCurrent$Settle
chartSeries(SilverCurrent, TA='addAroonOsc(); addMFI(14); addOBV(); addRSI(16);addCCI(); addDPO(20); addATR() ; ', theme = 'white')

SilverHLC=Silver[SilverLookback500Days:SilverCurrentDate,]


SilverHLC=na.fill(SilverHLC, "extend")
SilverHLC <- ADX(SilverHLC,  n = 14, maType = "EMA", wilder = TRUE)[, c("DIp", "DIn", "ADX")]
SilverHLC[, 3]=SilverHLC[, 3]*2
SilverHLC$Benchmark=rep(40, dim(SilverHLC)[1])
addTA(SilverHLC, col = c("green", "red", "blue", "black"), lwd = c(1, 1, 3, 1), legend = NULL)



SilverRSI=RSIProb(Silver$Settle
                  , RSI_n=16
                  , RSI_Threshold=c(35, 70)
                  , Range=0.04
                  , EstimatedPositionLength=30
                  , TargetReturn=0.0001
                  , ContractType='buy')

test=SilverRSI$RawDataset
SilverRSI$BestReturns
SilverRSI$AbovezWaterRatio
SilverRSI$TradingDaysDistribution


SilverCCI=CCIProb(Silver$Settle
                  , CCI_n=20
                  , CCI_c=0.015
                  , CCI_Threshold=145
                  , Range=0.05
                  , EstimatedPositionLength=30
                  , TargetReturn=0.001
                  , ContractType='buy' )

SilverCCI$BestReturns
SilverCCI$AbovezWaterRatio



chartSeries(SilverCloseRecent, TA='addMACD(); addBBands(); addRSI(16); addCCI(); addDPO(20);addDPO(3); addROC()', theme = 'white')
SilverSellRSI=RSI(SilverCloseRecent, n = 16)
SilverSellRSI$Benchmark=rep(50, dim(SilverSellRSI)[1])
addTA(SilverSellRSI, col=c(1,2))

SilverBuyRSI=RSI(SilverCloseRecent, n = 16)
SilverBuyRSI$Benchmark=rep(40, dim(SilverBuyRSI)[1])
addTA(SilverBuyRSI, col=c(1,3))

##############################################################  WHEAT  ##############################################################

USWheat=Quandl("CHRIS/CME_W1", api_key="GixSX89oiCWDRyS3B-Dy", type = 'xts')

USWheatClose=USWheat$Settle
WheatCurrentDate=dim(USWheatClose)[1]
WheatLookBack500Days=WheatCurrentDate-500

USWheatCloseRecent=USWheatClose[WheatLookBack500Days:WheatCurrentDate]

# NewUSWheatPrice=data.frame(date='2018-11-13', Settle=518)
# NewUSWheatPrice=xts(NewUSWheatPrice[,2], as.Date(NewUSWheatPrice[,1]))
# USWheatCloseRecent=rbind(USWheatCloseRecent, NewUSWheatPrice)


chartSeries(USWheatCloseRecent, TA='addMACD(); addBBands(); addRSI(10); addCCI(10); addDPO(20)', theme = 'white')





##############################################################  COFFEE  ##############################################################

Coffee=Quandl("CHRIS/ICE_KC2", api_key="GixSX89oiCWDRyS3B-Dy", type = 'xts')
CoffeeClose=Coffee$Settle

CoffeeCurrentDate=dim(CoffeeClose)[1]
CoffeeLookBack500Days=CoffeeCurrentDate-500

CoffeeCloseRecent=CoffeeClose[CoffeeLookBack500Days:CoffeeCurrentDate]
#CoffeeCloseRecent=CoffeeClose['2007/2009']

#CHECK PREDICTED PRICE LEVEL

# NewCoffeePrice=data.frame(date='2019-03-17', Settle=100)
# NewCoffeePrice=xts(NewCoffeePrice[,2], as.Date(NewCoffeePrice[,1]))
# CoffeeCloseRecent=rbind(CoffeeCloseRecent, NewCoffeePrice)

#chartSeries(CoffeeCloseRecent, TA="addRSI(); addROC(22); addEMA(10, col='black'); addEMA(30, col='red'); addCCI()", theme = 'white')
chartSeries(CoffeeCloseRecent, TA='addMACD(); addBBands(); addRSI(); addCCI(); addDPO(20); addDPO(3)', theme = 'white')

#ADD VERTICAL LINE
#addTA(xts(TRUE,as.POSIXlt("2008-09-15",tz="GMT")),on=1)



CoffeeCurrent=Coffee[CoffeeLookBack500Days:CoffeeCurrentDate, c(1,2,3,4,7)]
#CoffeeCurrent=Coffee['2016/2018', c(1,2,3,4,7)]

CoffeeCurrent$Close=CoffeeCurrent$Settle
chartSeries(CoffeeCurrent, TA='addAroonOsc(); addMFI(14); addOBV(); addRSI(); addCCI(); addDPO(20); addATR();' , theme = 'white')

chartSeries(CoffeeCurrent, TA='addAroonOsc(); addMFI(14);' , theme = 'white')

CoffeeHLC=Coffee[CoffeeLookBack500Days:CoffeeCurrentDate, ]

#CoffeeHLC=Coffee['2016/2018']

CoffeeHLC=na.fill(CoffeeHLC, "extend")
CoffeeHLC <- ADX(CoffeeHLC,  n = 14, maType = "EMA", wilder = TRUE)[, c("DIp", "DIn", "ADX")]
CoffeeHLC[, 3]=CoffeeHLC[, 3]*2
CoffeeHLC$Benchmark=rep(40, dim(CoffeeHLC)[1])
addTA(CoffeeHLC, col = c("green", "red", "blue", "black"), lwd = c(1, 1, 3, 1), legend = NULL)


CoffeeRSI=RSIProb(Coffee$Settle
                  , RSI_n=14
                  , RSI_Threshold=c(31.62, 59.8)
                  , Range=0.05
                  , EstimatedPositionLength=30
                  , TargetReturn=0.001
                  , ContractType='buy')

CoffeeRSI$BestReturns
CoffeeRSI$AbovezWaterRatio
CoffeeOdd=CoffeeRSI$RawDataset


CoffeeCCI=CCIProb(Coffee$Settle
                 , CCI_n=20
                 , CCI_c=0.015
                 , CCI_Threshold=141
                 , Range=0.05
                 , EstimatedPositionLength=30
                 , TargetReturn=0.001
                 , ContractType='buy' )


CoffeeCCI$BestReturns
CoffeeCCI$AbovezWaterRatio



#ODD DATES WHEN THE RSI INDICATOR FAILED - 21/05/2013
chartSeries(CoffeeClose['2012-09/2013'], TA='addMACD(); addBBands(); addRSI(); addCCI(); addDPO()', theme = 'white')

#ODD DATES WHEN THE RSI INDICATOR FAILED - 13/02/2012
chartSeries(CoffeeClose['2011-09/2012'], TA='addMACD(); addBBands(); addRSI(); addCCI(); addDPO()', theme = 'white')

#ODD DATES WHEN THE RSI INDICATOR FAILED - 13/02/2012
chartSeries(CoffeeClose['2014-09/2019'], TA='addMACD(); addBBands(); addRSI(); addCCI(); addDPO()', theme = 'white')



# 
# chartSeries(CoffeeClose['2016-09/2017'], TA="addRSI(); addROC(22); addEMA(10, col='black'); addEMA(30, col='red'); addCCI()", theme = 'white')
# chartSeries(CoffeeClose['2016-09/2017'], TA='addMACD(); addBBands(); addRSI(); addCCI(); addDPO()', theme = 'white')
# 
# chartSeries(CoffeeClose['2015-09/2016'], TA="addRSI(); addROC(22); addEMA(10, col='black'); addEMA(30, col='red'); addCCI()", theme = 'white')
# chartSeries(CoffeeClose['2015-09/2016'], TA='addMACD(); addBBands(); addRSI(); addCCI(); addDPO()', theme = 'white')
#
# chartSeries(CoffeeClose['2014-09/2015'], TA="addRSI(); addROC(22); addEMA(10, col='black'); addEMA(30, col='red'); addCCI()", theme = 'white')
# chartSeries(CoffeeClose['2014-09/2015'], TA='addMACD(); addBBands(); addRSI(); addCCI(); addDPO()', theme = 'white')


##############################################################  SUGAR  ##############################################################

SugarNo11=Quandl("CHRIS/ICE_SB1", api_key="GixSX89oiCWDRyS3B-Dy", type = 'xts')
SugarNo11Close=SugarNo11$Settle

SugarCurrentDate=dim(SugarNo11Close)[1]
SugarLookback500Days=SugarCurrentDate-500

SugarNo11CloseRecent=SugarNo11Close[SugarLookback500Days:SugarCurrentDate]

#chartSeries(SugarNo11CloseRecent, TA="addRSI(); addROC(22); addEMA(10, col='black'); addEMA(30, col='red'); addCCI()", theme = 'white')
chartSeries(SugarNo11CloseRecent, TA='addMACD(); addBBands(); addRSI(); addCCI(); addDPO(20)', theme = 'white')



###########################################################  LIVE CATTLE  ##############################################################

LiveCattle=Quandl("CHRIS/CME_LC2", api_key="GixSX89oiCWDRyS3B-Dy", type='xts')
LiveCattleClose=LiveCattle$Settle

LiveCattleCurrentDate=dim(LiveCattleClose)[1]
LiveCattleLookback500Days=LiveCattleCurrentDate-500

LiveCattleCloseRecent=LiveCattleClose[LiveCattleLookback500Days:LiveCattleCurrentDate]
#chartSeries(LiveCattleCloseRecent, TA="addRSI(); addROC(22); addEMA(10, col='black'); addEMA(30, col='red'); addCCI()", theme = 'white')
chartSeries(LiveCattleCloseRecent, TA='addMACD(); addBBands(); addRSI(); addCCI(); addDPO()', theme = 'white')


LiveCattleCurrent=LiveCattle[LiveCattleLookback500Days:LiveCattleCurrentDate]
chartSeries(LiveCattleCurrent, TA='addAroon(); addAroonOsc(); addMFI(14);', theme = 'white')

###########################################################  COTTON #################################################################

CottonNo2=Quandl("CHRIS/ICE_CT1", api_key="GixSX89oiCWDRyS3B-Dy", type='xts')
CottonNo2Close=CottonNo2$Settle

CottonCurrentDate=dim(CottonNo2Close)[1]
CottonLookBack500Days=CottonCurrentDate-500

CottonCloseRecent=CottonNo2Close[CottonLookBack500Days:CottonCurrentDate, ]
#chartSeries(CottonCloseRecent, TA="addRSI(); addROC(22); addEMA(10, col='black'); addEMA(30, col='red'); addCCI()", theme = 'white')
chartSeries(CottonCloseRecent, TA='addMACD(); addBBands(); addRSI(); addCCI(); addDPO(20)', theme = 'white')



CottonCurrent=CottonNo2[CottonLookBack500Days:CottonCurrentDate, c(1,2,3,4,7)]


CottonCurrent$Close=CottonCurrent$Settle
chartSeries(CottonCurrent, TA='addAroonOsc(); addMFI(14); addOBV();' , theme = 'white')

chartSeries(CottonCurrent, TA='addAroonOsc(); addMFI(14);' , theme = 'white')

CottonHLC=CottonNo2[CottonLookBack500Days:CottonCurrentDate, ]

#CottonHLC=Cotton['2016/2018']

CottonHLC=na.fill(CottonHLC, "extend")
CottonHLC <- ADX(CottonHLC,  n = 14, maType = "EMA", wilder = TRUE)[, c("DIp", "DIn", "ADX")]
CottonHLC[, 3]=CottonHLC[, 3]*2
CottonHLC$Benchmark=rep(40, dim(CottonHLC)[1])
addTA(CottonHLC, col = c("green", "red", "blue", "black"), lwd = c(1, 1, 3, 1), legend = NULL)



###########################################################  SOYBEAN  #################################################################

SoybeanNo1=Quandl("CHRIS/CME_S1", api_key="GixSX89oiCWDRyS3B-Dy", type='xts')
SoybeanNo1Close=SoybeanNo1$Settle

SoybeanCurrentDate=dim(SoybeanNo1Close)[1]
SoybeanLookback500Days=SoybeanCurrentDate-500

SoybeanCloseRecent=SoybeanNo1Close[SoybeanLookback500Days:SoybeanCurrentDate, ]
#chartSeries(SoybeanCloseRecent, TA="addRSI(); addROC(22); addEMA(10, col='black'); addEMA(30, col='red'); addCCI()", theme = 'white')
chartSeries(SoybeanCloseRecent, TA='addMACD(); addBBands(); addRSI(); addCCI(); addDPO()', theme = 'white')


###########################################################  COCOA  #################################################################


Cocoa=Quandl("CHRIS/ICE_CC1", api_key="GixSX89oiCWDRyS3B-Dy", type='xts')

CocoaClose=Cocoa$Settle

CocoaCurrentDate=dim(CocoaClose)[1]
CocoaLookBack500Days=CocoaCurrentDate-500

CocoaCloseRecent=CocoaClose[CocoaLookBack500Days:CocoaCurrentDate, ]

# NewPrice=data.frame(date='2019-02-14', Settle=2330)
# NewPrice=xts(NewPrice[,2], as.Date(NewPrice[,1]))
# CocoaCloseRecent=rbind(CocoaCloseRecent, NewPrice)


#chartSeries(CocoaCloseRecent, TA="addRSI(); addROC(22); addEMA(10, col='black'); addEMA(30, col='red'); addCCI()", theme = 'white')
chartSeries(CocoaCloseRecent, TA='addMACD(); addBBands(); addRSI(); addCCI(); addDPO(20); addDPO(3)', theme = 'white')



CocoaCurrent=Cocoa[CocoaLookBack500Days:CocoaCurrentDate, c(1,2,3,4,7)]

#CoffeeCurrent=Coffee['2016/2018', c(2, 3, 6, 7)]

CocoaCurrent$Close=CocoaCurrent$Settle
#chartSeries(CocoaCurrent, TA='addAroonOsc(); addMFI(14); addOBV();' , theme = 'white')
chartSeries(CocoaCurrent, TA='addAroonOsc(); addMFI(14); addDPO(20); addRSI();addCCI(); addATR()' , theme = 'white')

CocoaHLC=Cocoa[CocoaLookBack500Days:CocoaCurrentDate, -11]

#CocoaHLC=Cocoa['2016/2018']

CocoaHLC=na.fill(CocoaHLC, "extend")
CocoaHLC <- ADX(CocoaHLC,  n = 14, maType = "EMA", wilder = TRUE)[, c("DIp", "DIn", "ADX")]
CocoaHLC[, 3]=CocoaHLC[, 3]*2
CocoaHLC$Benchmark=rep(40, dim(CocoaHLC)[1])
addTA(CocoaHLC, col = c("green", "red", "blue", "black"), lwd = c(1, 1, 3, 1), legend = NULL)


#STOCK DATA
CocoaStock=read.csv('/Users/mingzhang/Documents/R/Commodity Data/Cocoa_Port_Stocks.csv')
CocoaStock=CocoaStock[!is.na(CocoaStock$Stock), ]

CocoaStockHistory=xts(CocoaStock[, 7], order.by=as.Date(CocoaStock[, 1]))
CocoaStockHistory=merge(CocoaClose, CocoaStockHistory)

colnames(CocoaStockHistory)=c('Price', 'Stock')
CocoaStockHistory[,2]=na.locf(CocoaStockHistory[,2], fromNext = TRUE)

CocoaStockHistory=CocoaStockHistory[!is.na(CocoaStockHistory$Price), ]
CocoaStockHistory_R36=CocoaStockHistory['2015/']


ggplot(CocoaStockHistory_R36, aes(x=Index, y=Stock/1500, col='Stock')) + geom_line() + scale_y_continuous(sec.axis = sec_axis(~./1500)) + 
  geom_line(data=CocoaStockHistory_R36$Price, aes(x = Index, y = Price, col = 'Price')) + theme_tq()
  
# CocoaRSI=RSIProb(Cocoa$Settle
#                   , RSI_n=14
#                   , RSI_Threshold=c(29, 59.8)
#                   , Range=0.05
#                   , EstimatedPositionLength=30
#                   , TargetReturn=0.001
#                   , ContractType='buy')



CocoaCCI=CCIProb(Cocoa$Settle
                 , CCI_n=20
                 , CCI_c=0.015
                 , CCI_Threshold=190
                 , Range=0.05
                 , EstimatedPositionLength=30
                 , TargetReturn=0.001
                 , ContractType='buy' )

CocoaCCI$BestReturns
CocoaCCI$AbovezWaterRatio
test=CocoaCCI$RawDataset

###########################################################  COPPER  #################################################################
Copper=Quandl("CHRIS/CME_HG1", api_key="GixSX89oiCWDRyS3B-Dy", type='xts')

CopperClose=Copper$Settle

CopperCurrentDate=dim(CopperClose)[1]
CopperLookback500Days=CopperCurrentDate-500

CopperCloseRecent=CopperClose[CopperLookback500Days:CopperCurrentDate, ]

# NewPrice=data.frame(date='2019-02-22', Settle=2.58)
# NewPrice=xts(NewPrice[,2], as.Date(NewPrice[,1]))
# CopperCloseRecent=rbind(CopperCloseRecent, NewPrice)

chartSeries(CopperCloseRecent, TA='addMACD(); addBBands(); addRSI(); addCCI(); addDPO(20); addDPO(3)', theme = 'white')



CopperCurrent=Copper[CopperLookback500Days:CopperCurrentDate, c(2, 3, 6, 7)]

#CopperCurrent=Copper['2016/2018', c(2, 3, 6, 7)]

CopperCurrent$Close=CopperCurrent$Settle
chartSeries(CopperCurrent, TA='addAroonOsc(); addMFI(14); addOBV();' , theme = 'white')

CopperHLC=Copper[CopperLookback500Days:CopperCurrentDate,]

#CopperHLC=Copper['2016/2018']

CopperHLC=na.fill(CopperHLC, "extend")
CopperHLC <- ADX(CopperHLC,  n = 14, maType = "EMA", wilder = TRUE)[, c("DIp", "DIn", "ADX")]
CopperHLC[, 3]=CopperHLC[, 3]*2
addTA(CopperHLC, col = c("green", "red", "blue"), lwd = c(1, 1, 3), legend = NULL)


test=MFIProb(Copper
             , MFI_n=14
             , HLC = c('High', 'Low', 'Settle')
             , Volume = 'Volume'
             , MFI_Threshold=c(30, 90)
             , Range=0.09
             , EstimatedPositionLength=30
             , TargetReturn=0.00
             , ContractType='sell')


CopperCCI=CCIProb(Copper$Settle
                  , CCI_n=14
                  , CCI_c=0.015
                  , CCI_Threshold=220
                  , Range=0.05
                  , EstimatedPositionLength=50
                  , TargetReturn=0.001
                  , ContractType='sell' )
CopperCCI$BestReturns
CopperCCI$AbovezWaterRatio


CopperRSI=RSIProb(Copper$Settle
                 , RSI_n=14
                 , RSI_Threshold=c(35, 71)
                 , Range=0.05
                 , EstimatedPositionLength=30
                 , TargetReturn=0.001
                 , ContractType='sell')

CopperRSI$BestReturns
CopperRSI$TradingDaysDistribution
CopperRSI$AbovezWaterRatio
CopperRSIRaw=CopperRSI$RawDataset
