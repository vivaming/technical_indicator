library(TTR)
library(tseries)
library(Quandl)
library(caret)
library(xts)
library(quantmod)


##############################################################  WHEAT  ##############################################################

USWheat=Quandl("CHRIS/CME_W1", api_key="GixSX89oiCWDRyS3B-Dy", type = 'xts')

USWheatClose=USWheat$Settle
WheatCurrentDate=dim(USWheatClose)[1]
WheatLookBack500Days=WheatCurrentDate-500

USWheatCloseRecent=USWheatClose[WheatLookBack500Days:WheatCurrentDate]

# NewUSWheatPrice=data.frame(date='2018-11-13', Settle=518)
# NewUSWheatPrice=xts(NewUSWheatPrice[,2], as.Date(NewUSWheatPrice[,1]))
# USWheatCloseRecent=rbind(USWheatCloseRecent, NewUSWheatPrice)


chartSeries(USWheatCloseRecent, TA="addRSI(); addROC(22); addEMA(10, col='black'); addEMA(30, col='red'); addCCI()", theme = 'white')
chartSeries(USWheatCloseRecent, TA='addMACD(); addBBands(); addRSI(); addCCI(); addDPO()', theme = 'white')





##############################################################  COFFEE  ##############################################################

USCoffee=Quandl("CHRIS/ICE_KC1", api_key="GixSX89oiCWDRyS3B-Dy", type = 'xts')
USCoffeeClose=USCoffee$Settle

CoffeeCurrentDate=dim(USCoffeeClose)[1]
CoffeeLookBack500Days=CoffeeCurrentDate-500

USCoffeeCloseRecent=USCoffeeClose[CoffeeLookBack500Days:CoffeeCurrentDate]


#CHECK PREDICTED PRICE LEVEL


# NewUSCoffeePrice=data.frame(date='2018-11-13', Settle=110)
# NewUSCoffeePrice=xts(NewUSCoffeePrice[,2], as.Date(NewUSCoffeePrice[,1]))
# USCoffeeCloseRecent=rbind(USCoffeeCloseRecent, NewUSCoffeePrice)

chartSeries(USCoffeeCloseRecent, TA="addRSI(); addROC(22); addEMA(10, col='black'); addEMA(30, col='red'); addCCI()", theme = 'white')
chartSeries(USCoffeeCloseRecent, TA='addMACD(); addBBands(); addRSI(); addCCI(); addDPO()', theme = 'white')

# chartSeries(USCoffeeClose['2017-09/2018'], TA="addRSI(); addROC(22); addEMA(10, col='black'); addEMA(30, col='red'); addCCI()", theme = 'white')
# chartSeries(USCoffeeClose['2017-09/2018'], TA='addMACD(); addBBands(); addRSI(); addCCI(); addDPO()', theme = 'white')
# 
# chartSeries(USCoffeeClose['2016-09/2017'], TA="addRSI(); addROC(22); addEMA(10, col='black'); addEMA(30, col='red'); addCCI()", theme = 'white')
# chartSeries(USCoffeeClose['2016-09/2017'], TA='addMACD(); addBBands(); addRSI(); addCCI(); addDPO()', theme = 'white')
# 
# chartSeries(USCoffeeClose['2015-09/2016'], TA="addRSI(); addROC(22); addEMA(10, col='black'); addEMA(30, col='red'); addCCI()", theme = 'white')
# chartSeries(USCoffeeClose['2015-09/2016'], TA='addMACD(); addBBands(); addRSI(); addCCI(); addDPO()', theme = 'white')
#
# chartSeries(USCoffeeClose['2014-09/2015'], TA="addRSI(); addROC(22); addEMA(10, col='black'); addEMA(30, col='red'); addCCI()", theme = 'white')
# chartSeries(USCoffeeClose['2014-09/2015'], TA='addMACD(); addBBands(); addRSI(); addCCI(); addDPO()', theme = 'white')


##############################################################  SUGAR  ##############################################################

SugarNo11=Quandl("CHRIS/ICE_SB1", api_key="GixSX89oiCWDRyS3B-Dy", type = 'xts')
SugarNo11Close=SugarNo11$Settle

SugarCurrentDate=dim(SugarNo11Close)[1]
SugarLookback500Days=SugarCurrentDate-500

SugarNo11CloseRecent=SugarNo11Close[SugarLookback500Days:SugarCurrentDate]

chartSeries(SugarNo11CloseRecent, TA="addRSI(); addROC(22); addEMA(10, col='black'); addEMA(30, col='red'); addCCI()", theme = 'white')
chartSeries(SugarNo11CloseRecent, TA='addMACD(); addBBands(); addRSI(); addCCI(); addDPO()', theme = 'white')



###########################################################  LIVE CATTLE  ##############################################################

LiveCattle=Quandl("CHRIS/CME_LC2", api_key="GixSX89oiCWDRyS3B-Dy", type='xts')
LiveCattleClose=LiveCattle$Settle

LiveCattleCurrentDate=dim(LiveCattleClose)[1]
LiveCattleLookback500Days=LiveCattleCurrentDate-500

LiveCattleCloseRecent=LiveCattleClose[LiveCattleLookback500Days:LiveCattleCurrentDate]
chartSeries(LiveCattleCloseRecent, TA="addRSI(); addROC(22); addEMA(10, col='black'); addEMA(30, col='red'); addCCI()", theme = 'white')
chartSeries(LiveCattleCloseRecent, TA='addMACD(); addBBands(); addRSI(); addCCI(); addDPO()', theme = 'white')


LiveCattleCurrent=LiveCattle[LiveCattleLookback500Days:LiveCattleCurrentDate]
chartSeries(LiveCattleCurrent, TA='addAroon(); addAroonOsc(); addMFI(14);', theme = 'white')

###########################################################  COTTON #################################################################






###########################################################  SOYBEAN  #################################################################

