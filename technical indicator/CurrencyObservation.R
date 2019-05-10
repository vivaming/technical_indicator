library(quantmod)
library(httr)
library(jsonlite)

source("/Users/mingzhang/Documents/R/dev/technical_indicator/technical indicator/CommodityBaynes_V0.2.R")


##############################################################  GBPUSD  ##############################################################

getSymbols("GBPUSD=X",src="yahoo") 

GBPUSD=`GBPUSD=X`
colnames(GBPUSD)=c('Open', 'High', 'Low', 'Settle', 'Volume', 'Adjusted')
GBPUSDPLot=TechnicalSignalsVisual(GBPUSD, name='GBPUSD')


##############################################################  NZDUSD  ##############################################################

getSymbols("NZDUSD=X",src="yahoo") 

NZDUSD=`NZDUSD=X`
colnames(NZDUSD)=c('Open', 'High', 'Low', 'Settle', 'Volume', 'Adjusted')
NZDUSDPLot=TechnicalSignalsVisual(NZDUSD, name='NZDUSD')



##############################################################  USDJPY  ##############################################################

getSymbols("USDJPY=X",src="yahoo") 

USDJPY=`USDJPY=X`
colnames(USDJPY)=c('Open', 'High', 'Low', 'Settle', 'Volume', 'Adjusted')
USDJPYPLot=TechnicalSignalsVisual(USDJPY, name='USDJPY')


USDJPYRSI=RSIProb(USDJPY$Settle
                  , RSI_n=14
                  , RSI_Threshold=c(30, 60)
                  , Range=0.04
                  , EstimatedPositionLength=30
                  , TargetReturn=0.0001
                  , ContractType='sell')

USDJPYRSI$BestReturns
USDJPYRSI$AbovezWaterRatio

USDJPYCCI=CCIProb(USDJPY$Settle
                  , CCI_n=20
                  , CCI_c=0.015
                  , CCI_Threshold=200
                  , Range=0.05
                  , EstimatedPositionLength=30
                  , TargetReturn=0.001
                  , ContractType='buy' )

USDJPYCCI$BestReturns
USDJPYCCI$AbovezWaterRatio


##############################################################  AUDNZD  ##############################################################

# getSymbols("AUDNZD=X",src="yahoo") 
# 
# AUDNZD=`AUDNZD=X`
# colnames(AUDNZD)=c('Open', 'High', 'Low', 'Settle', 'Volume', 'Adjusted')
# AUDNZDPLot=TechnicalSignalsVisual(AUDNZD, name='AUDNZD')




##############################################################  AUDUSD  ##############################################################

getSymbols("AUDUSD=X",src="yahoo") 

AUDUSD=`AUDUSD=X`
colnames(AUDUSD)=c('Open', 'High', 'Low', 'Settle', 'Volume', 'Adjusted')
AUDUSDPLot=TechnicalSignalsVisual(AUDUSD, name='AUDUSD')



##############################################################  EURUSD  ##############################################################

getSymbols("EURUSD=X",src="yahoo") 

EURUSD=`EURUSD=X`
colnames(EURUSD)=c('Open', 'High', 'Low', 'Settle', 'Volume', 'Adjusted')
EURUSDPLot=TechnicalSignalsVisual(EURUSD, name='EURUSD')




##############################################################  EURNZD  ##############################################################

# getSymbols("EURNZD=X",src="yahoo") 
# 
# EURNZD=`EURNZD=X`
# colnames(EURNZD)=c('Open', 'High', 'Low', 'Settle', 'Volume', 'Adjusted')
# EURNZDPLot=TechnicalSignalsVisual(EURNZD, name='EURNZD')



##############################################################  GBPJPY  ##############################################################

# getSymbols("GBPJPY=X",src="yahoo") 
# 
# GBPJPY=`GBPJPY=X`
# colnames(GBPJPY)=c('Open', 'High', 'Low', 'Settle', 'Volume', 'Adjusted')
# GBPJPYPLot=TechnicalSignalsVisual(GBPJPY, name='GBPJPY')



##############################################################  EURGBP  ##############################################################

# getSymbols("EURGBP=X",src="yahoo")
# 
# EURGBP=`EURGBP=X`
# colnames(EURGBP)=c('Open', 'High', 'Low', 'Settle', 'Volume', 'Adjusted')
# EURGBPPLot=TechnicalSignalsVisual(EURGBP, name='EURGBP')



##############################################################  EURJPY  ##############################################################

# getSymbols("EURJPY=X",src="yahoo")
# 
# EURJPY=`EURJPY=X`
# colnames(EURJPY)=c('Open', 'High', 'Low', 'Settle', 'Volume', 'Adjusted')
# EURJPYPLot=TechnicalSignalsVisual(EURJPY, name='EURJPY')



##############################################################  GBPAUD  ##############################################################

# getSymbols("GBPAUD=X",src="yahoo")
# 
# GBPAUD=`GBPAUD=X`
# colnames(GBPAUD)=c('Open', 'High', 'Low', 'Settle', 'Volume', 'Adjusted')
# GBPAUDPLot=TechnicalSignalsVisual(GBPAUD, name='GBPAUD')


##############################################################  EURCAD  ##############################################################

# getSymbols("EURCAD=X",src="yahoo")
# 
# EURCAD=`EURCAD=X`
# colnames(EURCAD)=c('Open', 'High', 'Low', 'Settle', 'Volume', 'Adjusted')
# EURCADPLot=TechnicalSignalsVisual(EURCAD, name='EURCAD')
