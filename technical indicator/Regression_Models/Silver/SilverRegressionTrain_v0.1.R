library(Quandl)
library(tensorflow)
library(keras)
library(ggplot2)
library(dplyr)
library(quantmod)
library(tidyverse)
library(tidyquant)
library(corrplot)
library(earth)
library(caret)
library(randomForest)

options(scipen=999)

TimeSeriesStartDate='1973-01-02'

#INDUSTRY PRODUCTION

IndustrialProduction=Quandl("FRED/INDPRO", api_key="GixSX89oiCWDRyS3B-Dy", type='xts')
names(IndustrialProduction)="IndustrialProduction"
#CONVERT TO RELEASE DATE
index(IndustrialProduction)=as.Date(index(IndustrialProduction))+41


#US GDP GROWTH RATE
USGDP=Quandl("FRED/Y695RY2Q224SBEA", api_key="GixSX89oiCWDRyS3B-Dy",type = 'xts')
names(USGDP)="USGDP"
#CONVERT TO RELEASE DATE
index(USGDP)=as.Date(index(USGDP))+115

#FED FUND RATE DAILY
FEDRate=Quandl("FED/RIFSPFF_N_D", api_key="GixSX89oiCWDRyS3B-Dy", type = 'xts')
names(FEDRate)="FEDRate"


#US CPI
USCPI=Quandl("RATEINF/CPI_USA", api_key="GixSX89oiCWDRyS3B-Dy", type='xts')
names(USCPI)='USCPI'
#CONVERT TO RELEASE DATE
index(USCPI)=as.Date(index(USCPI))+41


#US DURABLE GOOD
USDurable=Quandl("FRED/DGORDER", api_key="GixSX89oiCWDRyS3B-Dy", type='xts')
names(USDurable)="USDurable"
#CONVERT TO RELEASE DATE
index(USDurable)=as.Date(index(USDurable))+41


#OECD US CONSUMER CONFIDENCE
USCConfidence=Quandl("OECD/KEI_CSCICP02_USA_ST_M", api_key="GixSX89oiCWDRyS3B-Dy")
names(USCConfidence)='USCConfidence'
#CONVERT TO RELEASE DATE
USCConfidence=xts(USCConfidence[, -1], order.by=(USCConfidence[,1]))
index(USCConfidence)=index(USCConfidence)+10


#US NONFARM PAYROLLS
USNonFarm=Quandl("FRED/PAYEMS", api_key="GixSX89oiCWDRyS3B-Dy", type='xts')
names(USNonFarm)='USNonFarm'
#CONVERT TO RELEASE DATE
index(USNonFarm)=as.Date(index(USNonFarm))+41


#US PHILADELPHIA FED MANUFACTURING INDEX
USPhillyFEDManIndex=Quandl("FRBP/GAC", api_key="GixSX89oiCWDRyS3B-Dy")
names(USPhillyFEDManIndex)='USPhillyFEDManIndex'
#REMOVE THE ACTUAL DUPLICATE IN EACH MONTH
USPhillyFEDManIndex=USPhillyFEDManIndex[format(USPhillyFEDManIndex[,1], "%d")=='01',]
USPhillyFEDManIndex=xts(USPhillyFEDManIndex[, -1], order.by = (USPhillyFEDManIndex[, 1]))
#CONVERT TO RELEASE DATE
index(USPhillyFEDManIndex)=as.Date(index(USPhillyFEDManIndex))+41


#US INITIAL JOBLESS CLAIM
USInitialJobless=Quandl("FRED/ICSA", api_key="GixSX89oiCWDRyS3B-Dy", type='xts')
names(USInitialJobless)='USInitialJobless'
#CONVERT TO RELEASE DATE
index(USInitialJobless)=as.Date(index(USInitialJobless))-2

#US FED HOUSE PRICE INDEX
USHPI=Quandl("FRED/USSTHPI", api_key="GixSX89oiCWDRyS3B-Dy", type = 'xts')
names(USHPI)='USHPI'
index(USHPI)=as.Date(index(USHPI))+144


#US M2 MONEY SUPPLY
#SHOULD HAVE USED M3 FIGURES WHICH INCLUDED THE LARGE DEPOSIT $100K+
USM2=Quandl("FED/M2_M", api_key="GixSX89oiCWDRyS3B-Dy", type='xts')
USM2=Delt(USM2, type=c('arithmetic'))
names(USM2)='USM2'
index(USM2)=as.Date(index(USM2))+41
#ggplot(data = USM2[650:dim(USM2)[1]] , aes(x=Index, y=USM2)) + geom_line()+theme_tq()



#US DOLLAR
USDollarIndex=Quandl("CHRIS/ICE_DX1", api_key="GixSX89oiCWDRyS3B-Dy", type='xts')[, c(4,7)]
names(USDollarIndex)=c('USDollarIndex', 'USDollarIndexVol')

#FRED US DOLLAR INDEX
FedUSIndex=Quandl("FRED/DTWEXM", api_key="GixSX89oiCWDRyS3B-Dy", type='xts')
#USE FRED US DOLLAR INDEX TO BACKFILL THE ORIGINAL US DOLLAR FUTURE BETWEEN 02/01/1973 AND 19/11/1985
#ESITMATED DIFFERENCE BETWEEN THESE TWO INDEX IS APPROXIMATELY 1.05

USDollarIndexBackfill=FedUSIndex["/1985-11-19"]*1.05
colnames(USDollarIndexBackfill)='USDollarIndex'
USDollarIndexBackfill$USDollarIndexVol=rep(list(0), length(USDollarIndexBackfill))

USDollarIndex=rbind(USDollarIndexBackfill, USDollarIndex)



# US 2, 10, 30 YEARS TREASURY BOND RATE
USTreasury02Y=Quandl("FRED/DGS2", api_key="GixSX89oiCWDRyS3B-Dy", type='xts')
USTreasury10Y=Quandl("FRED/DGS10", api_key="GixSX89oiCWDRyS3B-Dy", type='xts')
USTreasury30Y=Quandl("FRED/DGS30", api_key="GixSX89oiCWDRyS3B-Dy", type='xts')

names(USTreasury02Y)='USTreasury02Y'
names(USTreasury10Y)='USTreasury10Y'
names(USTreasury30Y)='USTreasury30Y'

library(quantmod)
#S&P 500
getSymbols('^GSPC', src = "yahoo", from=as.Date('1900-01-01'))
SP500=GSPC[, c(4,5)]
SP500$SP500HLV=GSPC[,2]-GSPC[,1]
names(SP500)=c('SP500', 'SP500VOL', 'SP500HLV')

#NASDAQ
getSymbols('^IXIC', src = "yahoo", from=as.Date('1900-01-01'))
NASDAQ=IXIC[, c(4, 5)]
NASDAQ$NASDAQHLV=IXIC[,2]-IXIC[,1]
names(NASDAQ)=c('NASDAQ', 'NASDAQVOL', 'NASDAQHLV')


#EURUSD
getSymbols("EURUSD=X", from=as.Date('1900-01-01'))
EURUSD=`EURUSD=X`[, c(4)]
EURUSD$EURUSD_HLV=`EURUSD=X`[,2]-`EURUSD=X`[,1]
names(EURUSD)=c('EURUSD', 'EURUSDHLV')

#VIX
VIX=Quandl("CHRIS/CBOE_VX1", api_key="GixSX89oiCWDRyS3B-Dy", type='xts')
VIX=VIX$Settle
names(VIX)='VIX'

#CHINA M2 SUPPLY
#100 MILLION RMB
ChinaM2=Quandl("NBSC/A1B0101_M", api_key="GixSX89oiCWDRyS3B-Dy", type='xts')
ChinaM2=Delt(ChinaM2, type=c('arithmetic'))
names(ChinaM2)='ChinaM2'
index(ChinaM2)=as.Date(index(ChinaM2))+41


#SILVER
SLV=Quandl("LBMA/SILVER", api_key="GixSX89oiCWDRyS3B-Dy", type='xts')[,1]
names(SLV)="SLV"

data=merge(SLV, 
           USDollarIndex, 
           IndustrialProduction, 
           USGDP,
           FEDRate,
           USCPI,
           USDurable,
           USCConfidence,
           USNonFarm,
           USPhillyFEDManIndex,
           USInitialJobless,
           USHPI,
           USM2,
           USTreasury02Y,
           USTreasury10Y,
           USTreasury30Y,
           SP500,
           NASDAQ,
           EURUSD,
           VIX,
           ChinaM2,
           all=TRUE)



#ALWAYS USE THE FIRST NUMBER TO FIX NEXT
#TEST THE FromNext rather than FromLast SHOULD BE THE RIGHT OPTION

data01=data
for (i in (2:dim(data)[2]))
{
  data01[,i]=na.locf(data[,i], fromNext = TRUE)
}

#REMVOE THE EXTRA HISTORY DATA
data02=data01[index(data01)>=TimeSeriesStartDate, ]

#REMOVE SATURDAYS AND SUNDAYS
data02=data02[!is.na(data02[, 1]),]

#CONVERT MISSING VALUE TO 0
data02[is.na(data02)]=0


data03=data.frame(data02)
data03$date=as.Date(rownames(data03))
dim(data03)[2]

data03[,-dim(data03)[2]]=apply(data03[,-dim(data03)[2]], 2, as.numeric)

#apply(data03[,-dim(data03)[2]], 2, skewness)

# ggplot(data=data03, aes(x=date, y=SP500HLV))+geom_line()
# ggplot(data=data03, aes(x=USHPI))+geom_histogram()


#REMOVE SAT AND SUN WHICH DOESN'T TRADE
data03=data03[!data03$SLV==0, ]

#STANDARDISE THE MATRIX
mean=apply(data03[,-dim(data03)[2]], 2, function(x) (mean(x[!x==0])))
SD=apply(data03[,-dim(data03)[2]], 2, function(x) (sd(x[!x==0])))

data04=scale(data03[,-dim(data03)[2]], center=mean, scale=SD)

#data04=data04[, !colnames(data04) %in% c('IndustrialProduction', 'USCPI', 'USTreasury30Y', 'NASDAQ', 'USNonFarm', 'USTreasury10Y')]

#data03Preprocessed=preProcess(data03[,-dim(data03)[2]], method=c('center', 'scale','BoxCox'))
#data04=predict(data03Preprocessed, data03[,-dim(data03)[2]])

# apply(data03[,-dim(data03)[2]], 2, skewness)
#apply(data04, 2, skewness)

#CHECK NEAR ZERO VARIANCE VARIABLES
#nearZeroVar(data04)

#CHECK THE LINEAR CORRELATIONS
Correlations=cor(data04)
corrplot(Correlations)

Date=data.frame(as.Date(rownames(data04)))
data05=cbind(Date, data04)
colnames(data05)[1]='date'

#TrainingData_01=data05[data05$date<'2016-01-01' & data05$date>='2000-01-01', ]
TrainingData_01=data05[data05$date<'2016-01-01', ]
TestingData_01=data05[data05$date>='2016-01-01', ]

TrainingData_01=TrainingData_01[, -1]
TestingData_01=TestingData_01[, -1]

# MarsGird=expand.grid(degree=1:2
#                      , nprune=2:20)

MarsGird=expand.grid(degree=1:3
                     , nprune=c(12, 14, 18, 20, 25, 30, 35, 40))

MarsCtrl=trainControl(method='CV'
                      , number = 10
                      , verboseIter=TRUE
                      #, repeats=3
                    
                      )


set.seed(10001)

MarsTune=caret::train(TrainingData_01[, -1]
                      , TrainingData_01[, 1]
                      , method='earth'
                      , metric='RMSE'
                      , tuneGrid=MarsGird
                      , trControl=MarsCtrl
)

cor(MarsTune$finalModel$fitted.values,TrainingData_01[, 1])


# plot(MarsTune)
# MarsTune

#SAVE MODEL ONCE BEING PROPERLY TRAINED
# saveRDS(MarsTune, 
#   "/Users/mingzhang/Documents/R/dev/technical_indicator/technical indicator/Regression_Models/Silver/MarsTune_v0.0.rds")

#MarsTune=readRDS("/Users/mingzhang/Documents/R/dev/technical_indicator/technical indicator/Regression_Models/Silver/MarsTune_v0.0.rds")


MarsPred_01=predict(MarsTune, TestingData_01[, -1])
Predicted=(MarsPred_01*SD[[1]]+mean[[1]])
Original=TestingData_01$SLV*SD[[1]]+mean[[1]]

postResample(pred = Predicted, obs = Original)

PredictOriginal=data.frame(Pred=Predicted, Original=Original)
colnames(PredictOriginal)=c('Pred', 'Original')


ggplot(PredictOriginal, aes(x=seq(1, dim(PredictOriginal)[1]), y=Pred, col='pred')) + geom_line() +
  geom_line(data=PredictOriginal, aes(x=seq(1, dim(PredictOriginal)[1]), y=Original, col='original')) 

varImp(MarsTune)



#JUST CURIOUS WHAT THE 95%+ R SQUARE MODEL'S PREDTIONS LOOK LIKE
summary(MarsTune)
plot(MarsTune)

MarsPredVar=predict(MarsTune, TrainingData_01[, -1])
TrainVarRestore=MarsPredVar*SD[[1]]+mean[[1]]
TrainOrigin=TrainingData_01[,1]*SD[[1]]+mean[[1]]

TrainPredictOrigin=data.frame(Pred=TrainVarRestore, Origin=TrainOrigin)
colnames(TrainPredictOrigin)=c('Pred', 'Original')

ggplot(TrainPredictOrigin, aes(x=seq(1, dim(TrainPredictOrigin)[1]), y=Pred, col='pred')) + geom_line() +
  geom_line(data=TrainPredictOrigin, aes(x=seq(1, dim(TrainPredictOrigin)[1]), y=Original, col='original')) 





#--------------------------------------------------- SVM------------------------------------------------------------


SVMCtrl=trainControl(method='repeatedcv'
                      , number = 10
                      #, repeats = 3
                      , verboseIter=TRUE)

SVMRadialGrid=expand.grid(sigma= 2^c(-8, -7, -5, -4, -3), C= 2^c(9, 9.5, 10, 11))

set.seed(10001)

SVMTuneRadial=caret::train(TrainingData_01[, -1]
                              , TrainingData_01[, 1]
                              , method='svmRadial'
                              , metric='RMSE'
                              , tuneGrid=SVMRadialGrid
                              , trControl=SVMCtrl)

# saveRDS(SVMTuneRadial,
#   "/Users/mingzhang/Documents/R/dev/technical_indicator/technical indicator/Regression_Models/Silver/SVMTuneRadial_v0.1.rds")
#SVMTuneRadial=readRDS("/Users/mingzhang/Documents/R/dev/technical_indicator/technical indicator/Regression_Models/Silver/SVMTuneRadial_v0.1.rds")

# SVMTunePoly=caret::train(TrainingData_01[, -1]
#                            , TrainingData_01[, 1]
#                            , method='svmPoly'
#                            , metric='RMSE'
#                          , tuneLength=5
#                           , trControl=SVMCtrl)

# SVMTuneLinear=caret::train(TrainingData_01[, -1]
#                          , TrainingData_01[, 1]
#                          , method='svmLinear'
#                          , metric='RMSE'
#                          , tuneLength=5
#                          , trControl=SVMCtrl)


SVMPred=predict(SVMTuneRadial, TestingData_01[, -1])

SVMPredicted=(SVMPred*SD[[1]]+mean[[1]])

#SVMPredSMA=SMA(SVMPredicted, 30)

Original=TestingData_01$SLV*SD[[1]]+mean[[1]]

postResample(pred = SVMPredicted, obs = Original)

#SVMPredictOriginal=data.frame(SVMPred=SVMPredSMA, Original=Original)
SVMPredictOriginal=data.frame(SVMPred=SVMPredicted, Original=Original)
colnames(SVMPredictOriginal)=c('SVMPred', 'Original')


ggplot(SVMPredictOriginal, aes(x=seq(1, dim(SVMPredictOriginal)[1]), y=SVMPred, col='SVMPred')) + geom_line() +
  geom_line(data=SVMPredictOriginal, aes(x=seq(1, dim(SVMPredictOriginal)[1]), y=Original, col='original')) 

varImp(SVMTuneRadial)
plot(SVMTuneRadial)



#---------------------------------------------------Random Forest--------------------------------------------------------

RFTuneGird=expand.grid(mtry=c(4, 5, 7, 9))

RFCtrl=trainControl(method='repeatedcv'
                    , number = 10
                    , verboseIter=TRUE)

set.seed(100091)
RFTune <- train(x=TrainingData_01[, -1]
                , y=TrainingData_01[, 1]
                , method = "rf"
                , tuneGrid = RFTuneGird
                , trControl = RFCtrl
                , ntree=200
                , metric='RMSE')

summary(RFTune)
plot(RFTune)

# saveRDS(RFTune,
#   "/Users/mingzhang/Documents/R/dev/technical_indicator/technical indicator/Regression_Models/Silver/RFTune_v0.1.rds")

# RFTune=readRDS("/Users/mingzhang/Documents/R/dev/technical_indicator/technical indicator/Regression_Models/Silver/RFTune_v0.1.rds")


RFPred=predict(RFTune, TestingData_01[, -1])

RFPredicted=(RFPred*SD[[1]]+mean[[1]])
Original=TestingData_01$SLV*SD[[1]]+mean[[1]]

RFPredictOriginal=data.frame(Pred=RFPredicted, Original=Original)
colnames(RFPredictOriginal)=c('Pred', 'Original')


ggplot(RFPredictOriginal, aes(x=seq(1, dim(RFPredictOriginal)[1]), y=Pred, col='pred')) + geom_line() +
  geom_line(data=RFPredictOriginal, aes(x=seq(1, dim(RFPredictOriginal)[1]), y=Original, col='original'))


# ma <- function(x, n = 20){stats::filter(x, rep(1 / n, n), sides = 2)}
# 
# test=(SVMPredictOriginal+RFPredictOriginal)/2
# 
# test$Pred=ma(x = test$SVMPred)
# 
# ggplot(test, aes(x=seq(1, dim(test)[1]), y=Pred, col='pred')) + geom_line() +
#   geom_line(data=test, aes(x=seq(1, dim(test)[1]), y=Original, col='original'))
