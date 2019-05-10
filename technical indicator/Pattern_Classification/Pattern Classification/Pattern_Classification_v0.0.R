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
options(max.print = 3000)


FillDateGaps=function(TS, locf=TRUE)
{
  #USTreasury10Y=TS
  
  Date=index(TS)
  Date=seq.Date(min(Date), max(Date), by='day')
  Date=data.frame(Date=seq.Date(min(Date), max(Date), by='day'), value=rep(1, length(Date)))
  Date=xts(Date[,2], order.by = Date[,1])
  
  TS=merge(Date, TS)[, -1]
  
  #USE THE PREVIOUS VALUE TO FILL THE MISSING VALUE 
  if (locf==TRUE) {
    TS=na.locf(TS[,1], fromNext = TRUE) 
    
  }
  return(TS)
}


CutOffLookBack=function(TS, CutOffDate, LookBackDays=6, ColnamePrefix) {
  
  # TS=data_01[, 3]
  # LookBackDays=6
  # ColnamePrefix='USIndexVol'
  # CutOffDate="2008-05-27"

  CutOffDate=as.Date(CutOffDate)
  CutOffDateIndexStart=max(which(index(TS)<=CutOffDate))
  CutOffDateIndexEnd=CutOffDateIndexStart-LookBackDays+1
  if (is.infinite(CutOffDateIndexStart)|CutOffDateIndexStart<LookBackDays) {
    LookBackOutput=data.frame(t(rep(0, LookBackDays)))
  } else
  {
    LookBackOutput=data.frame(t(rev(TS[CutOffDateIndexStart:CutOffDateIndexEnd, 1])))
  }
      colnames(LookBackOutput)=paste0(ColnamePrefix, '_Lag', c(0:(LookBackDays-1)))
  
  Output=list(LookBackOutput=LookBackOutput, CutOffDateIndex=CutOffDateIndexStart)
  return(Output)
}


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
# FEDRate=Quandl("FED/RIFSPFF_N_D", api_key="GixSX89oiCWDRyS3B-Dy", type = 'xts')
# names(FEDRate)="FEDRate"

#FED FUND RATE MONTHLY
FEDRate=Quandl("FRED/FEDFUNDS", api_key="GixSX89oiCWDRyS3B-Dy", type='xts')
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
#USM2=Delt(USM2, type=c('arithmetic'))
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

USTreasury10Y=FillDateGaps(USTreasury10Y, locf = TRUE)
USTreasury30Y=FillDateGaps(USTreasury30Y, locf = TRUE)


# CONVERT DAILY RETURN TO MONTHLY RATE
# BECAUSE THERE ARE GAPS IN TREASURY DATA
# SO WILL NEED TO COMPLETE THE MISSING DATES FIRST

# Date=index(USTreasury10Y)
# Date=seq.Date(min(Date), max(Date), by='day')
# Date=data.frame(Date=seq.Date(min(Date), max(Date), by='day'), value=rep(1, length(Date)))
# Date=xts(Date[,2], order.by = Date[,1])
# 
# USTreasury10Y=merge(Date, USTreasury10Y)[, -1]
# 
# #USE THE PREVIOUS VALUE TO FILL THE MISSING VALUE
# USTreasury10Y=na.locf(USTreasury10Y[,1], fromNext = TRUE)
#EXTRACT THE RATE ON 1ST DATE
USTreasury10Y=USTreasury10Y[day(index(USTreasury10Y))==1, ]
USTreasury30Y=USTreasury30Y[day(index(USTreasury30Y))==1, ]

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
NASDAQIndex=FillDateGaps(NASDAQ[, 1],  locf = TRUE)
NASDAQVOL=FillDateGaps(NASDAQ[, 2],  locf = TRUE)
NASDAQHLV=FillDateGaps(NASDAQ[, 3],  locf = TRUE)

NASDAQ=merge(NASDAQIndex, NASDAQVOL, NASDAQHLV)


#EURUSD
getSymbols("EURUSD=X", from=as.Date('1900-01-01'))
EURUSD=`EURUSD=X`[, c(4)]
EURUSD$EURUSD_HLV=`EURUSD=X`[,2]-`EURUSD=X`[,1]
names(EURUSD)=c('EURUSD', 'EURUSDHLV')

EURUSD=FillDateGaps(EURUSD, locf = TRUE)



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
SLV=na.locf(SLV, fromNext = TRUE)
SLVRSI=RSI(SLV, n = 16)
names(SLVRSI)='SLVRSI'
SLVCCI=CCI(SLV,n=20, c=0.015)
names(SLVCCI)='SLVCCI'


data_01=merge(SLV, USDollarIndex, NASDAQ, EURUSD, SLVRSI, SLVCCI,
              all=TRUE)

for (i in (2:dim(data_01)[2]))
{
  data_01[,i]=na.locf(data_01[,i], fromNext = TRUE)
}

#REMVOE THE EXTRA HISTORY DATA
data_01=data_01[index(data_01)>=TimeSeriesStartDate, ]
data_01=data_01[!is.na(data_01[, 1]),]



#DEFINE NUMBER OF LOOPS
RecordCount=floor((dim(data_01)[1]-90)/10)


SliceCollection=data.frame()

for (h in (1:RecordCount)) {

CutOffDate=index(data_01[90+10*(h-1), ])

#PRICES IN NEXT 14 DAYS

FuturePrices=SLV[paste0(CutOffDate+1, "::", CutOffDate+14)]
PresentPrice=SLV[CutOffDate]

#HIGHEST PRICE IN THE FUTURE DAYS THEN CALCULATE THE RETURN
if (PresentPrice==0) {
  BestReturn=0 }
else {
  BestReturn=(max(FuturePrices)-PresentPrice)/PresentPrice
}
BestFuturePriceDate=index(FuturePrices[which(FuturePrices==max(FuturePrices))])[1]


#CHECK IF THE RETURN IS GREATER THAN TARGET RETURN -- 2% IN THIS CASE
TargetReturnFlag=data.frame(TargetReturnFlag=ifelse(BestReturn>=0.035, 1, 0)
                            , CutOffDate=CutOffDate
                            , PresentPrice=PresentPrice
                            , BestReturn=BestReturn
                            , BestFuturePrice=max(FuturePrices)
                            , BestFuturePriceDate=BestFuturePriceDate)

colnames(TargetReturnFlag)=c('TargetReturnFlag', 'CutOffDate', 'PresentPrice', 'BestReturn', 'BestFuturePrice', 'BestFuturePriceDate')


print(paste0("h: ", h))
print(paste0("CutOffDate: ", CutOffDate))

#DataSlice=data_01[CutOffDate]

# CutOffDateIndexEnd=which(index(data_01)==CutOffDate)
# CutOffDateIndexStart=which(index(data_01)==CutOffDate)-29
# 
# SLVSlice=data.frame(t(data_01[CutOffDateIndexStart:CutOffDateIndexEnd, 1]))
# colnames(SLVSlice)=paste0("SLV_Lag", c(1:30))

print("SLV")
SLVLookBack=CutOffLookBack(data_01[,1]
                           , CutOffDate=CutOffDate
                           , LookBackDays = 30
                           , ColnamePrefix = 'SLV')
SLVSlice=SLVLookBack$LookBackOutput
SLVSlice$SLV_Lag40=as.numeric(data_01[SLVLookBack$CutOffDateIndex-39, 1])
SLVSlice$SLV_Lag50=as.numeric(data_01[SLVLookBack$CutOffDateIndex-49, 1])
SLVSlice$SLV_Lag60=as.numeric(data_01[SLVLookBack$CutOffDateIndex-59, 1])
SLVSlice$SLV_Lag90=as.numeric(data_01[SLVLookBack$CutOffDateIndex-89, 1])

print("SLVRSI")
SLVRSILookBack=CutOffLookBack(data_01[,8]
                           , CutOffDate=CutOffDate
                           , LookBackDays = 30
                           , ColnamePrefix = 'SLVRSI')
SLVRSISlice=SLVRSILookBack$LookBackOutput

print("SLVCCI")
SLVCCILookBack=CutOffLookBack(data_01[,9]
                           , CutOffDate=CutOffDate
                           , LookBackDays = 30
                           , ColnamePrefix = 'SLVCCI')
SLVCCISlice=SLVCCILookBack$LookBackOutput


print("USIndex")
USIndexLookBack=CutOffLookBack(data_01[, 2]
                               , CutOffDate = CutOffDate
                               , LookBackDays = 30
                               , ColnamePrefix='USIndex')
USIndexSlice=USIndexLookBack$LookBackOutput
# USIndexSlice=data.frame(t(data_01[CutOffDateIndexStart:CutOffDateIndexEnd, 2]))
# colnames(USIndexSlice)=paste0("USIndex_Lag", c(1:30))
USIndexSlice$USIndex_Lag40=as.numeric(data_01[USIndexLookBack$CutOffDateIndex-39, 2])
USIndexSlice$USIndex_Lag50=as.numeric(data_01[USIndexLookBack$CutOffDateIndex-49, 2])
USIndexSlice$USIndex_Lag60=as.numeric(data_01[USIndexLookBack$CutOffDateIndex-59, 2])
USIndexSlice$USIndex_Lag90=as.numeric(data_01[USIndexLookBack$CutOffDateIndex-89, 2])

print("USIndexVol")
USIndexVolLookBack=CutOffLookBack(data_01[, 3]
                                  , CutOffDate = CutOffDate
                                  , LookBackDays = 30
                                  , ColnamePrefix='USIndexVol')
USIndexVolSlice=USIndexVolLookBack$LookBackOutput
# USIndexSlice=data.frame(t(data_01[CutOffDateIndexStar
USIndexVolSlice$USIndexVol_Lag40=as.numeric(data_01[USIndexVolLookBack$CutOffDateIndex-39, 3])
USIndexVolSlice$USIndexVol_Lag50=as.numeric(data_01[USIndexVolLookBack$CutOffDateIndex-49, 3])
USIndexVolSlice$USIndexVol_Lag60=as.numeric(data_01[USIndexVolLookBack$CutOffDateIndex-59, 3])
USIndexVolSlice$USIndexVol_Lag90=as.numeric(data_01[USIndexVolLookBack$CutOffDateIndex-89, 3])


print("NASDAQ")
NASDAQLookBack=CutOffLookBack(data_01[, 4]
                           , CutOffDate = CutOffDate
                           , LookBackDays = 30
                           , ColnamePrefix='NASDAQ')
NASDAQSlice=NASDAQLookBack$LookBackOutput
NASDAQSlice$NASDAQ_Lag40=as.numeric(data_01[NASDAQLookBack$CutOffDateIndex-39, 4])
NASDAQSlice$NASDAQ_Lag50=as.numeric(data_01[NASDAQLookBack$CutOffDateIndex-49, 4])
NASDAQSlice$NASDAQ_Lag60=as.numeric(data_01[NASDAQLookBack$CutOffDateIndex-59, 4])
NASDAQSlice$NASDAQ_Lag90=as.numeric(data_01[NASDAQLookBack$CutOffDateIndex-89, 4])


print("NASDAQVol")
NASDAQVolLookBack=CutOffLookBack(data_01[, 5]
                           , CutOffDate = CutOffDate
                           , LookBackDays = 30
                           , ColnamePrefix='NASDAQVol')
NASDAQVolSlice=NASDAQVolLookBack$LookBackOutput
NASDAQVolSlice$NASDAQ_Lag40=as.numeric(data_01[NASDAQVolLookBack$CutOffDateIndex-39, 5])
NASDAQVolSlice$NASDAQ_Lag50=as.numeric(data_01[NASDAQVolLookBack$CutOffDateIndex-49, 5])
NASDAQVolSlice$NASDAQ_Lag60=as.numeric(data_01[NASDAQVolLookBack$CutOffDateIndex-59, 5])
NASDAQVolSlice$NASDAQ_Lag90=as.numeric(data_01[NASDAQVolLookBack$CutOffDateIndex-89, 5])


print("USTreasury10Y")
USTreasury10YLookBack=CutOffLookBack(USTreasury10Y
                                     , CutOffDate = CutOffDate
                                     , LookBackDays = 6
                                     , ColnamePrefix = "USTreasury10Y")
USTreasury10YSlice=USTreasury10YLookBack$LookBackOutput

print("USTreasury30Y")
USTreasury30YLookBack=CutOffLookBack(USTreasury30Y
                                  , CutOffDate = CutOffDate
                                  , LookBackDays = 6
                                  , ColnamePrefix = "USTreasury30Y")
USTreasury30YSlice=USTreasury30YLookBack$LookBackOutput

print("USCPI")
USCPILookBack=CutOffLookBack(USCPI
                          , CutOffDate = CutOffDate
                          , LookBackDays = 6
                          , ColnamePrefix = "USCPI")
USCPISlice=USCPILookBack$LookBackOutput

print("FEDRate")
FEDRateLookBack=CutOffLookBack(FEDRate
                            , CutOffDate = CutOffDate
                            , LookBackDays = 6
                            , ColnamePrefix = "FEDRate")
FEDRateSlice=FEDRateLookBack$LookBackOutput

print("IndustryProd")
IndustryProdLookBack=CutOffLookBack(IndustrialProduction
                            , CutOffDate = CutOffDate
                            , LookBackDays = 6
                            , ColnamePrefix = "IndustryProd")
IndustryProdSlice=IndustryProdLookBack$LookBackOutput

print("EURUSD")
EURUSDLookBack=CutOffLookBack(data_01[, 6]
                           , CutOffDate = CutOffDate
                           , LookBackDays = 30
                           , ColnamePrefix = "EURUSD")
EURUSDSlice=EURUSDLookBack$LookBackOutput
  
EURUSDSlice$EURUSD_Lag40=as.numeric(data_01[EURUSDLookBack$CutOffDateIndex-39, 6])
EURUSDSlice$EURUSD_Lag50=as.numeric(data_01[EURUSDLookBack$CutOffDateIndex-49, 6])
EURUSDSlice$EURUSD_Lag60=as.numeric(data_01[EURUSDLookBack$CutOffDateIndex-59, 6])
EURUSDSlice$EURUSD_Lag90=as.numeric(data_01[EURUSDLookBack$CutOffDateIndex-89, 6])


print("USNonFarm")
USNonFarmLookBack=CutOffLookBack(USNonFarm
                         , CutOffDate = CutOffDate
                         , LookBackDays = 6
                         , ColnamePrefix = "USNonFarm")
USNonFarmSlice=USNonFarmLookBack$LookBackOutput

print("USHPI")
USHPILookBack=CutOffLookBack(USHPI
                          , CutOffDate = CutOffDate
                          , LookBackDays = 6
                          , ColnamePrefix = 'USHPI')
USHPISlice=USHPILookBack$LookBackOutput

DataSlice=cbind(TargetReturnFlag
                , SLVSlice
                # , SLVRSISlice
                # , SLVCCISlice
                , USIndexSlice
                , USIndexVolSlice
                , NASDAQSlice
                , NASDAQVolSlice
                , USTreasury10YSlice
                , USTreasury30YSlice
                , USCPISlice
                , FEDRateSlice
                , IndustryProdSlice
                , EURUSDSlice
                , USNonFarmSlice
                , USHPISlice)
                
rownames(DataSlice)=CutOffDate

SliceCollection=rbind(SliceCollection, DataSlice)             
}

#DROP THE EXTRA COLUMNS

data_02=SliceCollection[, !colnames(SliceCollection) %in% c("PresentPrice", "BestReturn", "BestFuturePrice", "BestFuturePriceDate")]

# sum(data_02$TargetReturnFlag)
# length(data_02$TargetReturnFlag)

data_02_Positive=data_02[data_02$TargetReturnFlag==1, ]
#RANDOM SAMPLING THE NEGATIVE OUTCOMES
data_02_Negative=data_02[data_02$TargetReturnFlag==0, ]
data_02_NegativeSample=data_02_Negative[sample(nrow(data_02_Negative), dim(data_02_Positive)[1]), ] 

data_03=rbind(data_02_Positive, data_02_NegativeSample)


#STANDARDISE THE MATRIX
mean=apply(data_03[, -(1:2)], 2, function(x) (mean(x[!x==0])))
SD=apply(data_03[, -(1:2)], 2, function(x) (sd(x[!x==0])))

data_04=scale(data_03[, -(1:2)], center=mean, scale=SD)



# data_04_pccomp=prcomp(data_04, scale = FALSE)
# library(factoextra)
# fviz_eig(data_04_pccomp, ncp = 20)
# 
# data_04=predict(data_04_pccomp, data_04)[, 1:20]


data_04=cbind(data_03[, (1:2)], data_04)
data_04$TargetReturnFlag=as.factor(ifelse(data_04$TargetReturnFlag==1, 'Yes', 'No'))

# test_01=data_04[data_04$CutOffDate>=as.Date('2016-01-01'), ]
 #write.csv(test_01, "/Users/mingzhang/Downloads/test.csv")

Testing=data_04[data_04$CutOffDate>=as.Date('2016-01-01'), -2]
Training=data_04[data_04$CutOffDate<as.Date('2016-01-01'), -2]


Ctrl <- trainControl(method = "cv"
                     #, number = 10
                     , summaryFunction = twoClassSummary
                     , classProbs = TRUE,
                     , savePredictions = TRUE
                     , verboseIter=TRUE)

MarsGird=expand.grid(degree=c(1)
                     , nprune=(15:20))

# MarsCtrl=trainControl(method='repeatedcv'
#                       , number = 10
#                       , repeats = 3
#                       , verboseIter=TRUE)
set.seed(938397)

MarsTune=caret::train(Training[, -1]
                      , Training[, 1]
                      , method='earth'
                      , tuneGrid=MarsGird
                      , trControl=Ctrl)

MarsPred_01=predict(MarsTune, Testing[, -1], type = 'prob')
MarsPred_01=cbind(Testing$TargetReturnFlag, MarsPred_01)






#SVMRadialGrid=expand.grid(sigma= 2^c(-8, -7, -5, -4, -3), C= 2^c(9, 9.5, 10, 11))
# SVMRadialGrid=expand.grid(sigma= 2^c(-14, -12, -10, -8), C= 2^c(5, 7, 8, 9))
#SVMRadialGrid=expand.grid(sigma= 2^c(-13.8, -13.5, -13.2, -13, -12.8, -12.5, -12.2, -12), C= 2^c(2, 3, 4, 5))
#SVMRadialGrid=expand.grid(sigma= 2^c(-13.8, -13.5, -13.2, -13, -12.8, -12.5, -12.2, -12), C= 10^c(-2:2))
# SVMRadialGrid=expand.grid(sigma= 2^c(-13.8, -13.5, -13.2, -13, -12.8, -12.5, -12.2, -12, -10), C= c(80, 100, 120,140, 180, 200))
# SVMRadialGrid=expand.grid(sigma= 2^c(-16, -15, -13.8, -13.5, -13.2, -13, -12.8, -12.5, -12.2, -12, -10, -8, -6, -4, -2, -1, 1, 3, 5), C= c(30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80))
# SVMRadialGrid=expand.grid(sigma= 2^c(-16, -15, -13.8, -13.5, -13.2, -13, -12.8, -12.5, -12.2), C= c( 60, 65, 70, 75))
SVMRadialGrid=expand.grid(sigma= 2^c(-14.5, -14.2, -14, -13.8, -13.6, -13.8), C=c(18, 20, 24, 26, 30, 32, 35, 40))
#SVMRadialGrid=expand.grid(sigma= 2^c(-13.2, -13.3, -13.4, -13.5, -13.6, -13.7), C=c(13, 15, 16, 18, 20))
SVMRadialGrid=expand.grid(sigma= 2^c(-13.2, -13.3, -13.4, -13.5, -13.6, -13.7), C=c( 60, 65, 70, 75))
SVMRadialGrid=expand.grid(sigma= 2^c(-12.8, -12.5), C=c( 60, 65, 70, 75))


# SVMRadialGrid=expand.grid(sigma= 2^c(-12.5), C= 2^c(5))
# SVMRadialGrid=expand.grid(sigma= 2^c(-12.8), C= 80)
SVMRadialGrid=expand.grid(sigma= 2^c(-13.2), C= 70)
#SVMRadialGrid=expand.grid(sigma= c(2, 4, 8, 10, 16, 20, 50, 100, 120, 150), C= c(2, 4, 6, 8, 10))
#BEST ROC 0.67

# SVMRadialGrid=expand.grid(sigma= 2^c(-2:4), C= 2^c(2, 3, 4, 5))
# Selecting tuning parameters
# Fitting sigma = 0.25, C = 0.5 on full training set

#SVMRadialGrid=expand.grid(sigma= 2^c(4, 7, 9, 11), C= 2^c(-1, 2, 3, 4))


set.seed(938397)

start = Sys.time()
SVMTuneRadial=caret::train(Training[, -1]
                           , Training[, 1]
                           , method='svmRadial'
                           , metric='ROC'
                           , tuneGrid=SVMRadialGrid
                           , trControl=Ctrl)
print(Sys.time() - start)

plot(SVMTuneRadial)

SVMRadialPred=predict(SVMTuneRadial, Testing[, -1], type = 'prob')
SVMRadialPred=cbind(Testing$TargetReturnFlag, SVMRadialPred)
# 
# SVMRadialPred=cbind(SVMRadialPred, CutOffDate=test_01$CutOffDate)
# 
# test_03=SliceCollection[, c(2, 4)]

library(sqldf)
test_02=sqldf("select t1.*, t2.BestReturn
                from SVMRadialPred as t1
                  left join test_03 as t2 on t1.CutOffDate=t2.CutOffDate")

test_04=test_02[test_02$Yes>=0.45,]


#SVMPolyGrid=expand.grid(degree=c(1:5), scale=c(0.001, 0.01, 0.1), C=2^c(2, 3, 4, 5))
#SVMPolyGrid=expand.grid(degree=c(0.2, 0.5, 1, 1.5, 2), scale=2^c(-14, -12, -10), C=2^c(0.1, 0.5, 1, 2))
#SVMPolyGrid=expand.grid(degree=c(1.6, 1.8, 2, 2.2, 2.6), scale=c(0.002, 0.0015, 0.001), C=2^c(1, 2, 2.2, 2.5))
SVMPolyGrid=expand.grid(degree=c(2), scale=c(0.001), C=2^c(2.5))
#ROC 0.66

SVMTunePoly=caret::train(Training[, -1]
                         , Training[, 1]
                           , method='svmPoly'
                           , metric='ROC'
                          , tuneGrid=SVMPolyGrid
                          , trControl=Ctrl)

plot(SVMTunePoly)

SVMPolyPred=predict(SVMTunePoly, Testing[, -1], type = 'prob')
SVMPolyPred=cbind(Testing$TargetReturnFlag, SVMPolyPred)



library(gbm)

# XGBtuneGrid = expand.grid(
#   nrounds=c(100, 350, 600),
#   #early.stop.round = c(100), 
#   max_depth = c(2, 3, 4, 5),
#   eta = c(0.01, 0.02, 0.05),
#   gamma = c(0.01, 0.2),
#   colsample_bytree = c(0.3, 0.5, 0.75),
#   subsample = c(0.50, 0.8),
#   min_child_weight = c(0))

# The final values used for the model were nrounds = 100, max_depth = 4, eta = 0.01, gamma = 0.2, colsample_bytree
# = 0.3, min_child_weight = 0 and subsample = 0.8.
#ROC 0.66

XGBtuneGrid = expand.grid(
  nrounds=c(50, 100, 150),
  #early.stop.round = c(100),
  max_depth = c(4, 6, 8, 10),
  eta = c(0.0005, 0.005, 0.01, 0.05),
  gamma = c(0.05, 0.01, 0.1, 0.5), 
  colsample_bytree = c(0.3, 0.5, 0.75),
  subsample = c(0.8),
  min_child_weight = c(0))

# ROC was used to select the optimal model using the largest value.
# The final values used for the model were nrounds = 150, max_depth = 4, eta = 0.0005, gamma = 0.5,
# colsample_bytree = 0.3, min_child_weight = 0 and subsample = 0.8.
#ROC 0.66


#FOCSU ON NROUNDS AMD DEPTHS AND LEARNING RATE
XGBtuneGrid = expand.grid(
  nrounds=c(150, 200, 250),
  #early.stop.round = c(100),
  max_depth = c(3,4),
  eta = c(0.0005, 0.001, 0.002),
  gamma = c(0.05, 0.01, 0.1, 0.5), 
  colsample_bytree = c(0.2, 0.3, 0.5),
  subsample = c(0.8),
  min_child_weight = c(0))



start = Sys.time()

# XGBTune=caret::train(Training[, -1]
#                       , Training[, 1]
#                       , method = 'xgbTree'
#                       , trControl = Ctrl
#                       , tuneGrid = XGBtuneGrid)


XGBTune=caret::train(Training[, -1]
                     , Training[, 1]
                     , method = 'xgbTree'
                     , metric='ROC'
                     , trControl = Ctrl
                     , tuneGrid = XGBtuneGrid)

print(Sys.time() - start)

plot(XGBTune)

varImp(XGBTune)
varImp(SVMTuneRadial)
varImp(SVMTunePoly)
varImp(MarsTune)















confusionMatrix(Testing$TargetReturnFlag, MarsTune$pred)



library(pROC)
# Select a parameter setting
selectedIndices <- rfFit$pred$mtry == 2
# Plot:
plot.roc(rfFit$pred$obs[selectedIndices],
         rfFit$pred$M[selectedIndices])

