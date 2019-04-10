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
  
  # TS=USHPI
  # LookBackDays=6
  # ColnamePrefix='USHPI'
  # CutOffDate="1975-05-27"

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


data_01=merge(SLV, USDollarIndex, NASDAQ, EURUSD,
              all=TRUE)

#REMVOE THE EXTRA HISTORY DATA
data_01=data_01[index(data_01)>=TimeSeriesStartDate, ]
data_01=data_01[!is.na(data_01[, 1]),]

#DEFINE NUMBER OF LOOPS
RecordCount=floor((dim(data_01)[1]-90)/7)



SliceCollection=data.frame()

for (h in (1:RecordCount)) {

CutOffDate=index(data_01[90+7*(h-1), ])

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

DataSlice=cbind(SLVSlice
                , USIndexSlice
                , USIndexVOLSlice
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
#START FROM 90 DAYS AND MOVE FORWARD EVERY 15 DAYS


# USDollarIndex        100.000
# USTreasury10Y        100.000
# USCPI                 56.514
# FEDRate               48.957
# IndustrialProduction  33.458
# EURUSD                30.498
# USNonFarm             25.946
# USHPI                 14.608
# USTreasury30Y         11.971
# NASDAQ                 9.246




#DEFINE FUNCTIONS


