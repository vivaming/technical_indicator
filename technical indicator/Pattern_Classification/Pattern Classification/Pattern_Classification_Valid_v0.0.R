

data_01=data_01[index(data_01)>=as.Date('2016-01-01'), ]
data_01=data_01[!is.na(data_01[, 1]),]

DateInterval=1

#DEFINE NUMBER OF LOOPS
RecordCount=floor((dim(data_01)[1]-90)/DateInterval)


SliceCollection=data.frame()

for (h in (1:RecordCount)) {
  
  CutOffDate=index(data_01[90+DateInterval*(h-1), ])
  
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


data_02=SliceCollection[, !colnames(SliceCollection) %in% c("PresentPrice", "BestReturn", "BestFuturePrice", "BestFuturePriceDate")]


data_03=data_02

# mean=apply(data_03[, -(1:2)], 2, function(x) (mean(x[!x==0])))
# SD=apply(data_03[, -(1:2)], 2, function(x) (sd(x[!x==0])))


data_04=scale(data_03[, -(1:2)], center=mean, scale=SD)

data_04=cbind(data_03[, (1:2)], data_04)
data_04$TargetReturnFlag=as.factor(ifelse(data_04$TargetReturnFlag==1, 'Yes', 'No'))

SVMRadialPred=predict(SVMTuneRadial, data_04[, -(1:2)], type = 'prob')
SVMRadialPred=cbind(CutOffDate=data_03$CutOffDate, SVMRadialPred)
PriceHistory=data.frame(Date=index(SLV), value=SLV)
PriceHistory=PriceHistory[PriceHistory$Date>=as.Date('2016-01-01'), ]

PriceHistory=sqldf("select t1.*, t2.Yes
                    from PriceHistory as t1
                    left join SVMRadialPred as t2 on (t1.Date=t2.CutOffDate)")

#PriceHistory=PriceHistory[PriceHistory$Date>='2016-10-01' & PriceHistory$Date<='2016-10-31', ]

test=PriceHistory[PriceHistory$Yes>=0.6 & !is.na(PriceHistory$Yes), ]


PriceHistory=as.xts(PriceHistory[, -1], order.by = PriceHistory[, 1])

CollectionAll=data.frame()

for (i in (1:dim(test)[1])) {
  StartDate=test$Date[i]
  print(StartDate)
  EndDate=StartDate+19
  TradingPeriod=PriceHistory[paste0(StartDate, '::', EndDate)]
  BestPrice=max(TradingPeriod$SLV)
  StartPrice=PriceHistory[StartDate]$SLV
  Collection=data.frame(StartDate=StartDate, StartPrice=StartPrice, BestPrice=BestPrice, BestReturn=(BestPrice-StartPrice)/StartPrice)
  CollectionAll=rbind(Collection, CollectionAll)
}

# ggplot(PriceHistory, aes(y=SLV, x=Date, col="SLV")) + geom_line() + 
#   geom_line(data=PriceHistory,aes(x = Date, y = Yes*50, col = "Pred")) + 
#   scale_y_continuous(sec.axis = sec_axis(~.*50))
# 
# write.csv(PriceHistory, "/Users/mingzhang/Downloads/PriceHistory.csv")
