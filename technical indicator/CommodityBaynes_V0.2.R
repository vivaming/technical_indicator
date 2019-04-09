library(TTR)
library(tseries)
library(Quandl)
library(caret)
library(xts)
library(quantmod)
library(tidyquant)

#Silver=Quandl("CHRIS/CME_SI1", api_key="GixSX89oiCWDRyS3B-Dy", type='xts')


# TS=Silver$Settle
# RSIDays=c(10, 12, 14, 16, 18, 20)
# OverBoughtThreshold=c(70, 60, 55)
# OverSoldThreshold=c(20, 30, 35 )
# PositionLength=c(20, 30, 40)
# Range=0.05

RSITune=function(TS
                 , RSIDays=c(10, 12, 14, 16, 18, 20)
                 , OverBoughtThreshold=c(70, 60, 55, 50)
                 , OverSoldThreshold=c(20, 30, 35, 40)
                 , PositionLength=c(20, 30, 40, 50)
                 , Range=0.05
)
{
  
  RSICount=length(RSIDays)
  OverBoughtCount=length(OverBoughtThreshold)
  # OverSoldCount=length(OverSoldSignal) THERE IS NO NEED TO COUNT AS OVERSOLD OVERBOUGHT WILL COME IN PAIRS
  PositionLengthCount=length(PositionLength)
  
  TS=na.locf(TS, fromLast = TRUE)
  colnames(TS)='CommodityPrice'
  
  TSStartYear=as.numeric(min(format(as.Date(index(TS)), '%Y')))
  TSEndYear=as.numeric(max(format(as.Date(index(TS)), '%Y')))
  
  
  CollectionSummary=data.frame()
  
  #RSI COUNT LOOP
  for (j in (1:RSICount)) {
    #for (j in (1:1)) {
    RSI_n=RSIDays[j]
    
    print(paste0("RSI - ", RSI_n))
    
    RSI=RSI(TS, RSI_n)
    Indicator_01=merge(TS, RSI)
    Indicator_02=data.frame(Indicator_01[is.na(Indicator_01[, 2])==FALSE, ])
    Indicator_02$Date=as.Date(row.names(Indicator_02))
    
    #OVERBOUGHT OVERSOLD THREHOLD COUNT LOOP
    for (k in (1:OverBoughtCount)) {
      #for (k in (1:1)) {
      OverBoughtSignal=OverBoughtThreshold[k]
      OverSoldSignal=OverSoldThreshold[k]
      print(paste0("OverBoughtSignal: ", OverBoughtSignal))
      print(paste0("OverSoldSignal: ", OverSoldSignal))
      
      #POSITION LENGTH COUNT      
      for (l in (1:PositionLengthCount)) {
        #for (l in (1:1)) {
        EstimatedPositionLength=PositionLength[l]
        print(paste0("EstimatedPositionLength: ", EstimatedPositionLength))
        
        #SET THE DEFAULT PREVIOUS FLAG
        if (exists("PreviousFlag")==FALSE)
        {PreviousFlag=0}
        
        xtsLength=dim(Indicator_02)[1]
        for (m in (1:xtsLength)) {
          if (is.na(Indicator_02[m, 2])==FALSE) {
            # IDENTIFY OVERBOUGHT AND OVERSOLD SIGNALS
            if (Indicator_02[m, 2]>=OverBoughtSignal*(1-Range) & Indicator_02[m, 2]<=OverBoughtSignal*(1+Range)) {
              Indicator_02$SignalFlag[[m]]=1
            } else if (Indicator_02[m, 2]>=OverSoldSignal*(1-Range) & Indicator_02[m, 2]<=OverSoldSignal*(1+Range)) {
              Indicator_02$SignalFlag[[m]]=-1
              
            } else { 
              Indicator_02$SignalFlag[[m]]=0
              Indicator_02$OpportunityType[[m]]=''
            }
            
            SignalFlag=as.numeric(Indicator_02$SignalFlag[[m]])
            
            Indicator_02$PreviousFlag[[m]]=PreviousFlag
            
            
            # IDENTIFY THE OPPORTUNITY
            if (abs(SignalFlag-PreviousFlag)==2) {
              # IF TREND IS REVERSED THEN OPPORTUNITYFLAG = 1 ELSE 0
              Indicator_02$OpportunityFlag[[m]]=1
              
              
              if (SignalFlag>0) {Indicator_02$OpportunityType[[m]]='Overbought'}
              if (SignalFlag<0) {Indicator_02$OpportunityType[[m]]='Oversold'}
              
              
            } else {
              Indicator_02$OpportunityFlag[[m]]=0
            }
            
            
            #UPDATE PREVIOUS FLAG IF THE SIGNALS APPEAR
            if (!SignalFlag==0) {
              PreviousFlag=SignalFlag
            }
            
          } # if (is.na(Indicator_01[i, 2])==FALSE) 
        } # m LOOP
        
        
        
        #RESET DEFAULT FLAG
        if (exists("PreviousFlag")==FALSE) {
          rm(PreviousFlag)
        }
        
        Indicator_02$PostionEndDateLimit=as.Date(ifelse(Indicator_02$OpportunityFlag>0, Indicator_02$Date+EstimatedPositionLength, NA))
        Indicator_03=Indicator_02[Indicator_02$OpportunityFlag==1,]
        OpportunityCount=dim(Indicator_03)[1]
        
        Indicator_03$EstimatedReturn=NA
        Indicator_03$EstimatedValue=NA
        
        for (n in (1:OpportunityCount))
        {
          TradingPeriodStart=Indicator_03$Date[n]
          TradingPeriodEnd=Indicator_03$PostionEndDateLimit[n]
          TradingPeriod=Indicator_01[paste(TradingPeriodStart, TradingPeriodEnd, sep = "::")]
          
          #DEFINE START VALUE OF THE COMMODITY
          StartValue=TradingPeriod[1]$CommodityPrice
          e=length(TradingPeriod$CommodityPrice)
          
          
          if (Indicator_03$SignalFlag[n]==1) {
            AllReturns=(rep(StartValue, e)-TradingPeriod$CommodityPrice)/rep(StartValue, e)
          }
          
          if (Indicator_03$SignalFlag[n]==-1) {
            AllReturns=(TradingPeriod$CommodityPrice-rep(StartValue, e))/rep(StartValue, e)
          }
          
          
          # cut FUNCTION IS TRYING TO CLASS RETURNS INTO FOUR LEVELS
          # I TEND TO BELIEVE THAT WE ARE VERY LIKELY TO EXIT THE POSITION AT 3/4 HIGH LEVEL (ABOVE AVERAGE RETURN)
          # HOWEVER, THERE ARE CASES WHEN THERE IS NO 3RD CLASS EXISTING
          # GIVEN THESE CASES ARE RARE SO I DECIDE TO USE THE WHOLE SET OF RETURNS RATHER THAN JUST 3RD CLASS
          ReturnRanks=rank(as.numeric(AllReturns))
          if (sum(as.numeric(cut(ReturnRanks, 4))==3)==0)
          {
            Top3rdQtrReturns=AllReturns} else {
              Top3rdQtrReturns=AllReturns[as.numeric(cut(ReturnRanks, 4))==3]
            }
          EstimatedReturn=max(Top3rdQtrReturns)
          EstimatedValue=as.numeric(TradingPeriod$CommodityPrice[which(AllReturns==EstimatedReturn)][1])
          
          
          Indicator_03$EstimatedReturn[n]=EstimatedReturn
          Indicator_03$EstimatedValue[n]=EstimatedValue
        } #n LOOP
        
        Collection=data.frame(
          RSI_n=RSI_n,
          OverBoughtSignal=OverBoughtSignal,
          OverSoldSignal=OverSoldSignal,
          EstimatedPositionLength=EstimatedPositionLength,
          PositiveReturnChance=sum(Indicator_03$EstimatedReturn>0)/length(Indicator_03$EstimatedReturn),
          AccumReturn=prod(1+Indicator_03$EstimatedReturn),
          TotalOpportunity=length(Indicator_03$OpportunityType),
          OpportunityPerYear=length(Indicator_03$OpportunityType)/(TSEndYear-TSStartYear),
          # MeanReturn=mean(Indicator_03$EstimatedReturn),
          MedianReturn=median(Indicator_03$EstimatedReturn),
          MinReturn=min(Indicator_03$EstimatedReturn),
          # Percent_20=quantile(Indicator_03$EstimatedReturn, probs = 0.2),
          # Percent_50=quantile(Indicator_03$EstimatedReturn, probs = 0.5),
          # Percent_80=quantile(Indicator_03$EstimatedReturn, probs = 0.8),
          MaxReturn=max(Indicator_03$EstimatedReturn),
          BuyOpportunity=sum(Indicator_03$OpportunityType=='Oversold'), 
          BuyAccumReturn=prod(1+Indicator_03$EstimatedReturn[Indicator_03$OpportunityType=='Oversold']),
          BuyPositiveReturnChance=sum(Indicator_03$EstimatedReturn[Indicator_03$OpportunityType=='Oversold']>0)/length(Indicator_03$EstimatedReturn[Indicator_03$OpportunityType=='Oversold']),
          SellOpportunity=sum(Indicator_03$OpportunityType=='Overbought'), 
          SellAccumReturn=prod(1+Indicator_03$EstimatedReturn[Indicator_03$OpportunityType=='Overbought']),
          SellPositiveReturnChance=sum(Indicator_03$EstimatedReturn[Indicator_03$OpportunityType=='Overbought']>0)/length(Indicator_03$EstimatedReturn[Indicator_03$OpportunityType=='Overbought'])
        )
        
        CollectionSummary=rbind(CollectionSummary, Collection)  
      } #POSITION LENGTH COUNT LOOP
      
    } #OVERBOUGHT OVERSOLD THREHOLD COUNT LOOP
    
  } #RSI COUNT LOOP
  
  return(CollectionSummary)
}



################################################################# COMMODITY CHANNEL INDEX #################################################################

CCIProb=function(TS
                 , CCI_n=20
                 , CCI_c=0.015
                 , CCI_Threshold=180
                 , Range=0.05
                 , EstimatedPositionLength=30
                 , TargetReturn=0.03
                 , ContractType='both' )
{
  
  #FILL THE NON-LEADING NAs BY PREVIOUS PRICES
  TS=na.locf(TS, fromLast = TRUE)
  colnames(TS)='CommodityPrice'
  
  #CCI
  CCI_01=CCI(TS, n=CCI_n, c=CCI_c)
  #CCI_02=CCI_01[!is.na(CCI01), ]
  #Indicator_01=cbind(CCI_02)
  
  Indicator_01=merge(TS, CCI_01)
  #Indicator_02=Indicator_01[!is.na(Indicator_01), ]
  
  #SET THE DEFAULT PREVIOUS FLAG
  if (exists("PreviousFlag")==FALSE)
  {PreviousFlag=0}
  
  
  Indicator_02=data.frame(Indicator_01[is.na(Indicator_01[, 2])==FALSE, ])
  Indicator_02$Date=as.Date(row.names(Indicator_02))
  #row.names(Indicator_02[1,])
  
  xtsLength=dim(Indicator_02)[1]
  
  for (i in (1:xtsLength)) {
    if (is.na(Indicator_02[i, 2])==FALSE) {
      # IDENTIFY OVERBOUGHT AND OVERSOLD SIGNALS 
      if (Indicator_02[i, 2]>=CCI_Threshold*(1-Range) & Indicator_02[i, 2]<=CCI_Threshold*(1+Range)) {
        Indicator_02$SignalFlag[[i]]=1
      } else if (Indicator_02[i, 2]>=CCI_Threshold*(1+Range)*(-1) & Indicator_02[i, 2]<=CCI_Threshold*(1-Range)*(-1)) {
        Indicator_02$SignalFlag[[i]]=-1
        
      } else { 
        Indicator_02$SignalFlag[[i]]=0
        Indicator_02$OpportunityType[[i]]=''
      }
      
      SignalFlag=as.numeric(Indicator_02$SignalFlag[[i]])
      
      Indicator_02$PreviousFlag[[i]]=PreviousFlag
      
      
      # IDENTIFY THE OPPORTUNITY
      if (abs(SignalFlag-PreviousFlag)==2) {
        # IF TREND IS REVERSED THEN OPPORTUNITYFLAG = 1 ELSE 0
        Indicator_02$OpportunityFlag[[i]]=1
        
        
        if (SignalFlag>0) {Indicator_02$OpportunityType[[i]]='Overbought'}
        if (SignalFlag<0) {Indicator_02$OpportunityType[[i]]='Oversold'}
        
        
        # PRINT LOGS
        print(paste0(i, " - Date: ", row.names(Indicator_02[i,])))
        print(paste0("Opprtunity Type: ", Indicator_02$OpportunityType[[i]]))
        
      } else {
        Indicator_02$OpportunityFlag[[i]]=0
      }
      
      
      #UPDATE PREVIOUS FLAG IF THE SIGNALS APPEAR
      if (!SignalFlag==0) {
        PreviousFlag=SignalFlag
      }
      
      
    } # if (is.na(Indicator_01[i, 2])==FALSE) 
  } # i LOOP
  
  # IDENTIFY THE OPPORTUNITY DATE AND TRADING PERIOD
  Indicator_02$PostionEndDateLimit=as.Date(ifelse(Indicator_02$OpportunityFlag>0, Indicator_02$Date+EstimatedPositionLength, NA))
  Indicator_03=Indicator_02[Indicator_02$OpportunityFlag==1,]
  
  
  OpportunityCount=dim(Indicator_03)[1]
  
  BestPosition=data.frame()
  
  Indicator_03$BestPostionOut=NA
  Indicator_03$BestPositionOutDate=NA
  
  for (k in (1:OpportunityCount))
  {
    TradingPeriodStart=Indicator_03$Date[k]
    TradingPeriodEnd=Indicator_03$PostionEndDateLimit[k]
    TradingPeriod=Indicator_01[paste(TradingPeriodStart, TradingPeriodEnd, sep = "::")]
    
    #DEFINE START VALUE OF THE COMMODITY
    StartValue=TradingPeriod[1]$CommodityPrice
    e=length(TradingPeriod$CommodityPrice)
    
    
    if (Indicator_03$SignalFlag[k]==1) {
      BestPostionOut=min(TradingPeriod$CommodityPrice)[1]
      AboveWaterRatio=sum((TradingPeriod$CommodityPrice < rep(StartValue*(1-TargetReturn), e)))/e
    }
    
    if (Indicator_03$SignalFlag[k]==-1) {
      BestPostionOut=max(TradingPeriod$CommodityPrice)[1]
      AboveWaterRatio=sum((TradingPeriod$CommodityPrice > rep(StartValue*(1+TargetReturn), e)))/e
    }
    
    BestPositionOutDate=index(TradingPeriod[TradingPeriod$CommodityPrice==BestPostionOut])[1]
    
    Indicator_03$BestPostionOut[k]=BestPostionOut
    Indicator_03$BestPositionOutDate[k]=BestPositionOutDate
    Indicator_03$AboveWaterRatio[k]=AboveWaterRatio
  } #K LOOP
  
  #DEFINE CONTRACTS
  if (ContractType=='both') {Indicator_04=Indicator_03}
  if (ContractType=='buy') {Indicator_04=Indicator_03[Indicator_03$SignalFlag==-1, ]}
  if (ContractType=='sell') {Indicator_04=Indicator_03[Indicator_03$SignalFlag==1, ]}
  
  Indicator_04$BestPositionOutDate=as.Date(Indicator_04$BestPositionOutDate)
  Indicator_04$BestReturn=ifelse(Indicator_04$SignalFlag==1
                                 , (Indicator_04$CommodityPrice-Indicator_04$BestPostionOut)/Indicator_04$CommodityPrice
                                 , (Indicator_04$BestPostionOut-Indicator_04$CommodityPrice)/Indicator_04$CommodityPrice)
  
  Indicator_04$TradingLength=Indicator_04$BestPositionOutDate-Indicator_04$Date
  Indicator_04$YearRange=paste0(substr(Indicator_04$Date, 1, 3), "0S")
  
  OutputCollection=list(BestReturns=quantile(Indicator_04$BestReturn, probs = seq(0, 1, by= 0.1))
                        , TradingDaysDistribution=quantile(Indicator_04$TradingLength, probs = seq(0, 1, by= 0.1))
                        , AbovezWaterRatio=quantile(Indicator_04$AboveWaterRatio, probs = seq(0, 1, by= 0.1))
                        , RawDataset=Indicator_04)
  
  return(OutputCollection)
  
}




################################################################# RELATIVE STRENGTH INDEX #################################################################

RSIProb=function(TS
                 , RSI_n=14
                 , RSI_Threshold=c(30, 60)
                 , Range=0.05
                 , EstimatedPositionLength=30
                 , TargetReturn=0.03
                 , ContractType='both')
{
  
  #FILL THE NON-LEADING NAs BY PREVIOUS PRICES
  TS=na.locf(TS, fromLast = TRUE)
  colnames(TS)='CommodityPrice'
  
  RSI=RSI(TS, RSI_n)
  Indicator_01=merge(TS, RSI)
  
  Indicator_02=data.frame(Indicator_01[is.na(Indicator_01[, 2])==FALSE, ])
  Indicator_02$Date=as.Date(row.names(Indicator_02))
  
  #SET THE DEFAULT PREVIOUS FLAG
  if (exists("PreviousFlag")==FALSE)
  {PreviousFlag=0}
  
  xtsLength=dim(Indicator_02)[1]
  
  OverboughtSignal=max(RSI_Threshold)
  OverSoldSignal=min(RSI_Threshold)
  
  for (i in (1:xtsLength)) {
    if (is.na(Indicator_02[i, 2])==FALSE) {
      # IDENTIFY OVERBOUGHT AND OVERSOLD SIGNALS 
      if (Indicator_02[i, 2]>=OverboughtSignal*(1-Range) & Indicator_02[i, 2]<=OverboughtSignal*(1+Range)) {
        Indicator_02$SignalFlag[[i]]=1
      } else if (Indicator_02[i, 2]>=OverSoldSignal*(1-Range) & Indicator_02[i, 2]<=OverSoldSignal*(1+Range)) {
        Indicator_02$SignalFlag[[i]]=-1
        
      } else { 
        Indicator_02$SignalFlag[[i]]=0
        Indicator_02$OpportunityType[[i]]=''
      }
      
      SignalFlag=as.numeric(Indicator_02$SignalFlag[[i]])
      
      Indicator_02$PreviousFlag[[i]]=PreviousFlag
      
      
      # IDENTIFY THE OPPORTUNITY
      if (abs(SignalFlag-PreviousFlag)==2) {
        # IF TREND IS REVERSED THEN OPPORTUNITYFLAG = 1 ELSE 0
        Indicator_02$OpportunityFlag[[i]]=1
        
        
        if (SignalFlag>0) {Indicator_02$OpportunityType[[i]]='Overbought'}
        if (SignalFlag<0) {Indicator_02$OpportunityType[[i]]='Oversold'}
        
        
        # PRINT LOGS
        print(paste0(i, " - Date: ", row.names(Indicator_02[i,])))
        print(paste0("Opprtunity Type: ", Indicator_02$OpportunityType[[i]]))
        
      } else {
        Indicator_02$OpportunityFlag[[i]]=0
      }
      
      
      #UPDATE PREVIOUS FLAG IF THE SIGNALS APPEAR
      if (!SignalFlag==0) {
        PreviousFlag=SignalFlag
      }
      
      
    } # if (is.na(Indicator_01[i, 2])==FALSE) 
  } # i LOOP
  
  # IDENTIFY THE OPPORTUNITY DATE AND TRADING PERIOD
  Indicator_02$PostionEndDateLimit=as.Date(ifelse(Indicator_02$OpportunityFlag>0, Indicator_02$Date+EstimatedPositionLength, NA))
  Indicator_03=Indicator_02[Indicator_02$OpportunityFlag==1,]
  
  
  OpportunityCount=dim(Indicator_03)[1]
  
  BestPosition=data.frame()
  
  Indicator_03$BestPostionOut=NA
  Indicator_03$BestPositionOutDate=NA
  
  
  for (k in (1:OpportunityCount))
  {
    TradingPeriodStart=Indicator_03$Date[k]
    TradingPeriodEnd=Indicator_03$PostionEndDateLimit[k]
    TradingPeriod=Indicator_01[paste(TradingPeriodStart, TradingPeriodEnd, sep = "::")]
    
    #DEFINE START VALUE OF THE COMMODITY
    StartValue=TradingPeriod[1]$CommodityPrice
    e=length(TradingPeriod$CommodityPrice)
    
    
    if (Indicator_03$SignalFlag[k]==1) {
      BestPostionOut=min(TradingPeriod$CommodityPrice)[1]
      AboveWaterRatio=sum((TradingPeriod$CommodityPrice < rep(StartValue*(1-TargetReturn), e)))/e
    }
    
    if (Indicator_03$SignalFlag[k]==-1) {
      BestPostionOut=max(TradingPeriod$CommodityPrice)[1]
      AboveWaterRatio=sum((TradingPeriod$CommodityPrice > rep(StartValue*(1+TargetReturn), e)))/e
    }
    
    BestPositionOutDate=index(TradingPeriod[TradingPeriod$CommodityPrice==BestPostionOut])[1]
    
    Indicator_03$BestPostionOut[k]=BestPostionOut
    Indicator_03$BestPositionOutDate[k]=BestPositionOutDate
    Indicator_03$AboveWaterRatio[k]=AboveWaterRatio
  } #K LOOP
  
  #DEFINE CONTRACTS
  if (ContractType=='both') {Indicator_04=Indicator_03}
  if (ContractType=='buy') {Indicator_04=Indicator_03[Indicator_03$SignalFlag==-1, ]}
  if (ContractType=='sell') {Indicator_04=Indicator_03[Indicator_03$SignalFlag==1, ]}
  
  Indicator_04$BestPositionOutDate=as.Date(Indicator_04$BestPositionOutDate)
  Indicator_04$BestReturn=ifelse(Indicator_04$SignalFlag==1
                                 , (Indicator_04$CommodityPrice-Indicator_04$BestPostionOut)/Indicator_04$CommodityPrice
                                 , (Indicator_04$BestPostionOut-Indicator_04$CommodityPrice)/Indicator_04$CommodityPrice)
  
  Indicator_04$TradingLength=Indicator_04$BestPositionOutDate-Indicator_04$Date
  Indicator_04$YearRange=paste0(substr(Indicator_04$Date, 1, 3), "0S")
  
  OutputCollection=list(BestReturns=quantile(Indicator_04$BestReturn, probs = seq(0, 1, by= 0.1))
                        , TradingDaysDistribution=quantile(Indicator_04$TradingLength, probs = seq(0, 1, by= 0.1))
                        , AbovezWaterRatio=quantile(Indicator_04$AboveWaterRatio, probs = seq(0, 1, by= 0.1))
                        , RawDataset=Indicator_04)
  
  return(OutputCollection)
  
}


################################################################# MONEY FLOW INDEX #################################################################


MFIProb=function(TS
                 , MFI_n=14
                 , HLC = c('High', 'Low', 'Settle')
                 , Volume = 'Volume'
                 , MFI_Threshold=c(30, 60)
                 , Range=0.05
                 , EstimatedPositionLength=30
                 , TargetReturn=0.03
                 , ContractType='both' )
{
  
  # MFI_n=14
  # HLC = c('High', 'Low', 'Settle')
  # Volume = 'Volume'
  # MFI_Threshold=c(30, 80)
  # Range=0.1
  # EstimatedPositionLength=30
  # TargetReturn=0.03
  # ContractType='sell'
  # TS=Copper
  
  
  TS=TS[, colnames(TS) %in% c(HLC, Volume)]  
  
  #FILL THE NON-LEADING NAs BY PREVIOUS PRICES
  TS=na.locf(TS, fromLast = TRUE)
  
  if (is.null(TS$Close))
  {
    TS$Close=TS[, HLC[3]]
  }
  
  TS_HLC=HLC(TS)
  
  #MFI
  MFI_01=MFI(TS_HLC, TS[, colnames(TS) %in% Volume], n=EstimatedPositionLength)
  
  Indicator_01=merge(TS, MFI_01)
  #Indicator_02=Indicator_01[!is.na(Indicator_01), ]
  
  #Indicator_01=Indicator_01[Indicator_01$mfi>22 & Indicator_01$mfi<=32, ]
  
  #SET THE DEFAULT PREVIOUS FLAG
  if (exists('PreviousFlag')==FALSE)
  {PreviousFlag=0}
  
  
  Indicator_02=data.frame(Indicator_01[is.na(Indicator_01[, colnames(Indicator_01)=='mfi'])==FALSE, ])
  Indicator_02$Date=as.Date(row.names(Indicator_02))
  #row.names(Indicator_02[1,])
  
  xtsLength=dim(Indicator_02)[1]
  
  
  OverboughtSignal=max(MFI_Threshold)
  OverSoldSignal=min(MFI_Threshold)
  
  IndexNumber=which(colnames(Indicator_02)=='mfi')
  
  for (i in (1:xtsLength)) {
    if (is.na(Indicator_02[i, IndexNumber])==FALSE) {
      # IDENTIFY OVERBOUGHT AND OVERSOLD SIGNALS 
      if (Indicator_02[i, IndexNumber]>=OverboughtSignal*(1-Range) & Indicator_02[i, IndexNumber]<=OverboughtSignal*(1+Range)) {
        Indicator_02$SignalFlag[[i]]=1
      } else if (Indicator_02[i, IndexNumber]>=OverSoldSignal*(1-Range) & Indicator_02[i, IndexNumber]<=OverSoldSignal*(1+Range)) {
        Indicator_02$SignalFlag[[i]]=-1
        
      } else { 
        Indicator_02$SignalFlag[[i]]=0
        Indicator_02$OpportunityType[[i]]=''
      }
      
      SignalFlag=as.numeric(Indicator_02$SignalFlag[[i]])
      
      Indicator_02$PreviousFlag[[i]]=PreviousFlag
      
      
      # IDENTIFY THE OPPORTUNITY
      if (abs(SignalFlag-PreviousFlag)==2) {
        # IF TREND IS REVERSED THEN OPPORTUNITYFLAG = 1 ELSE 0
        Indicator_02$OpportunityFlag[[i]]=1
        
        
        if (SignalFlag>0) {Indicator_02$OpportunityType[[i]]='Overbought'}
        if (SignalFlag<0) {Indicator_02$OpportunityType[[i]]='Oversold'}
        
        
        # PRINT LOGS
        print(paste0(i, " - Date: ", row.names(Indicator_02[i,])))
        print(paste0("Opprtunity Type: ", Indicator_02$OpportunityType[[i]]))
        
      } else {
        Indicator_02$OpportunityFlag[[i]]=0
      }
      
      
      #UPDATE PREVIOUS FLAG IF THE SIGNALS APPEAR
      if (!SignalFlag==0) {
        PreviousFlag=SignalFlag
      }
      
      
    } # if (is.na(Indicator_01[i, 2])==FALSE) 
  } # i LOOP
  
  # IDENTIFY THE OPPORTUNITY DATE AND TRADING PERIOD
  Indicator_02$PostionEndDateLimit=as.Date(ifelse(Indicator_02$OpportunityFlag>0, Indicator_02$Date+EstimatedPositionLength, NA))
  Indicator_03=Indicator_02[Indicator_02$OpportunityFlag==1,]
  
  
  OpportunityCount=dim(Indicator_03)[1]
  
  BestPosition=data.frame()
  
  Indicator_03$BestPostionOut=NA
  Indicator_03$BestPositionOutDate=NA
  
  
  for (k in (1:OpportunityCount))
  {
    TradingPeriodStart=Indicator_03$Date[k]
    TradingPeriodEnd=Indicator_03$PostionEndDateLimit[k]
    TradingPeriod=Indicator_01[paste(TradingPeriodStart, TradingPeriodEnd, sep = "::")]
    
    #DEFINE START VALUE OF THE COMMODITY
    StartValue=TradingPeriod[1]$Close
    e=length(TradingPeriod$Close)
    
    
    if (Indicator_03$SignalFlag[k]==1) {
      BestPostionOut=min(TradingPeriod$Close)[1]
      AboveWaterRatio=sum((TradingPeriod$Close < rep(StartValue*(1-TargetReturn), e)))/e
    }
    
    if (Indicator_03$SignalFlag[k]==-1) {
      BestPostionOut=max(TradingPeriod$Close)[1]
      AboveWaterRatio=sum((TradingPeriod$Close > rep(StartValue*(1+TargetReturn), e)))/e
    }
    
    BestPositionOutDate=index(TradingPeriod[TradingPeriod$Close==BestPostionOut])[1]
    
    Indicator_03$BestPostionOut[k]=BestPostionOut
    Indicator_03$BestPositionOutDate[k]=BestPositionOutDate
    Indicator_03$AboveWaterRatio[k]=AboveWaterRatio
  } #K LOOP
  
  #DEFINE CONTRACTS
  if (ContractType=='both') {Indicator_04=Indicator_03}
  if (ContractType=='buy') {Indicator_04=Indicator_03[Indicator_03$SignalFlag==-1, ]}
  if (ContractType=='sell') {Indicator_04=Indicator_03[Indicator_03$SignalFlag==1, ]}
  
  Indicator_04$BestPositionOutDate=as.Date(Indicator_04$BestPositionOutDate)
  Indicator_04$BestReturn=ifelse(Indicator_04$SignalFlag==1
                                 , (Indicator_04$Close-Indicator_04$BestPostionOut)/Indicator_04$Close
                                 , (Indicator_04$BestPostionOut-Indicator_04$Close)/Indicator_04$Close)
  
  Indicator_04$TradingLength=Indicator_04$BestPositionOutDate-Indicator_04$Date
  Indicator_04$YearRange=paste0(substr(Indicator_04$Date, 1, 3), "0S")
  
  OutputCollection=list(BestReturns=quantile(Indicator_04$BestReturn, probs = seq(0, 1, by= 0.1))
                        , TradingDaysDistribution=quantile(Indicator_04$TradingLength, probs = seq(0, 1, by= 0.1))
                        , AbovezWaterRatio=quantile(Indicator_04$AboveWaterRatio, probs = seq(0, 1, by= 0.1))
                        , RawDataset=Indicator_04)
  
  return(OutputCollection)
}

