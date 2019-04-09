require(TTR)
require(tseries)
require(Quandl)
require(xts)
require(quantmod)


#SILVER PRICE FULL HISTORY
SLVHist=Quandl("LBMA/SILVER", api_key="GixSX89oiCWDRyS3B-Dy", type = 'xts')
SLVHist_01=SLVHist[, 1]

CCI=TRUE
CCI_n=14
CCI_c=0.015
CCI_thold=1
CCI_Threshold=200

EsitmatedPositionLength=30

TS=SLVHist_01



#FILL THE NON-LEADING NAs BY PREVIOUS PRICES
TS=na.locf(TS, fromLast = TRUE)

#CCI
CCI_01=CCI(TS, n=CCI_n, c=CCI_c)
#CCI_02=CCI_01[!is.na(CCI01), ]
#Indicator_01=cbind(CCI_02)

Indicator_01=merge(TS, CCI_01)
#Indicator_02=Indicator_01[!is.na(Indicator_01), ]


SignalCollections=data.frame()

xtsLength=dim(Indicator_01)[1]


for (i in (1:xtsLength)) {
  if (is.na(Indicator_01[i, 2])==FALSE) {
    if (abs(Indicator_01[i, 2])>CCI_Threshold) {
        SignalDate=index(Indicator_01[i, 2])
        SignalValue=as.numeric(Indicator_01[i, 2])
        CommodityValue=as.numeric(Indicator_01[i, 1])
        
        #IDENTIFY THE BIGGEST VARIANCE WITHIN 'EsitmatedPositionLength' DAYS AND WHICH DAY
          
            if (i<(xtsLength-EsitmatedPositionLength)) {
              #SUBSET THE XTS TO ONLY THE TRADING PERIOD - 'EsitmatedPositionLength' DAYS FROM THE SIGNAL DAYS
              TradingPeriodValues=Indicator_01[paste(SignalDate, SignalDate+EsitmatedPositionLength, sep='::')][, 1]
              print(paste0("SignalDate: ", SignalDate))
              
              #IF THE INDICATOR>0 THEN OVERSOLD OTHERWISE OVERBOUGHT
                if (SignalValue>0) {
                    ExtremeValueInTradingPeriod=min(TradingPeriodValues)
                    ExtremeValueDate=index(TradingPeriodValues[which(TradingPeriodValues==ExtremeValueInTradingPeriod)])
                    SignalType='Over-bought'
                    BestReturn=(CommodityValue-ExtremeValueInTradingPeriod)/CommodityValue
                } else {
                    ExtremeValueInTradingPeriod=max(TradingPeriodValues)
                    ExtremeValueDate=index(TradingPeriodValues[which(TradingPeriodValues==ExtremeValueInTradingPeriod)])
                    SignalType='Over-sold'
                    BestReturn=(ExtremeValueInTradingPeriod-CommodityValue)/CommodityValue
                } # if (SignalValue>0)
              PositionLength=ExtremeValueDate-SignalDate
              print(paste0("ExtremeValueDate: ", ExtremeValueDate))
            } # if (i<(xtsLength-EsitmatedPositionLength))
          SignalOutput=data.frame(date=SignalDate
                                  , SignalValue=SignalValue
                                  , CommodityValue=CommodityValue
                                  , ExtremeValueDate=ExtremeValueDate
                                  , ExtremeValue=ExtremeValueInTradingPeriod
                                  , SignalType=SignalType
                                  , PositionLength=PositionLength
                                  , BestReturn=BestReturn)
                          
          SignalCollections=rbind(SignalCollections, SignalOutput)
      
        } # if (asb(Indicator_01[i, ])>CCI_Threshold)
      } # if (is.na(Indicator_01[i, 2])==FALSE) 
    } # i LOOP

# REMOVE THE OPPORTUNITY WHICH ARE EsitmatedPositionLength' DAYS LESS THAN THE PREVIOUS SIGNAL DATE


