require(TTR)
require(tseries)
require(Quandl)
require(xts)
require(quantmod)


#SILVER PRICE FULL HISTORY
slv_hist=Quandl("LBMA/SILVER")
slv_hist$Date=as.Date(slv_hist$Date, '%Y-%m-%d')

SLVHist_01=slv_hist[,1:2]
SLVHist_02=xts(SLVHist_01[,-1], order.by=SLVHist_01[,1])


#US DOLLAR INDEX
us_index=Quandl("CHRIS/ICE_DX1", api_key="GixSX89oiCWDRyS3B-Dy")
us_index$Date=as.Date(us_index$Date, '%Y-%m-%d')
us_index=xts(us_index[,-1], order.by = us_index[,1])
us_index_hl=us_index[,2:3]

TS=SLVHist_02
US=us_index

CCI=TRUE
CCI_n=14
CCI_c=0.015
CCI_thold=1

US_SAR=TRUE
US_SAR_ACCR=0.02
US_SAR_MAX=0.2
US_SAR_thold=1

TradeOpenDate=3
TradeCloseDate=3

TradePrinciples=function(TS,
                         TradeOpenDate=3,
                         TradeCloseDate=3,
                         US,
                         CCI=TRUE, 
                         CCI_n=14,
                         CCI_c=0.015,
                         CCI_thold=1,
                         US_SAR=TRUE,
                         US_SAR_ACCR=0.02,
                         US_SAR_MAX=0.2,
                         US_SAR_thold=1

                          )
  
{
      #FILL THE NON-LEADING NAs BY PREVIOUS PRICES
      TS=na.locf(TS, fromLast = TRUE)
      Valid_Signals_Expected=sum(CCI, US_SAR)
  #SILVER CCI  
  # if (CCI==TRUE)
  # {
      #CCI
      CCI01=CCI(TS, n=CCI_n, c=CCI_c)
      #USE THRESHOLD TO CONTROL CCO
      CCI01$CCI_Flag=ifelse(CCI01$cci>=100*CCI_thold, 1,
                                 ifelse((CCI01$cci<100*CCI_thold & CCI01$cci>-100*CCI_thold), 0,
                                        ifelse(CCI01$cci<=-100*CCI_thold, -1, 0)))
      CCI02=CCI01[is.na(CCI01$cci)==FALSE]
      names(CCI02)[1]="CCI"
  
    # }#CCI ENDS
  
  # if(US_SAR==TRUE)
  # {
    SAR01=merge(US=US[,4], US_SAR_SMA=SMA(SAR(US[, 2:3], accel = c(0.02, 0.2)),5))
    names(SAR01)=c("US_Index", "US_SAR")
    SAR01$SAR_Flag=ifelse(SAR01$US_SAR>SAR01$US_Index*US_SAR_thold, 1, -1)
  # }
      
    Indicator01=merge(TS, CCI02, SAR01)
    Indicator01=Indicator01[!is.na(Indicator01$US_SAR) & !is.na(Indicator01$TS), ]
 
  
    InitialInvestment=100
    capital=numeric()
    capital[0]=InitialInvestment
    
    #DEFINE THE DATE VECTORS TO COLLECT DATE RANGES      
    m=1
    StartDate=date()
    EndDate=date()
  
  
    #DEFINE THE LENGTH OF TS
    xtsLength=dim(Indicator01)[1]
    
    #FIRST LOOP DEFINE THE START DATE AND END DATE FOR EACH TRAINING PERIOD
    for (k in (1:xtsLength))
    {
      OverallPosition=0
      OverallReturnedCapital=0
      CloseBalance=0
      
      if(m==1)
      {
        #ALWAYS STARTS FROM DAY 1
        StartDate[m]=index(Indicator01)[1]
      }
      
        #IDENTIFY HOW MANY INDICATORS WE ARE GOING TO USE
          print(k)
          Indicator01$Signal_Sum[k]=sum(Indicator01$CCI_Flag[k], Indicator01$SAR_Flag[k])
          Indicator01$Flag[k]=ifelse(Indicator01$Signal_Sum[k]-Valid_Signals_Expected==0, 1, 
                                     ifelse(abs(Indicator01$Signal_Sum[k]-Valid_Signals_Expected)==Valid_Signals_Expected*2, -1, 0))
          #SAVE CURRENT FLAG TO COMPARE WITH SIGNAL FLAG TO IDENTIFY A REVERSED DAY
          
            CurrentFlag=as.numeric(Indicator01$Flag[k])
          
          if (exists("SignalFlag")==FALSE)
          {SignalFlag=0}
            
          if (abs(CurrentFlag-SignalFlag)==2)
          {
            ReversedDate=index(Indicator01[k])
            print(paste("ReversedDate: ", ReversedDate))
            #Indicator01$ReverseDate[k]=ReverseDate
            
            
            # ONCE WE KNOW WHERE THE TREND GETS REVERSED THEN WE SAVE THE ENDDATE
            # IF THE REVERSED DATE IS THE FIRST DATE OF THIS PERIOD THEN END DATE WILL BE EARLIER THAN START DATE
            # SO WILL NEED TO MAKE SURE THE END DATE IS NOT GOING TO BE EARLIER THAN START DATE
            EndDate[m]=ifelse(as.numeric(ReversedDate-1)<StartDate[m], StartDate[m], ReversedDate-1)
            
            #IF WE NOT YET REACH THE END OF LOOP THEN WE WILL NEED TO UPDATE THE NEXT START DATE
            if (k<dim(Indicator01)[1])
            {
              m=m+1
              StartDate[m]=ReversedDate
            }
          }
          
          #CHECK IF THE SIGNAL FLAG NEEDS TO BE UPDATED
          if (!Indicator01$Flag[k]==0)
          {
            SignalFlag=as.numeric(Indicator01$Flag[k])
          }
            
    }
    StartDate=StartDate[-length(StartDate)]
    StartDate=as.Date(as.numeric(StartDate), origin='1970-01-01')
    EndDate=as.Date(as.numeric(EndDate), origin='1970-01-01')  

    #SUBSET TS BY START AND END WHICH ARE DEFINED BY REVERSE DATES
    Indicator02=list()
    for (h in (1:length(StartDate)))
    #for (h in (1:1))
    {
      Indicator02[[h]]=Indicator01[paste(StartDate[h], EndDate[h], sep="::")]
      Indicator01[1]
      
      
      #START FROM HERE
      #IDENTIFY THE OPEN POSITION DATE
      if (h+TradeOpenDate<xtsLength)
      {
        TradePostionOpenDate=index(Indicator02[[h]])[1+TradeOpenDate]
        print(paste("TradePostionOpenDate: ", TradePostionOpenDate))
        
      }
      
      #DEFINE TYPE OF TRADE
      if (Indicator01$Flag>0)
      {
        TradePositionType="sell"
      }
      
      if (Indicator01$Flag<0)
      {
        TradePositionType="buy"
      }
      
      PositionQuantity=InitialInvestment/as.numeric(Indicator02[[h]][index(Indicator02[[h]])==TradePostionOpenDate]$TS)
      
      
      #CLOSED POSITIONS NEED TO BE RETHINK THROUGH
      if (h+1<=xtsLength)
      {
          if (dim(Indicator02[[h]])[1]>=)
            dim(Indicator02[[h]])
        TradePositionCloseDate=Indicator02[[h+1]]
      }
    }   
    
    Indicator02[[3]]
    
    
    
    
}
