library(TTR)
library(tseries)
library(Quandl)
library(caret)
library(xts)
library(quantmod)


#SILVER PRICE FULL HISTORY
slv_hist=Quandl("LBMA/SILVER")
slv_hist$Date=as.Date(slv_hist$Date, '%Y-%m-%d')

SLVHist_01=slv_hist[,1:2]
SLVHist_02=xts(SLVHist_01[,-1], order.by=SLVHist_01[,1])

#FILL THE NON-LEADING NAs BY PREVIOUS PRICES
SLVHist_02=na.locf(SLVHist_02, fromLast = TRUE)


##STRATEGY 1#######
#1. CCI X(14) DAYS RANGE
#2. FIND THE CCI INDEX WHERE IT EITHER GO BEYOUD 100  OR GO BELOW -100
#3. START COUNTING NUMBER OF DAYS WHEN IT REACH THE PEAK (BEST MOMENT TO CREATE A POISTION)
#4. TAKE THE MEDIUM DAYS COUNT AND USE IT IN TRADING STATEGY. WHEN CCI IS BELOW ZERO THEN BUY;OTHERWISE SELL

SLVHist_02_CCI=CCI(SLVHist_02, n=14, c=0.015)
SLVHist_02_CCI$flag=ifelse(SLVHist_02_CCI$cci>=100, 1,
                      ifelse((SLVHist_02_CCI$cci<100 & SLVHist_02_CCI$cci>-100), 0,
                        ifelse(SLVHist_02_CCI$cci<=-100, -1, 0)))



#REMOVE NA
CCI01=SLVHist_02_CCI[is.na(SLVHist_02_CCI$cci)==FALSE]

CCI02=CCI01

#DEFINE THE DATE VECTORS TO COLLECT DATE RANGES      
m=1
StartDate=date()
EndDate=date()


    for (i in (1:dim(CCI02)[1]))
      {
            if(m==1)
            {
              #ALWAYS STARTS FROM DAY 1
              StartDate[m]=index(CCI02)[1]
            }
            #print(paste("i: ", i))
            #SAVE CURRENT FLAG TO COMPARE WITH SIGNAL FLAG TO IDENTIFY A REVERSED DAY
            CurrentFlag=as.numeric(CCI02$flag[i])
            #SET THE DEFAULT SIGNAL DATE
            # if (is.null(SignalFlag)==TRUE)
            if (exists("SignalFlag")==FALSE)
              {SignalFlag=0}
            #COMPARE THE CURRENT FLAG WITH SAVED THE SIGNALFLAG
            #IF THE ABSOLUTE VALUE IS 2 (EITHER -1 VS 1 OR 1 VS -1) THEN IT MEANS THE TREND IS REVERSED
                if (abs(CurrentFlag-SignalFlag)==2)
                {
                  ReversedDate=index(CCI02[i])
                  print(paste("ReversedDate: ", ReversedDate))
                    # ONCE WE KNOW WHERE THE TREND GETS REVERSED THEN WE SAVE THE ENDDATE
                  EndDate[m]=ReversedDate-1
                 
                  #IF WE NOT YET REACH THE END OF LOOP THEN WE WILL NEED TO UPDATE THE NEXT START DATE
                    if (i<dim(CCI02)[1])
                      {
                      m=m+1
                      StartDate[m]=ReversedDate
                    }
                }
            #CHECK IF THE SIGNAL FLAG NEEDS TO BE UPDATED
            if (!CCI02$flag[i]==0)
              {
              SignalFlag=as.numeric(CCI02$flag[i])
              }
        }
StartDate=StartDate[-length(StartDate)]
StartDate=as.Date(as.numeric(StartDate), origin='1970-01-01')
EndDate=as.Date(as.numeric(EndDate), origin='1970-01-01')  


#SUBSET CCI 100 PLUS BY START AND END DATES
CCI03=list()
for (h in (1:length(StartDate)))
{
  CCI03[[h]]=CCI02[paste(StartDate[h], EndDate[h], sep="::")]

}

#NUMBER OF DAYS AN OSCILLATOR CAN USUALLY LAST
#FRIST OF ALL FILTER OUT THE NON-SIGNAL DAYS WHICH FLAG=0
CCI04=lapply(CCI03, function(x) subset(x, !x$flag==0))
#REMOVE BLANK LIST
CCI04=CCI04[lapply(CCI04, length)>1]


#CALCULATE THE DATE DIFFERENCE WHERE THE OSCILLATOR LAST
CCI05=sapply(CCI04, function(x) (as.numeric(max(index(x$flag))-min(index(x$flag)))))

#CALCULATE WHICH DAY REACH THE HIGHEST INDEX
CCI06=sapply(CCI04, function(x) (which.max(abs(x))))

#HOW MANY DAYS DOES IT TAKE TO REACH THE HIGHEST PRICE
CCI07=numeric()
for (j in (1:length(CCI04)))
  {
    #THE DATE DIFFERENCE BETWEEN HIGHEST VALUE DAY VS THE FRIST DAY
  CCI07[j]=as.numeric(index(CCI04[[j]][CCI06[j]])-index(CCI04[[j]][1]))
  }


#  OPEN STRATEGY:WHEN CCI MOVE BELOW -100 OR ABOVE 100 THEN INVEST 25% on day1 , 25% on day 3, 25% on day 7
# CLOSE STRATEGY:SELL 25% ON DAY 1, DAY 3 50%, DAY7 THE REST 25%. IF ONLY LAST FOR ONE DAY SELL ALL THE REST WHEN GO OVER -100 OR UNDER 100 


# XTS FOR INVESTIGATION

SLVHist_03=merge(SLVHist_02, SLVHist_02_CCI)
SLVHist_03$BuyPosition=NA
SLVHist_03$SellPosition=NA

InitialInvestment=100
capital=numeric()
capital[0]=InitialInvestment
length(CCI03)
for (k in (1:2))
  {
  OverallPosition=0
  OverallReturnedCapital=0
  CloseBalance=0
    #TOTAL NUMBER OF DAYS IN THIS ROUND OF MOMENTUM
      TradePositionOpenDayCount=max(index(CCI04[[k]]))-min(index(CCI04[[k]]))+1
    #DEFINE TYPE OF TRADE
      TradePositionType=ifelse(CCI04[[k]][1]$cci>0, "sell", "buy")
  #IDENTIFY THE FIRST DAY
      TradePositionOpenDate1=index(CCI04[[k]][1])
      #UPDATE INVESTIGATION LIST
          if (TradePositionType=="buy")
            {
              SLVHist_03$BuyPosition=ifelse(index(SLVHist_03)==TradePositionOpenDate1, 
                                            "TradePositionOpenDate1", 
                                            SLVHist_03$BuyPosition)
            }
          else
            {
              SLVHist_03$SellPosition=ifelse(index(SLVHist_03)==TradePositionOpenDate1, 
                                            "TradePositionOpenDate1", 
                                            SLVHist_03$SellPosition)
            }
      print(paste("TradePositionOpenDate1: ", TradePositionOpenDate1))
      
      ############## this might be the problem when there is a sell position
      PositionQuantity1=InitialInvestment*0.25/SLVHist_02[index(SLVHist_02)==TradePositionOpenDate1]
      OverallPosition=as.numeric(PositionQuantity1)+OverallPosition
        
    if (TradePositionOpenDayCount>=3) 
      {
      #FIND THE POSITION OF THE FIRST TRADE THEN DERIVE THE SECOND TRADE
      TradePositionOpenDate2=index(SLVHist_02[which(index(SLVHist_02)==TradePositionOpenDate1)+2])
      #UPDATE INVESTIGATION LIST
      if (TradePositionType=="buy")
      {
        SLVHist_03$BuyPosition=ifelse(index(SLVHist_03)==TradePositionOpenDate2, 
                                      "TradePositionOpenDate2", 
                                      SLVHist_03$BuyPosition)
      }
      else
      {
        SLVHist_03$SellPosition=ifelse(index(SLVHist_03)==TradePositionOpenDate2, 
                                       "TradePositionOpenDate2", 
                                       SLVHist_03$SellPosition)
      }
      print(paste("TradePositionOpenDate2: ", TradePositionOpenDate2))
      PositionQuantity2=InitialInvestment*0.5/SLVHist_02[index(SLVHist_02)==TradePositionOpenDate2]
      OverallPosition=as.numeric(PositionQuantity2)+OverallPosition
    } 
      if (TradePositionOpenDayCount>=7) 
      {
        TradePositionOpenDate3=index(SLVHist_02[which(index(SLVHist_02)==TradePositionOpenDate1)+6])
        #UPDATE INVESTIGATION LIST
        if (TradePositionType=="buy")
        {
          SLVHist_03$BuyPosition=ifelse(index(SLVHist_03)==TradePositionOpenDate3, 
                                        "TradePositionOpenDate3", 
                                        SLVHist_03$BuyPosition)
        }
        else
        {
          SLVHist_03$SellPosition=ifelse(index(SLVHist_03)==TradePositionOpenDate3, 
                                         "TradePositionOpenDate3", 
                                         SLVHist_03$SellPosition)
        }
        print(paste("TradePositionOpenDate3: ", TradePositionOpenDate3))
        PositionQuantity3=InitialInvestment*0.25/SLVHist_02[index(SLVHist_02)==TradePositionOpenDate3]
        OverallPosition=as.numeric(PositionQuantity3)+OverallPosition
      } 
    
#CLOSED POSITIONS

        TradePositionCloseDayCount=max(index(CCI04[[k+1]]))-min(index(CCI04[[k+1]]))
        TradePositionCloseDate1=index(CCI04[[k+1]][1])
          print(paste("TradePositionCloseDate1: ", TradePositionCloseDate1))
              if (TradePositionType=='buy')
                {
                  ClosePositionBalanceDate1=as.numeric(SLVHist_02[index(SLVHist_02)==TradePositionCloseDate1])*PositionQuantity1
                  SLVHist_03$BuyPosition=ifelse(index(SLVHist_03)==TradePositionCloseDate1, 
                                                "TradePositionCloseDate1", 
                                                SLVHist_03$BuyPosition)
                }
          
            # WHEN THIS IS A SELL POSITION PROFIT=INITIAL INVESTMENT - CLOSED POSITION PRICE
            # OVER BALANCE AFTER CLOSING POSITION=INITIAL INVESTMENT + (INITIAL INVESTMENT - CLOSED POSITION PRICE)
            # OVER BALANCE AFTER CLOSING POSITION=INITIAL INVESTMENT * 2 - CLOSED POSITION PRICE          
              if (TradePositionType=='sell')
                {
                  ClosePositionBalanceDate1=InitialInvestment*0.25*2-(as.numeric(SLVHist_02[index(SLVHist_02)==TradePositionCloseDate1])*PositionQuantity1)
                  SLVHist_03$SellPosition=ifelse(index(SLVHist_03)==TradePositionCloseDate1, 
                                                "TradePositionCloseDate1", 
                                                SLVHist_03$SellPosition)
                }
        OverallReturnedCapital=OverallReturnedCapital+as.numeric(ClosePositionBalanceDate1)
    
    # ONLY EXECUTE IF THE SECOND POSITION EXISTS
    if (exists("PositionQuantity2"))
    {
        #IF THE MOMENTUM LAST MORE THAN 2 DAYS THEN CLOSE THE SECOND POSITION ON DAY 3
        TradePositionCloseDate2=index(SLVHist_02[which(index(SLVHist_02)==TradePositionCloseDate1)+2])
          print(paste("TradePositionCloseDate2: ", TradePositionCloseDate2))
          if (TradePositionCloseDayCount>3)
          {
            #print("Rountine 1")
            #CLOSE THE SECOND POSITION
                if (TradePositionType=='buy')
                  {
                    ClosePositionBalanceDate2=as.numeric(SLVHist_02[index(SLVHist_02)==TradePositionCloseDate2])*PositionQuantity2
                    SLVHist_03$BuyPosition=ifelse(index(SLVHist_03)==TradePositionCloseDate2, 
                                                "TradePositionCloseDate2", 
                                                SLVHist_03$BuyPosition)
                    }
                if (TradePositionType=='sell')
                  {
                    ClosePositionBalanceDate2=InitialInvestment*0.5*2-(as.numeric(SLVHist_02[index(SLVHist_02)==TradePositionCloseDate2])*PositionQuantity2)
                    SLVHist_03$SellPosition=ifelse(index(SLVHist_03)==TradePositionCloseDate2, 
                                                  "TradePositionCloseDate2", 
                                                  SLVHist_03$SellPosition)
                  }
            OverallReturnedCapital=OverallReturnedCapital+as.numeric(ClosePositionBalanceDate2)
           
            # ONLY EXECUTE IF THE THIRD POSITION EXISTS 
            if (exists("PositionQuantity3"))
              {
                TradePositionCloseDate3=index(SLVHist_02[which(index(SLVHist_02)==TradePositionCloseDate1)+6])
                    print(paste("TradePositionCloseDate3: ", TradePositionCloseDate3))
                #CLOSE THE THRID POSITION
                    if (TradePositionType=='buy')
                        {
                          ClosePositionBalanceDate3=as.numeric(SLVHist_02[index(SLVHist_02)==TradePositionCloseDate3])*PositionQuantity3
                          SLVHist_03$BuyPosition=ifelse(index(SLVHist_03)==TradePositionCloseDate3, 
                                                      "TradePositionCloseDate3", 
                                                      SLVHist_03$BuyPosition)
                        }
                    if (TradePositionType=='sell')
                        {
                            ClosePositionBalanceDate3=InitialInvestment*0.25*2-(as.numeric(SLVHist_02[index(SLVHist_02)==TradePositionCloseDate3])*PositionQuantity3)
                            SLVHist_03$SellPosition=ifelse(index(SLVHist_03)==TradePositionCloseDate3, 
                                                          "TradePositionCloseDate3", 
                                                          SLVHist_03$SellPosition)
                        }
                OverallReturnedCapital=OverallReturnedCapital+as.numeric(ClosePositionBalanceDate3)
              }
          }
        #OTHEREISE, WE SHOULD CLOSE ALL POSITION ON DAY 3
          else
            {
              #print("Rountine 2")
              #CLOSE SECOND AND THIRD POSITION TOGETHER
                if (TradePositionType=='buy')
                    {ClosePositionBalanceDate2=as.numeric(SLVHist_02[index(SLVHist_02)==TradePositionCloseDate2])*(OverallPosition-PositionQuantity1)}
                if (TradePositionType=='sell')
                    {ClosePositionBalanceDate2=InitialInvestment*0.75*2-(as.numeric(SLVHist_02[index(SLVHist_02)==TradePositionCloseDate2])*(OverallPosition-PositionQuantity1))}

              OverallReturnedCapital=OverallReturnedCapital+as.numeric(ClosePositionBalanceDate2)
            }
    } 
      
      
      
      InitialInvestment=OverallReturnedCapital
      capital[k]=InitialInvestment
      print(k)
      print(paste("InitialInvestment: ", InitialInvestment))
      
      # if (exists("PositionQuantity1")) {rm(PositionQuantity1)}
      # if (exists("PositionQuantity2")) {rm(PositionQuantity2)}
      # if (exists("PositionQuantity3")) {rm(PositionQuantity3)}
      
}



test=data.frame(date=index(SLVHist_03[!is.na(SLVHist_03$BuyPosition), ]), SLVHist_03[!is.na(SLVHist_03$BuyPosition), ])
write.csv(test, "/Users/mingzhang/desktop/test.csv")

capital[1:100]
