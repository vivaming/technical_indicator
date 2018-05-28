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

CCI02[1:24]
      #for (i in (1:length(CCI02)))
      for (i in (1:24))
          {
            print(paste("i: ", i))
            #SAVE CURRENT FLAG FOR LATER USE
            CurrentFlag=as.numeric(CCI02$flag[i])
            if (is.null(SignalFlag)==TRUE)
              {SignalFlag=0}
            #IF CURRENT FLAG IS A SIGNAL FLAG (1/-1)
                if (!CCI02$flag[i]==0)
                  {
                    #THEN RECORD THE CURRENT DATE AS THE NEXT START DATE
                    StartDate=index(CCI02[i])
                    #RECORD THE SIGNAL FLAG FOR LATER USE
                    print(index(CCI02$flag[i]))
                    print(SignalFlag)
                    #print(paste(StartDate, SignalFlag, sep=" - "))
                }
            
            #COMPARE THE CURRENT FLAG WITH SAVED THE SIGNALFLAG
                if (abs(CurrentFlag-SignalFlag)==2)
                {
                  PauseDate=index(CCI02[i])
                  print(paste("PauseDate: ", PauseDate))
                }
            
            #CHECK IF THE SIGNAL FLAG NEEDS TO BE UPDATED
            if (!CCI02$flag[i]==0)
              {
              SignalFlag=as.numeric(CCI02$flag[i])
              }
          }

# CCI_100_Plus=which(SLVHist_02_CCI>100)
# CCI_100_Plus_Date=index(SLVHist_02_CCI[CCI_100_Plus, ])
# CCI_100_Plus_Date_Diff=diff(CCI_100_Plus_Date)


#IDENTIFY WHICH DATES WHICH GAP START AND END
#GAP=
# CCI_100_PLus_Gap=which(CCI_100_Plus_Date_Diff>10)

#START: ADD 1 AT THE FRONT AND SUBTRACT THE LAST ONE IN THE END
#END: GAP-1
# CCI_100_Plus_Start_Index=c(1, CCI_100_PLus_Gap[-length(CCI_100_PLus_Gap)])
# CCI_100_Plus_End_Index=CCI_100_PLus_Gap

# CCI_100_Plus_Start_Date=CCI_100_Plus_Date[CCI_100_Plus_Start_Index]
# CCI_100_Plus_End_Date=CCI_100_Plus_Date[CCI_100_Plus_End_Index]

# d_CCI100PlusStartEndDates=data.frame(start=CCI_100_Plus_Start_Date, end=CCI_100_Plus_End_Date)

#SUBSET CCI 100 PLUS BY START AND END DATES
l_CCI100PlusXts=list()
for (i in (1:length(CCI_100_Plus_Start_Date)))
{
  l_CCI100PlusXts[[i]]=SLVHist_02_CCI[paste(CCI_100_Plus_Start_Date[i], CCI_100_Plus_End_Date[i], sep="::")]
}
