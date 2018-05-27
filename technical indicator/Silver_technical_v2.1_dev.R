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

CCI_100_Plus=which(SLVHist_02_CCI>100)
CCI_100_Plus_Date=index(SLVHist_02_CCI[CCI_100_Plus, ])
CCI_100_Plus_Date_Diff=diff(CCI_100_Plus_Date)

#IDENTIFY WHICH DATES WHICH GAP START AND END
#GAP=
CCI_100_PLus_Gap=which(CCI_100_Plus_Date_Diff>10)
length(CCI_100_PLus_Gap)
#START: ADD 1 AT THE FRONT AND SUBTRACT THE LAST ONE IN THE END
#END: GAP-1
CCI_100_Plus_Start_Index=c(1, CCI_100_PLus_Gap[-length(CCI_100_PLus_Gap)])
CCI_100_Plus_End_Index=CCI_100_PLus_Gap-1

CCI_100_Plus_Start_Date=CCI_100_Plus_Date[CCI_100_Plus_Start_Index]
CCI_100_Plus_End_Date=CCI_100_Plus_Date[CCI_100_Plus_End_Index]

test=lapply(SLVHist_02_CCI, function(x)())










