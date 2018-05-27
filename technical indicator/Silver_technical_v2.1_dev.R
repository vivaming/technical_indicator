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

sapply(l_CCI100PlusXts, function(x) which(x==max(x)))
l_CCI100PlusXts[[2]]

SLVHist_02_CCI[1:100]

# CCI_100_Plus_Date_Diff[1:20]
# CCI_100_PLus_Gap[1:10]
# CCI_100_Plus_Start_Date[1]
# CCI_100_Plus_End_Date[1]
# CCI_100_Plus_Start_Date[2]
# CCI_100_Plus_End_Date[2]
# CCI_100_Plus_Start_Index[1]
# CCI_100_Plus_End_Index[1]
# CCI_100_Plus_Start_Index[2]
# CCI_100_Plus_End_Index[2]
# CCI_100_PLus_Gap[1:10]
# test=data.frame(start=CCI_100_Plus_Start_Index, end=CCI_100_Plus_End_Index)

test_gap=data.frame(date=index(SLVHist_02_CCI[CCI_100_PLus_Gap]), value=SLVHist_02_CCI[CCI_100_PLus_Gap], index=CCI_100_PLus_Gap)
test_gap[1:20]
