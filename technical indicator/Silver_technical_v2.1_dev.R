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
            if (is.null(SignalFlag)==TRUE)
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
  #CCI03[[h]]=data.frame(Date=index(CCI02[paste(StartDate[h], EndDate[h], sep="::")]), 
                        #Value=as.numeric(CCI02[paste(StartDate[h], EndDate[h], sep="::")]))
}

#NUMBER OF DAYS AN OSCILLATOR CAN USUALLY LAST
lapply(CCI03, function(x) (x$flag==1|x$flag==1))

CCI03[[1]]$flag==1

