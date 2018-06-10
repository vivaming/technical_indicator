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

#FILL THE NON-LEADING NAs BY PREVIOUS PRICES
SLVHist_02=na.locf(SLVHist_02, fromLast = TRUE)

CCI()

TradeCCI=function(TS,
                  n=14,
                  c=0.015,
                  OpenPoistionsDays=c(), 
                  OpenPositionWeights=c(), 
                  ClosedPositionsDays=c(),
                  ClosePositionWeights=c())