library(quantmod)
library(ggplot2)

#https://ntguardian.wordpress.com/2017/03/27/introduction-stock-market-data-r-1/


start=as.Date('2016-01-01')
end=Sys.Date()
getMetals('XAG', src='google', from=start, to=end)
ggplot(XAGUSD, aes(x=Index, y=XAG.USD))+geom_line()

XAG_MACD=MACD(XAGUSD[,'XAG.USD'], 12, 26, 19, percent = FALSE)
# BULLISH DIVERGE - PRICE LOWER LOW, MACD HIGHER LOW
# SILVER PRICR 08/05/2017 VS 10/07/2017


XAG_SMA_20=SMA(XAGUSD[, 'XAG.USD'], 20)
XAG_SMA_60=SMA(XAGUSD[, 'XAG.USD'], 60)

XAG_SMA=merge(XAGUSD, XAG_SMA_20, XAG_SMA_60, XAG_MACD)
XAG_SMA=XAG_SMA[!is.na(XAG_SMA[,'SMA'])]

# ggplot(AG_SMA, aes(x=index, y=c('AG.Close', 'SMA')))+geom_line()
plot(x=XAG_SMA[,1], y="Time", major.ticks = 'months')
lines(x=XAG_SMA[,2], col='orange')
lines(x=XAG_SMA[,3], col='blue')

chartSeries(XAGUSD, TA='addRSI()', theme = 'white')
chartSeries(XAGUSD, TA='addMACD(); addBBands(); addRSI(); addCCI();', theme = 'white')


test=RSI(XAGUSD)
tail(test)
