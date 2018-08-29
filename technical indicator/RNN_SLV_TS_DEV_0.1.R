library(Quandl)
library(tensorflow)
library(keras)

TimeSeriesStartDate='1985-11-20'

#INDUSTRY PRODUCTION
IndustrialProduction=Quandl("FRED/INDPRO", type='xts')
names(IndustrialProduction)="IndustrialProduction"

#US GDP GROWTH RATE
USGDP=Quandl("FRED/Y695RY2Q224SBEA", api_key="GixSX89oiCWDRyS3B-Dy",type = 'xts')
names(USGDP)="USGDP"

#FED FUND RATE DAILY
FEDRate=Quandl("FED/RIFSPFF_N_D", api_key="GixSX89oiCWDRyS3B-Dy", type = 'xts')
names(FEDRate)="FEDRate"

#US CPI
USCPI=Quandl("RATEINF/CPI_USA", type='xts')
names(USCPI)='USCPI'

#US DURABLE GOOD
USDurable=Quandl("FRED/DGORDER", api_key="GixSX89oiCWDRyS3B-Dy", type='xts')
names(USDurable)="USDurable"

#OECD US CONSUMER CONFIDENCE
USCConfidence=Quandl("OECD/KEI_CSCICP02_USA_ST_M", api_key="GixSX89oiCWDRyS3B-Dy", type='xts')
names(USCConfidence)='USCConfidence'

#US NONFARM PAYROLLS
USNonFarm=Quandl("FRED/PAYEMS", api_key="GixSX89oiCWDRyS3B-Dy", type='xts')
names(USNonFarm)='USNonFarm'

#US PHILADELPHIA FED MANUFACTURING INDEX
USPhillyFEDManIndex=Quandl("FRBP/GAC", api_key="GixSX89oiCWDRyS3B-Dy", type='xts')
names(USPhillyFEDManIndex)='USPhillyFEDManIndex'

#US INITIAL JOBLESS CLAIM
USInitialJobless=Quandl("FRED/ICSA", api_key="GixSX89oiCWDRyS3B-Dy", type='xts')
names(USInitialJobless)='USInitialJobless'

#US HOUSE PRICE INDEX
USHPI=Quandl("FMAC/HPI_USA", api_key="GixSX89oiCWDRyS3B-Dy", type = 'xts')
names(USHPI)='USHPI'

#US DOLLAR
USDollarIndex=Quandl("CHRIS/ICE_DX1", api_key="GixSX89oiCWDRyS3B-Dy", type='xts')[, c(4,7)]
names(USDollarIndex)=c('USDollarIndex', 'USDollarIndexVol')

library(quantmod)
#S&P 500
getSymbols('^GSPC', src = "yahoo", from=as.Date('1900-01-01'))
SP500=GSPC[, c(4,5)]
SP500$SP500HLV=GSPC[,2]-GSPC[,1]
names(SP500)=c('SP500', 'SP500VOL', 'SP500HLV')


#EURUSD
getSymbols("EURUSD=X", from=as.Date('1900-01-01'))
EURUSD=`EURUSD=X`[, c(4)]
EURUSD$EURUSD_HLV=`EURUSD=X`[,2]-`EURUSD=X`[,1]
names(EURUSD)=c('EURUSD', 'EURUSDHLV')


#SILVER
SLV=Quandl("LBMA/SILVER", type='xts')[,1]
names(SLV)="SLV"

data=merge(SLV, 
           USDollarIndex, 
           IndustrialProduction, 
           USGDP,
           FEDRate,
           USCPI,
           USDurable,
           USCConfidence,
           USNonFarm,
           USPhillyFEDManIndex,
           USInitialJobless,
           USHPI,
           SP500,
           EURUSD,
           all=TRUE)

#REMVOE THE EXTRA HISTORY DATA
data01=data[index(data)>=TimeSeriesStartDate, ]
for (i in (1:dim(data)[2]))
{
  data01[,i]=na.locf(data[,i], fromLast = TRUE)
}


test=data[, names(data)=='USGDP']
test01=apply(data, 2, function(x)(na.locf(x, fromLast = TRUE)))
test02=na.locf(test, fromLast = TRUE)

# https://stackoverflow.com/questions/35979455/create-daily-dataset-using-repeated-monthly-values