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

#ALWAYS USE THE FIRST NUMBER TO FIX NEXT
#TEST THE FromNext rather than FromLast SHOULD BE THE RIGHT OPTION

data01=data
for (i in (2:dim(data)[2]))
{
  data01[,i]=na.locf(data[,i], fromNext = TRUE)
}

#REMVOE THE EXTRA HISTORY DATA
data02=data01[index(data01)>=TimeSeriesStartDate, ]

#REMOVE SATURDAYS AND SUNDAYS
data02=data02[!is.na(data02[, 1]),]

#CONVERT MISSING VALUE TO 0
data02[is.na(data02)]=0


#STANDARDISE THE MATRIX
mean=apply(data02, 2, mean)
SD=apply(data02, 2, sd)

data03=scale(data02, center=mean, scale=SD)

generator <- function(data, lookback, delay, min_index, max_index,
                      shuffle = FALSE, batch_size = 60, step = 1) {
  if (is.null(max_index))
    max_index <- nrow(data) - delay - 1
  i <- min_index + lookback
  function() {
    if (shuffle) {
      rows <- sample(c((min_index+lookback):max_index), size = batch_size)
    } else {
      if (i + batch_size >= max_index)
        i <<- min_index + lookback
      rows <- c(i:min(i+batch_size, max_index))
      i <<- i + length(rows)
    }
    samples <- array(0, dim = c(length(rows),
                                lookback / step,
                                dim(data)[[-1]]))
    targets <- array(0, dim = c(length(rows)))
    for (j in 1:length(rows)) {
      indices <- seq(rows[[j]] - lookback, rows[[j]],
                     length.out = dim(samples)[[2]])
      samples[j,,] <- data[indices,]
      targets[[j]] <- data[rows[[j]] + delay,2]
    }
    list(samples, targets)
  }
}

lookback = 100
step = 1
delay = 0
batch_size = 70


train_gen = generator(
  data03,
  lookback = lookback,
  delay = delay,
  min_index = 1,
  max_index = 7000,
  shuffle = FALSE,
  step = step,
  batch_size = batch_size
)
val_gen = generator(
  data03,
  lookback = lookback,
  delay = delay,
  min_index = 7001,
  max_index = 8000,
  step = step,
  batch_size = batch_size
)
test_gen = generator(
  data03,
  lookback = lookback,
  delay = delay,
  min_index = 8001,
  max_index = NULL,
  step = step,
  batch_size = batch_size
)
val_steps <- (8000 - 7001 - lookback) / batch_size
test_steps <- (nrow(data03) - 8001 - lookback) / batch_size




model_01=keras_model_sequential() %>%
  layer_gru(units = 32,
            activation = 'relu', 
            dropout = 0.1,
            recurrent_dropout = 0.3,
            return_sequences = TRUE,
            input_shape = list(NULL, dim(data03)[[-1]])) %>%
  layer_gru(unit = 32, 
            activation = 'relu',
            dropout= 0.1,
            recurrent_dropout = 0.3) %>%
  layer_dense(units = 1)

model_01 %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = 'mae'
)



history_01 = model_01 %>% fit_generator(
  callbacks = callback_model_checkpoint("/Users/mingzhang/Documents/R/dev/technical_indicator/technical indicator/RNN_SLV_TS_MODELS/MODEL_01/model_01.{epoch:02d}-{val_loss:.2f}.hdf5",
                                        monitor = 'val_loss',
                                        save_best_only = FALSE),
  train_gen,
  steps_per_epoch = 500,
  epochs = 15,
  validation_data = val_gen,
  validation_steps = val_steps
)


ModelPred_01=predict(model_01, test_gen()[[1]])
ModelOutput_01=data.frame(Pred=ModelPred_01, 
                          Actual=test_gen()[[2]], 
                          Date=seq(as.Date("2018-01-01"), by="1 day", length.out=xxx))
library(ggplot2)

ggplot(ModelOutput_01, aes(Date)) + 
  geom_line(aes(y = Pred, colour = "Pred")) + 
  geom_line(aes(y = Actual, colour = "Actual"))
