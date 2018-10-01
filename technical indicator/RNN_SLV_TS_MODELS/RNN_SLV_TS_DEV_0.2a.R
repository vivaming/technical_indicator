library(Quandl)
library(tensorflow)
library(keras)
library(ggplot2)
library(dplyr)
library(quantmod)
library(tidyverse)
library(tidyquant)

options(scipen=999)

TimeSeriesStartDate='1985-11-20'

#INDUSTRY PRODUCTION

IndustrialProduction=Quandl("FRED/INDPRO", api_key="GixSX89oiCWDRyS3B-Dy", type='xts')
names(IndustrialProduction)="IndustrialProduction"
#CONVERT TO RELEASE DATE
index(IndustrialProduction)=as.Date(index(IndustrialProduction))+45


#US GDP GROWTH RATE
USGDP=Quandl("FRED/Y695RY2Q224SBEA", api_key="GixSX89oiCWDRyS3B-Dy",type = 'xts')
names(USGDP)="USGDP"
#CONVERT TO RELEASE DATE
index(USGDP)=as.Date(index(USGDP))+115

#FED FUND RATE DAILY
FEDRate=Quandl("FED/RIFSPFF_N_D", api_key="GixSX89oiCWDRyS3B-Dy", type = 'xts')
names(FEDRate)="FEDRate"


#US CPI
USCPI=Quandl("RATEINF/CPI_USA", api_key="GixSX89oiCWDRyS3B-Dy", type='xts')
names(USCPI)='USCPI'
#CONVERT TO RELEASE DATE
index(USCPI)=as.Date(index(USCPI))+42


#US DURABLE GOOD
USDurable=Quandl("FRED/DGORDER", api_key="GixSX89oiCWDRyS3B-Dy", type='xts')
names(USDurable)="USDurable"
#CONVERT TO RELEASE DATE
index(USDurable)=as.Date(index(USDurable))+54


#OECD US CONSUMER CONFIDENCE
USCConfidence=Quandl("OECD/KEI_CSCICP02_USA_ST_M", api_key="GixSX89oiCWDRyS3B-Dy")
names(USCConfidence)='USCConfidence'
#CONVERT TO RELEASE DATE
USCConfidence=xts(USCConfidence[, -1], order.by=(USCConfidence[,1]))
index(USCConfidence)=index(USCConfidence)+29


#US NONFARM PAYROLLS
USNonFarm=Quandl("FRED/PAYEMS", api_key="GixSX89oiCWDRyS3B-Dy", type='xts')
names(USNonFarm)='USNonFarm'
#CONVERT TO RELEASE DATE
index(USNonFarm)=as.Date(index(USNonFarm))+54


#US PHILADELPHIA FED MANUFACTURING INDEX
USPhillyFEDManIndex=Quandl("FRBP/GAC", api_key="GixSX89oiCWDRyS3B-Dy")
names(USPhillyFEDManIndex)='USPhillyFEDManIndex'
#REMOVE THE ACTUAL DUPLICATE IN EACH MONTH
USPhillyFEDManIndex=USPhillyFEDManIndex[format(USPhillyFEDManIndex[,1], "%d")=='01',]
USPhillyFEDManIndex=xts(USPhillyFEDManIndex[, -1], order.by = (USPhillyFEDManIndex[, 1]))
#CONVERT TO RELEASE DATE
index(USPhillyFEDManIndex)=as.Date(index(USPhillyFEDManIndex))+54


#US INITIAL JOBLESS CLAIM
USInitialJobless=Quandl("FRED/ICSA", api_key="GixSX89oiCWDRyS3B-Dy", type='xts')
names(USInitialJobless)='USInitialJobless'
#CONVERT TO RELEASE DATE
index(USInitialJobless)=as.Date(index(USInitialJobless))-2

#US FEDHOUSE PRICE INDEX
USHPI=Quandl("FRED/USSTHPI", api_key="GixSX89oiCWDRyS3B-Dy", type = 'xts')
names(USHPI)='USHPI'
index(USHPI)=as.Date(index(USHPI))+144


#US M2 MONEY SUPPLY
USM2=Quandl("FED/M2_M", api_key="GixSX89oiCWDRyS3B-Dy", type='xts')
USM2=Delt(USM2, type=c('arithmetic'))
names(USM2)='USM2'
index(USM2)=as.Date(index(USM2))+55
#ggplot(data = USM2[650:dim(USM2)[1]] , aes(x=Index, y=USM2)) + geom_line()+theme_tq()



#US DOLLAR
USDollarIndex=Quandl("CHRIS/ICE_DX1", api_key="GixSX89oiCWDRyS3B-Dy", type='xts')[, c(4,7)]
names(USDollarIndex)=c('USDollarIndex', 'USDollarIndexVol')

#FRED US DOLLAR INDEX
FedUSIndex=Quandl("FRED/DTWEXM", api_key="GixSX89oiCWDRyS3B-Dy", type='xts')
#USE FRED US DOLLAR INDEX TO BACKFILL THE ORIGINAL US DOLLAR FUTURE BETWEEN 02/01/1973 AND 19/11/1985
#ESITMATED DIFFERENCE BETWEEN THESE TWO INDEX IS APPROXIMATELY 1.05

USDollarIndexBackfill=FedUSIndex["/1985-11-19"]*1.05
colnames(USDollarIndexBackfill)='USDollarIndex'
USDollarIndexBackfill$USDollarIndexVol=rep(list(0), length(USDollarIndexBackfill))

USDollarIndex=rbind(USDollarIndexBackfill, USDollarIndex)



# US 2, 10, 30 YEARS TREASURY BOND RATE
USTreasury02Y=Quandl("FRED/DGS2", api_key="GixSX89oiCWDRyS3B-Dy", tyoe='xts')
USTreasury10Y=Quandl("FRED/DGS10", api_key="GixSX89oiCWDRyS3B-Dy", tyoe='xts')
USTreasury30Y=Quandl("FRED/DGS30", api_key="GixSX89oiCWDRyS3B-Dy", tyoe='xts')

library(quantmod)
#S&P 500
getSymbols('^GSPC', src = "yahoo", from=as.Date('1900-01-01'))
SP500=GSPC[, c(4,5)]
SP500$SP500HLV=GSPC[,2]-GSPC[,1]
names(SP500)=c('SP500', 'SP500VOL', 'SP500HLV')

#NASDAQ
getSymbols('^IXIC', src = "yahoo", from=as.Date('1900-01-01'))

#EURUSD
getSymbols("EURUSD=X", from=as.Date('1900-01-01'))
EURUSD=`EURUSD=X`[, c(4)]
EURUSD$EURUSD_HLV=`EURUSD=X`[,2]-`EURUSD=X`[,1]
names(EURUSD)=c('EURUSD', 'EURUSDHLV')


#SILVER
SLV=Quandl("LBMA/SILVER", api_key="GixSX89oiCWDRyS3B-Dy", type='xts')[,1]
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

# dim(data02)[1]

#STANDARDISE THE MATRIX
mean=apply(data02, 2, mean)
SD=apply(data02, 2, sd)

data03=scale(data02, center=mean, scale=SD)

train01=data03[1:7000, ]
val01=data03[7001:8000, ]
test01=data03[8001:nrow(data02), ]

# TS=train01
# Timesteps=250
# BatchNumber=floor(nrow(TS)/Timesteps)
# FeatureNumber=dim(TS)[2]-1

#ROUND DOWN TO THE NEAREST INTEGER IF THE NUMBER CAN'T NOT BE FULLY DIVIDED
TSTensorflowGEN=function(TS, BatchNumber, Timesteps, FeatureNumber)
{
  #RANDOMLY SELECTED SAMPLE NUMBER THAT CAN BE FULLY DIVIDED
    RndSample=sort(sample(c(1:nrow(TS)), size = Timesteps * BatchNumber, replace = FALSE))
    TSFiltered=TS[RndSample, ]
    
    ArrayX=array(numeric() ,c(BatchNumber, Timesteps, FeatureNumber))
    #TrainY=matrix(, BatchNumber, Timesteps)
    ArrayY=array(numeric() ,c(BatchNumber, Timesteps, 1))
    
    for (i in (1:BatchNumber))
    {
      #print(paste0((i-1)*Timesteps+1, ":", i*Timesteps))
      TwotoThreeFilter=seq((i-1)*Timesteps+1, i*Timesteps)
      ArrayX[i,,]=TSFiltered[TwotoThreeFilter, -1]
      #TrainY[seq((i-1)*Timesteps+1, i*Timesteps)]=as.numeric(TSFiltered[TwotoThreeFilter, 1])
      ArrayY[i,,]=TSFiltered[TwotoThreeFilter, 1]
      #print(paste0((i-1)*Timesteps+1, ':', i*Timesteps))
      Result=list(ArrayX, ArrayY)
    }

  return(Result)
}

train02=TSTensorflowGEN(train01
                        , Timesteps = 250 #250 TRADING DAYS
                        , BatchNumber = floor(nrow(train01)/Timesteps)
                        , FeatureNumber = dim(train01)[2]-1)
TrainX=train02[[1]]
TrainY=train02[[2]]

val02=TSTensorflowGEN(val01
                      , Timesteps = 250 #250 TRADING DAYS
                      , BatchNumber = floor(nrow(val01)/Timesteps)
                      , FeatureNumber = dim(val01)[2]-1)
ValX=val02[[1]]
ValY=val02[[2]]

test02=TSTensorflowGEN(test01
                  , Timesteps = 250 #250 TRADING DAYS
                  , BatchNumber = floor(nrow(test01)/Timesteps)
                  , FeatureNumber = dim(test01)[2]-1)
TestX=test02[[1]]
TestY=test02[[2]]

# FLAGS = flags(
#   )


FLAGS <- flags(
  flag_boolean("stateful", FALSE),
  flag_boolean("stack_layers", TRUE),
  # number of samples fed to the model in one go
  flag_integer("batch_size", 28),
  # size of the hidden state, equals size of predictions
  flag_integer("n_timesteps", 250),
  # how many epochs to train for
  flag_integer("n_epochs", 100),
  # fraction of the units to drop for the linear transformation of the inputs
  flag_numeric("dropout", 0.1),
  # fraction of the units to drop for the linear transformation of the 
  # recurrent state
  flag_numeric("recurrent_dropout", 0.2),
  # loss function. Found to work better for this specific case than mean
  # squared error
  flag_string("loss", "mae"),
  # optimizer = stochastic gradient descent. Seemed to work better than adam 
  # or rmsprop here (as indicated by limited testing)
  flag_string("optimizer_type", "rmsprop"),
  # size of the LSTM layer
  flag_integer("n_units_1", 64),
  flag_integer("n_units_2", 32),
  # learning rate
  flag_numeric("lr", 0.001),
  # momentum, an additional parameter to the SGD optimizer
  flag_numeric("momentum", 0.2),
  # parameter to the early stopping callback
  flag_integer("patience", 10)
)


# the number of predictions we'll make equals the length of the hidden state
n_predictions <- FLAGS$n_timesteps
# how many features = predictors we have
n_features <- 17
# just in case we wanted to try different optimizers, we could add here
# optimizer <- switch(FLAGS$optimizer_type,
#                     sgd = optimizer_sgd(lr = FLAGS$lr, 
#                                         momentum = FLAGS$momentum)

optimizer=optimizer_rmsprop()
)

# callbacks to be passed to the fit() function
# We just use one here: we may stop before n_epochs if the loss on the
# validation set does not decrease (by a configurable amount, over a 
# configurable time)
callbacks <- list(
  callback_early_stopping(patience = FLAGS$patience)
)


# create the model
model <- keras_model_sequential()

# add layers
# we have just two, the LSTM and the time_distributed 
model %>%
  layer_lstm(
    units = FLAGS$n_units_1, 
    # the first layer in a model needs to know the shape of the input data
    #batch_input_shape  = c(FLAGS$batch_size, FLAGS$n_timesteps, n_features),
    input_shape = list(NULL, 17),
    dropout = FLAGS$dropout,
    recurrent_dropout = FLAGS$recurrent_dropout,
    # by default, an LSTM just returns the final state
    return_sequences = TRUE
  ) %>%
layer_lstm(
  units = FLAGS$n_units_2, 
  #input_shape = list(NULL, 17),
  dropout = FLAGS$dropout,
  recurrent_dropout = FLAGS$recurrent_dropout,
  # by default, an LSTM just returns the final state
  return_sequences = TRUE
) %>% 
  time_distributed(layer_dense(units = 1))

model %>%
  compile(
    loss = FLAGS$loss,
    optimizer = optimizer,
    # in addition to the loss, Keras will inform us about current 
    # MSE while training
    metrics = list("mean_squared_error")
  )

history <- model %>% fit(
  x  = TrainX,
  y  = TrainY,
  validation_data = list(ValX, ValY),
  batch_size = 28,
  epochs     = FLAGS$n_epochs,
  callbacks = callback_model_checkpoint("/Users/mingzhang/Documents/R/dev/technical_indicator/technical indicator/RNN_SLV_TS_MODELS/MODEL_05/model_05.{epoch:02d}-{val_loss:.2f}.hdf5",
                                        monitor = 'val_loss',
                                        save_best_only = FALSE)
)

model=load_model_hdf5("/Users/mingzhang/Documents/R/dev/technical_indicator/technical indicator/RNN_SLV_TS_MODELS/MODEL_05/model_05.04-0.19.hdf5")

pred_test <- model %>%
  predict(TestX, batch_size = 1) %>%
  .[, , 1]

pred_test=(pred_test*SD[[1]]+mean[[1]])

df=data.frame(date=seq(as.Date('2018-01-01'), by = '1 day', length=length(pred_test)), 
              actual=as.numeric(TestY*SD[[1]]+mean[[1]]),
              pred=pred_test)

ggplot(df, aes(date)) + 
  geom_line(aes(y=actual, colour = 'actual')) + 
  geom_line(aes(y=pred, colour = 'pred')) 



val_test <- model %>%
  predict(ValX) %>%
  .[, , 1]

val_test=val_test*SD[[1]]+mean[[1]]
val_actual=ValY*SD[[1]]+mean[[1]]

#NEED TO CONVERT 2D MATRIX TO 1D VECTOR
ValTestReshaped=numeric()
ValActualReshaped=numeric()
for (m in (1:dim(val_test)[1]))
{
  ValTestReshaped=c(ValTestReshaped, val_test[m, ])
  ValActualReshaped=c(ValActualReshaped, val_actual[m,,])
}


val_df=data.frame(date=seq(as.Date('2016-01-01'), by = '1 day', length=length(val_test)), 
              actual=ValActualReshaped,
              pred=ValTestReshaped)

ggplot(val_df, aes(date)) + 
  geom_line(aes(y=actual, colour = 'actual')) + 
  geom_line(aes(y=pred, colour = 'pred')) 
