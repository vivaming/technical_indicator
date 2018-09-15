library(Quandl)
library(tensorflow)
library(keras)
library(ggplot2)
library(dplyr)

TimeSeriesStartDate='1985-11-20'

#INDUSTRY PRODUCTION
IndustrialProduction=Quandl("FRED/INDPRO", api_key="GixSX89oiCWDRyS3B-Dy", type='xts')
names(IndustrialProduction)="IndustrialProduction"

#US GDP GROWTH RATE
USGDP=Quandl("FRED/Y695RY2Q224SBEA", api_key="GixSX89oiCWDRyS3B-Dy",type = 'xts')
names(USGDP)="USGDP"

#FED FUND RATE DAILY
FEDRate=Quandl("FED/RIFSPFF_N_D", api_key="GixSX89oiCWDRyS3B-Dy", type = 'xts')
names(FEDRate)="FEDRate"

#US CPI
USCPI=Quandl("RATEINF/CPI_USA", api_key="GixSX89oiCWDRyS3B-Dy", type='xts')
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
    
    TrainX=array(numeric() ,c(BatchNumber, Timesteps, FeatureNumber))
    #TrainY=matrix(, BatchNumber, Timesteps)
    TrainY=array(numeric() ,c(BatchNumber, Timesteps, 1))
    
    for (i in (1:BatchNumber))
    {
      #print(paste0((i-1)*Timesteps+1, ":", i*Timesteps))
      TwotoThreeFilter=seq((i-1)*Timesteps+1, i*Timesteps)
      TrainX[i,,]=TSFiltered[TwotoThreeFilter, -1]
      #TrainY[seq((i-1)*Timesteps+1, i*Timesteps)]=as.numeric(TSFiltered[TwotoThreeFilter, 1])
      TrainY[i,,]=TSFiltered[TwotoThreeFilter, 1]
      #print(paste0((i-1)*Timesteps+1, ':', i*Timesteps))
      Result=list(TrainX, TrainY)
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
  flag_boolean("stack_layers", FALSE),
  # number of samples fed to the model in one go
  flag_integer("batch_size", 28),
  # size of the hidden state, equals size of predictions
  flag_integer("n_timesteps", 250),
  # how many epochs to train for
  flag_integer("n_epochs", 20),
  # fraction of the units to drop for the linear transformation of the inputs
  flag_numeric("dropout", 0.2),
  # fraction of the units to drop for the linear transformation of the 
  # recurrent state
  flag_numeric("recurrent_dropout", 0.2),
  # loss function. Found to work better for this specific case than mean
  # squared error
  flag_string("loss", "logcosh"),
  # optimizer = stochastic gradient descent. Seemed to work better than adam 
  # or rmsprop here (as indicated by limited testing)
  flag_string("optimizer_type", "sgd"),
  # size of the LSTM layer
  flag_integer("n_units", 128),
  # learning rate
  flag_numeric("lr", 0.003),
  # momentum, an additional parameter to the SGD optimizer
  flag_numeric("momentum", 0.9),
  # parameter to the early stopping callback
  flag_integer("patience", 10)
)


# the number of predictions we'll make equals the length of the hidden state
n_predictions <- FLAGS$n_timesteps
# how many features = predictors we have
n_features <- 17
# just in case we wanted to try different optimizers, we could add here
optimizer <- switch(FLAGS$optimizer_type,
                    sgd = optimizer_sgd(lr = FLAGS$lr, 
                                        momentum = FLAGS$momentum)
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
    units = FLAGS$n_units, 
    # the first layer in a model needs to know the shape of the input data
    #batch_input_shape  = c(FLAGS$batch_size, FLAGS$n_timesteps, n_features),
    input_shape = list(NULL, 17),
    dropout = FLAGS$dropout,
    recurrent_dropout = FLAGS$recurrent_dropout,
    # by default, an LSTM just returns the final state
    return_sequences = TRUE
  ) %>% time_distributed(layer_dense(units = 1))

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
  epochs     = FLAGS$n_epochs
  #,callbacks = callbacks
)

