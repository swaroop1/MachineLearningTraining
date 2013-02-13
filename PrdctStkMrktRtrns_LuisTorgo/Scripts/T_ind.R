# Function to calculate the indicator value as given by the formula
# Date : 09-02-2013
# Author : Chrystal Meth

T.indicator <- function(quotes, tgt.margin = 0.025, n.days = 10){
# function to calculate the indicator value of the stock price
# Args:
#     quotes : the zoo (or) xts object which has "Open", "High", "Low", "Close" columns, along with time index
#     tgt.margin : the value of p as given in formula divided by 100.
#     n.days : the k value in the formula  
  v <- apply(HLC(quotes), 1, mean) # returns a vector, with the mean stock price for every day.
  
  r <- matrix(NA, ncol = n.days, nrow = NROW(quotes)) #matrix with 
  
  for(x in 1:n.days) r[, x] <- Next(Delt(v, k = x), x) # fills up the matrix,Next(),Delt() functions are of quantmod package
  
  x <- apply(r, 1, function(x) sum(x[x > tgt.margin | x < -tgt.margin]))
  
  if(is.xts(quotes))
    xts(x, time(quotes))
  else x
  
}
# draws a candle chart, the box indicates , the opening and closing values, the tails indicate the day's highest
# and lowest values, orange indicates decrease from previous day closing, green indicates reverse.
candleChart(last(GSPC, "3 months"), theme = "white", TA = NULL) 

avgPrice <- function(p) apply(HLC(p), 1, mean)

# newTA is used to create plotting funtions for indicators, which are to be included in the candle stick chart
addAvgPrice <- newTA(FUN = avgPrice, col = 1, legend = "AvgPrice")

addT.indicator <- newTA(FUN = T.indicator, col = "red", legend = "tgtRet")

addAvgPrice(on = 1) # on value indiactes the same plot as the candle chart plot.

addT.indicator()

names(GSPC) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")

various indicators from the TTR package :
  myATR <- function(x) ATR(HLC(x))[, "atr"]
  mySMI <- function(x) SMI(HLC(x))[, "SMI"]
  myADX <- function(x) ADX(HLC(x))[, "ADX"]
  myAroon <- function(x) aroon(x[, c("High","Low")])$oscillator
  myBB <- function(x) BBands(HLC(x))[, "pctB"]
  myChaikinVol <- function(x) Delt(chaikinVolatility(x[, c("High", "Low")]))[, 1]
  myCLV <- function(x) EMA(CLV(HLC(x)))[, 1]
  myEMV <- function(x) EMV(x[, c("High", "Low")], x[, "Volume"])[, 2]
  myMACD <- function(x) MACD(Cl(x))[, 2]
  myMFI <- function(x) MFI(x[, c("High", "Low", "Close")], x[, "Volume"])
  mySAR <- function(x) SAR(x[, c("High", "Close")])[, 1]
  myVolat <- function(x) volatility(OHLC(x), calc = "garman")[, 1]

# elimination of unimportant variables from the initial set using random forest ################

dataGSPC.model <- specifyModel(T.indicator(GSPC) ~ Delt(Cl(GSPC), k = 1:10) + myATR(GSPC) + mySMI(GSPC) + myADX(GSPC) + myAroon(GSPC) + myBB(GSPC) + myChaikinVol(GSPC) + myCLV(GSPC) + CMO(Cl(GSPC)) + EMA(Delt(Cl(GSPC))) + myEMV(GSPC) + myVolat(GSPC) + myMACD(GSPC) + myMFI(GSPC) + RSI(Cl(GSPC)) + mySAR(GSPC) + runMean(Cl(GSPC)) + runSD(Cl(GSPC)))

set.seed(1234)

rf_GSPC <- buildModel(dataGSPC.model, method = 'randomForest', training.per = c(start(GSPC), index(GSPC["1999-12-31"])), ntree = 50, importance = T)

varImpPlot(rf_GSPC@fitted.model, type = 1)

imp_GSPC <- importance(rf_GSPC@fitted.model, type = 1)

rownames(imp_GSPC)[which(imp_GSPC > 10)]

#new abstract model created with only select varibles identified by random forest. #################
dataGSPCfinal.model <-  specifyModel(T.indicator(GSPC) ~ Delt(Cl(GSPC), k = 4) + myATR(GSPC) + mySMI(GSPC) + myADX(GSPC) + myAroon(GSPC) + myMACD(GSPC) + myMFI(GSPC) + mySAR(GSPC) + runSD(Cl(GSPC)))

Tdata.train <- as.data.frame(modelData(dataGSPCfinal.model, data.window = c("1970-01-02", "1999-12-31")))

Tdata.eval <- na.omit(as.data.frame(modelData(dataGSPCfinal.model, data.window = c("2000-01-01", "2013-02-08"))))

T_GSPC.formula <- as.formula("T.indicator.GSPC ~ .")

set.seed(1234)

Tdata.train.normal <- scale(Tdata.train)

Tdata.eval.normal <- scale(Tdata.eval)

library(nnet)

# Obtaining the neural network model ######

 nn <- nnet(T_GSPC.formula, Tdata.train.normal[1:1000,], size = 50, decay = 0.01, maxit = 100000, linout = T, trace = F)

# obtaining prediction from the ANN model #############

 Tdata.train.normal.preds <- predict(nn, Tdata.train.normal[1001:2000,])

 Tdata.train.preds <- unscale(Tdata.train.normal.preds, Tdata.train.normal)

# Transforming numeric values in to trading signals ############

 GSPC_signals.nn.preds <- trading.signals(Tdata.train.preds, 0.1, -0.1)

 trueGSPC_signals.nn <- trading.signals(Tdata.train[1001:2000,"T.indicator.GSPC"], 0.1, -0.1)

 sigs.PR(GSPC_signals.nn.preds, trueGSPC_signals.nn)


# ANN for classification problems #########

set.seed(1234)

GSPC_trading.signals <- trading.signals(Tdata.train[,"T.indicator.GSPC"], 0.1, -0.1)
 
Tdata.train.classification <- data.frame(signals = GSPC_trading.signals, scale(Tdata.train[, -1]))

Tdata.eval.classificaion <- data.frame(signals = trading.signals(Tdata.eval[,"T.indicator.GSPC"], 0.1, -0.1), scale(Tdata.eval[, -1]))


 nn <- nnet(signals ~ ., Tdata.train.classification[1:1000,], size = 10, decay = 0.01, maxit = 1000, trace = F)

Tdata.train.classify.predict <- predict(nn, Tdata.train.classification[1001:2000,], type = "class")

sigs.PR(preds = Tdata.train.classify.predict, trues = Tdata.train.classification[1001:2000, 1])

# Suport Vector Machines ############

 sv.GSPC <- svm(T_GSPC.formula, Tdata.train[1:1000,], gamma = 0.001, cost = 100)

 sv.GSPC_predictions <- predict(sv.GSPC, Tdata.train[1001:2000,])

 signals.svm_GSPC <- trading.signals(sv.GSPC_predictions, 0.1, -0.1)

 true.signals.svm_GSPC <- trading.signals(Tdata.train[1001:2000, "T.indicator.GSPC"], 0.1, -0.1)

 sigs.PR(signals.svm_GSPC, true.signals.svm_GSPC)

#  Support Vector Machines for classfication task using kernlab package #########

 GSPC_svmkernlab_data <- cbind(signals = trading.signals(Tdata.train[, "T.indicator.GSPC"], 0.1, -0.1), Tdata.train[, -1])

 kernSVM_GSPC <- ksvm(signals ~ ., GSPC_svmkernlab_data[1:1000,], C = 10)
# predicting the values and finding out precision and recall

  kernSVM_predictions <- predict(kernSVM_GSPC, GSPC_svmkernlab_data[1001:2000,])

  sigs.PR(kernSVM_predictions, GSPC_svmkernlab_data[1001:2000, 1])

# Multivariate Adaptive Regression Splines (MARS) ########

 GSPC_earth <- earth(T_GSPC.formula, Tdata.train[1:1000,])

 GSPC_earth_predicts <- predict(GSPC_earth, Tdata.train[1001:2000,])

 sigs.GSPC_earth_predicts <- trading.signals(GSPC_earth_predicts, 0.1, -0.1)

 true.sigs.GSPC_earth <- trading.signals(Tdata.train[1001:2000, "T.indicator.GSPC"], 0.1, -0.1)

 sigs.PR(sigs.GSPC_earth_predicts, true.sigs.GSPC_earth)