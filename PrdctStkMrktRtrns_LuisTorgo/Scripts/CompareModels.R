# Eval models based on monte Carlo methods for simulations.

# Different modelling methods ###########

# build Support Vector Machine model ##########

 MC.svmR <- function(form, train, test, b.t = 0.1, s.t = -0.1, ...){
   require(e1071)
   training_model <- svm(form, train, ...)
   predictions <- predict(training_model, test)
   trading.signals(predictions, b.t, s.t)
   
 }

MC.svmC <- function(form, train, test, b.t = 0.1, s.t = -0.1, ...){
  require(e1071)
  tgtName <- all.vars(form)[1]
  train[, tgtName] <- trading.signals(train[, tgtName], b.t, s.t)
  training_model <- svm(form, train, ...)
  predictions <- predict(training_model, test)
  factor(predictions, levels = c("s", "h", "b"))
    
}

# build Artificial Neural Network model ######

 MC.nnetR <- function(form, train, test, b.t = 0.1, s.t = -0.1, ...){
   require(nnet)
   training_model <- nnet(form, train, ...)
   predictions <- predict(training_model, test)
   trading.signals(predictions, b.t, s.t)
 }

MC.nnetC <- function(form, train, test, b.t = 0.1, s.t = -0.1, ...){
  require(nnet)
  tgtName <- all.vars(form)[1]
  train[, tgtName] <- trading.signals(train[, tgtName], b.t, s.t)
  training_model <- nnet(form, train, ...)
  predictions <- predict(training_model, test, type = "class")
  factor(predictions, levels = c("s", "h", "b"))
 }

MC.earth <- function(form, train, test, b.t = 0.1, s.t = -0.1, ...){
  require(earth)
  training_model <- earth(form, train, ...)
  predictions <- predict(training_model, test)
  trading.signals(predictions, b.t, s.t)
}

# single test set

single <- function(form, train, test, learner, policy.func, ...){
  p <- do.call(paste("MC", learner, sep = "."), list(form, train, test, ...))
  eval.stats(form, train, test, p, policy.func = policy.func)
}

# Sliding Window test #########

slide <- function(form, train, test, learner, relearn.step, policy.func, ...){
  real.learner <- learner(paste("MC", learner, sep = "."), pars = list(...))
  p <- slidingWindowTest(real.learner, form, train, test, relearn.step)
  p <- factor(p, levels = 1:3, labels = c("s", "h", "b"))
  eval.stats(form, train, test, p, policy.func = policy.func)
}

# Growing window test #############

grow <- function(form, train, test, learner, relearn.step, policy.func, ...){
  real.learner <- learner(paste("MC", learner, sep = "."), pars = list(...))
  p <- growingWindowTesting(real.learner, form, train, test, relearn.step)
  p <- factor(p, levels = 1:3, labels = c("s", "h", "b"))
  eval.status(form, train, test, p, policy.func = policy.func)
}


eval.stats <- function(form, train, test, preds, b.t = 0.1, s.t = -0.1, ...){
  #Signals evaluation
  tgtName <- all.vars(form)[1]
  test[, tgtName] <- trading.signals(test[, tgtName], b.t, s.t)
  st <- sigs.PR(preds, test[, tgtName])
  dim(st) <- NULL
  names(st) <- paste(rep(c('prec', 'rec'), each = 3), c('s', 'b', 'sb'), sep = ".")
  
}