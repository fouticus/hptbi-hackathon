################################################################################
#
################################################################################
# Mortality Model
#
# Args:
#   data a data.frame resulting from a call to prepare_mortality_data()
#
# Return:
#   An R object.  This object will have the "hackathon_mortality_model" class
#   prepended to it such that a call to predict can be used to generate
#   predictions from the training and testing data sets.
#
mortality_model <- function(data) {

  ##############################################################################
  # User code starts here
  
  Y <- as.factor(data$Y)
  pmort <- mean(data$Y)
  
  # standardize columns and remember for testing
  X <- data$X
  X_mean <- apply(X, 2, function(x) mean(x, na.rm=T))
  X_sd <- apply(X, 2, function(x) sd(x, na.rm=T))
  X2 <- sapply(seq_len(ncol(X)), function(i){x2 <- (X[,i] - X_mean[i])/X_sd[i]; x2[is.na(x2)] <- 0; x2})
  
  # train model
  rtn <- list()
  rtn$model <- randomForest::randomForest(X2, Y, ntree=1000, classwt=c(1-pmort, pmort))
  rtn$vars <- names(X)  # record the variable names
  rtn$X_mean <- X_mean
  rtn$X_sd <- X_sd
  
  # User code ends here
  ##############################################################################

  class(rtn) <- c("hackathon_mortality_model", class(rtn))
  rtn
}

################################################################################
# Predict Hackathon Mortality Model
#
# An S3 function call for hackathon_mortality_model
#
# Args:
#   object  a hackathon_mortality_model object
#   newdata a data.frame
#   ...     additional arguments passed through.  Not expected to be used as
#           part of the hackathon.
#
# Return:
#   A character vector of length equal to the nrow(newdata) with values
#   "Mortality" and "Alive"
#
predict.hackathon_mortality_model <- function(object, newdata, ...) {
  
  ##############################################################################
  # User Defined data preparation code starts here
  
  # make sure we have the same columns and in the same order as training
  vars <- object$vars
  X <- matrix(NA, nrow(newdata$X), length(vars))
  for(i in seq_along(vars)){
    if(vars[i] %in% names(newdata$X))
      X[,i] <- newdata$X[[vars[i]]]
  }
  X <- as.data.frame(X)
  
  X_mean <- object$X_mean
  X_sd <- object$X_sd
  X2 <- sapply(seq_len(ncol(X)), function(i){x2 <- (X[,i] - X_mean[i])/X_sd[i]; x2[is.na(x2)] <- 0; x2})
  
  loadNamespace("randomForest")
  pred <- getS3method("predict", "randomForest")(object$model, X2)
  preds <- ifelse(pred == "1", "Mortality", "Alive")
  
  return(preds)
}

################################################################################
#                                 End of File
################################################################################
