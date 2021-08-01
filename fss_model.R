################################################################################
#
################################################################################
# FSS Model
#
# Args:
#   data a data.frame resulting from a call to prepare_fss_data()
#
# Return:
#   An R object.  This object will have the "hackathon_fss_model" class
#   prepended to it such that a call to predict can be used to generate
#   predictions from the training and testing data sets.
#
fss_model <- function(data) {

  ##############################################################################
  # User code starts here
  
  Y <- data$Y
  
  # standardize columns and remember for testing
  X <- data$X
  X_mean <- apply(X, 2, function(x) mean(x, na.rm=T))
  X_sd <- apply(X, 2, function(x) sd(x, na.rm=T))
  X2 <- sapply(seq_len(ncol(X)), function(i){x2 <- (X[,i] - X_mean[i])/X_sd[i]; x2[is.na(x2)] <- 0; x2})
  
  # train model
  loadNamespace("randomForest")
  rtn <- list()
  rtn$model <- randomForest::randomForest(X2, Y, ntree=2000, mtry=ncol(X2)/2, sampsize=ceiling(0.6*length(Y)))
  rtn$X_mean <- X_mean
  rtn$X_sd <- X_sd
  
  # User code ends here
  ##############################################################################

  class(rtn) <- c("hackathon_fss_model", class(rtn))
  rtn

}

################################################################################
# Predict Hackathon Mortality Model
#
# An S3 function call for hackathon_fss_model
#
# Args:
#   object  a hackathon_fss_model object
#   newdata a data.frame
#   ...     additional arguments passed through.  Not expected to be used as
#           part of the hackathon.
#
# Return:
#   A numeric vector of length equal to the nrow(newdata) representing the
#   predicted total FSS score.
#

predict.hackathon_fss_model <- function(object, newdata, ...) {

  ##############################################################################
  # user defined code starts here

  X <- newdata$X
  X_mean <- object$X_mean
  X_sd <- object$X_sd
  X2 <- sapply(seq_len(ncol(X)), function(i){x2 <- (X[,i] - X_mean[i])/X_sd[i]; x2[is.na(x2)] <- 0; x2})
  
  loadNamespace("randomForest")
  pred <- getS3method("predict", "randomForest")(object$model, X2)
  
  return(round(pred))
}

################################################################################
#                                 End of File
################################################################################
