ENLR <- function(Xdata, label, alpha, seed) {
  library(glmnet)
  library(caret)
  library(Metrics)
  
  set.seed(seed)  
  
  folds <- createFolds(label, k = 10, list = TRUE, returnTrain = FALSE)
  allAct <- vector()
  allPred <- vector()
  RID_pred <- vector()
  all_coef <- matrix(0, nrow = ncol(Xdata), ncol = 10)
  
  for (i in 1:length(folds)) {
    print(i)
    ind <- folds[[i]]
    RID_pred <- c(RID_pred, rownames(Xdata)[ind])
    
    Xtrain <- Xdata[-ind, ]
    Ytrain <- label[-ind]
    Xtest <- Xdata[ind, ]
    Ytest <- label[ind]
    
    # Preprocess - normalize data
    normParam <- preProcess(Xtrain)
    Xtrain <- predict(normParam, Xtrain)
    Xtest <- predict(normParam, Xtest)
    
    # Train Elastic Net model using cross-validation
    model <- cv.glmnet(Xtrain, Ytrain, alpha = alpha)
    
    # Extract coefficients for selected lambda
    tmp <- as.vector(coef(model, s = "lambda.min"))[-1]
    all_coef[, i] <- tmp
    
    # Predict on test set
    ypred <- predict(model, Xtest, s = "lambda.min")
    
    allAct <- c(allAct, Ytest)
    allPred <- c(allPred, ypred)
  }
  rownames(all_coef) = colnames(Xdata)
  ii= match(rownames(Xdata), RID_pred)
  yhat= allPred[ii]
  pearson_cor= cor(label, yhat, method = "pearson")
  spearman_cor= cor(label, yhat, method = "spearman")
  MAE= mae(label, yhat)
  
  return(list(pearson_cor= pearson_cor, spearman_cor=spearman_cor, MAE= MAE,  yhat = yhat, coefs= all_coef, label= label))
}
