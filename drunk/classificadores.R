library(caret)

#Naive bayes
nv_train <- function(dadosP) {
  fit_nv <- train(x = dadosP[,2:ncol(dadosP)], 
                  y = dadosP$alc, 
                  method = "nb", 
                  trControl = trainControl(method = "cv", number = 10)
  ) 
  return (fit_nv)
}

svm_train <- function(dadosP) {
  fit_nv <- train(x = dadosP[,2:ncol(dadosP)], 
                  y = dadosP$alc, 
                  method = "svmRadial", 
                  trControl = trainControl(method = "cv", number = 10)
  ) 
  return (fit_nv)
}

glm_train <- function(dadosP) {
  fit_nv <- train(x = dadosP[,2:ncol(dadosP)], 
                  y = dadosP$alc, 
                  method = "glm", 
                  family="binomial",
                  trControl = trainControl(method = "cv", number = 10)
  ) 
  return (fit_nv)
}

classificar <- function(dadosP) {
  #fit <- nv_train(dadosP)
  fit <- svm_train(dadosP)
  fit
  bh_pred <- predict(fit, dadosP)
  bh_pred
  
  precision <- posPredValue(bh_pred, dadosP$alc)
  print(paste("Precision", precision, sep=" "))
  recall <- sensitivity(bh_pred, dadosP$alc)
  print(paste("Recall", recall, sep=" "))
  F1 <- (2 * precision * recall) / (precision + recall)
  print(paste("F1", F1, sep=" "))
  
  a <- table(bh_pred, dadosP$alc)
  
  uarA <- a[1,1] / (a[1,1] + a[1,2])
  uarNA <- a[2,2] / (a[2,2] + a[2,1])
  if (uarNA == "NaN"){
    uarNA = 0
  }
  uar = (uarA + uarNA) / 2
  uar
  
  #TP FP
  #FN TN
  return (uar)
}

'
#Comparando algoritmos
# load the library
library(mlbench)
library(caret)
# load the dataset
data(PimaIndiansDiabetes)
# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the LVQ model
set.seed(7)
modelLvq <- train(diabetes~., data=PimaIndiansDiabetes, method="lvq", trControl=control)
# train the GBM model
set.seed(7)
modelGbm <- train(diabetes~., data=PimaIndiansDiabetes, method="gbm", trControl=control, verbose=FALSE)
# train the SVM model
set.seed(7)
modelSvm <- train(diabetes~., data=PimaIndiansDiabetes, method="svmRadial", trControl=control)
# collect resamples
results <- resamples(list(LVQ=modelLvq, GBM=modelGbm, SVM=modelSvm))
# summarize the distributions
summary(results)
# boxplots of results
bwplot(results)
# dot plots of results
dotplot(results)
'