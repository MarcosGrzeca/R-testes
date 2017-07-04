library(tools)
PATH_FIT <- "rda/svm/fit_svm.Rda"
PATH_PRED <- "rda/svm/bh_pred_svm.Rda"
PATH_IMAGE <- "rda/svm/svm.RData"

load("rda/alemao_base_completa.Rda")

print("SVM")

library(caret)

trainAlgoritmo <- function(dadosP) {
  fit_nv <- train(x = dadosP[,2:ncol(dadosP)], 
                  y = dadosP$alc, 
                  method = "svmRadial", 
                  trControl = trainControl(method = "cv", number = 10)
  ) 
  return (fit_nv)
}

source(file_path_as_absolute("classificador_default.R"))