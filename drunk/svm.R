library(tools)

PATH_FIT <- "resultados/svm/fit.Rda"
PATH_PRED <- "resultados/svm/pred.Rda"
PATH_IMAGE <- "resultados/svm/image.RData"


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
