library(tools)
PATH_FIT <- "rda/nv/fit.Rda"
PATH_PRED <- "rda/nv/bh_pred.Rda"
PATH_IMAGE <- "rda/nv/nv.RData"

load("rda/alemao_base_completa.Rda")

print("Naive Bayes")

library(caret)

trainAlgoritmo <- function(dadosP) {
  fit_nv <- train(x = dadosP[,2:ncol(dadosP)], 
                  y = dadosP$alc, 
                  method = "nb", 
                  trControl = trainControl(method = "cv", number = 10)
  ) 
  return (fit_nv)
}


source(file_path_as_absolute("classificador_default.R"))
