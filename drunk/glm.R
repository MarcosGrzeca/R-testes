library(tools)
PATH_FIT <- "rda/glm/fit.Rda"
PATH_PRED <- "rda/glm/bh_pred.Rda"
PATH_IMAGE <- "rda/glm/nv.RData"

load("rda/alemao_base_completa.Rda")

print("GLM")

library(caret)

trainAlgoritmo <- function(dadosP) {
  fit_nv <- train(x = dadosP[,2:ncol(dadosP)], 
                  y = dadosP$alc, 
                  method = "glm", 
                  family="binomial",
                  trControl = trainControl(method = "cv", number = 10)
  ) 
  return (fit_nv)
}

source(file_path_as_absolute("classificador_default.R"))
