library(tools)

PATH_FIT <- "resultados/svm/fit2.Rda"
PATH_PRED <- "resultados/svm/pred2.Rda"
PATH_IMAGE <- "resultados/svm/image2.RData"

load("rda/alemao_base_completa.Rda")

print("SVM")

library(caret)

trainAlgoritmo <- function(dadosP) {
  fit_nv <- train(x = subset(dadosP, select = -c(alc)),
                  y = dadosP$alc, 
                  method = "svmRadial", 
                  trControl = trainControl(method = "cv", number = 10)
  ) 
  return (fit_nv)
}



source(file_path_as_absolute("classificador_default.R"))

#load(PATH_FIT)
#importantes(fit)

teste <- function() {
  print("Resultados")
  a <- table(bh_pred, dadosFinal$alc)
  a
  uarA <- a[1,1] / (a[1,1] + a[2,1])
  uarNA <- a[2,2] / (a[2,2] + a[1,2])
  if (uarNA == "NaN"){
    uarNA = 0
  }
  uar = (uarA + uarNA) / 2
  uar
}