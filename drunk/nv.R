library(tools)
PATH_FIT <- "resultados/nv/fit.Rda"
PATH_PRED <- "resultados/nv/pred.Rda"
PATH_IMAGE <- "resultados/nv/nv.RData"

load("rda/alemao_base_completa.Rda")

print("Naive Bayes")

library(caret)

trainAlgoritmo <- function(dadosP) {
  fit_nv <- train(x = subset(dadosP, select = -c(alc)),
                  y = dadosP$alc,
                  method = "nb", 
                  trControl = trainControl(method = "cv", number = 10)
  ) 
  return (fit_nv)
}


source(file_path_as_absolute("classificador_default.R"))

teste <- function() {

  library(caret)
  
  if (!require("doMC")) {
    install.packages("doMC")
  }
  library(doMC)
  
  registerDoMC(4)
  
  print("Treinando")
  fit <- trainAlgoritmo(dadosFinal)
  save(fit, file=PATH_FIT)
  #load(PATH_FIT)
  
  print("Prevendo")
  
  bh_pred <- predict(fit, dadosFinal)
  save(bh_pred, file=PATH_PRED)
  #load(PATH_PRED)
  
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
  
  save.image(file=PATH_IMAGE)
}
#stopCluster(cl)
