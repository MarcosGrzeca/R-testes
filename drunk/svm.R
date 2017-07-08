library(tools)

PATH_FIT <- "resultados/svm/fit4.Rda"
PATH_PRED <- "resultados/svm/pred4.Rda"
PATH_IMAGE <- "resultados/svm/image4.RData"

load("rda/alemao_base_completa.Rda")

print("SVM")

library(caret)

trainAlgoritmo <- function(dadosP) {
  fit_nv <- train(x = dadosP,
                  y = dadosP$alc, 
                  method = "svmPoly", 
                  trControl = trainControl(method = "cv", number = 10, classProbs =  TRUE)
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

if (!require("doParallel")) {
  install.packages("doParallel")
}
library(doParallel); 
cl <- makeCluster(3); 
registerDoParallel(cl);

library(caret)
load(PATH_FIT)
fit

load(PATH_PRED)
pred <- predict(fit, dadosFinal)

set.seed(7)
inTrain <- createDataPartition(y = dadosFinal$alc, p = .80, list = FALSE)
training <- dadosFinal[ inTrain,]
testing <- dadosFinal[-inTrain,]

pred <- predict(fit, testing)
predict(fit, type = "prob")

predict(fit, newdata = dadosFinal)
predict(fit, newdata = subset(dadosFinal, select = -c(alc)))
