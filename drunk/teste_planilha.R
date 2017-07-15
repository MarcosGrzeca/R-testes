#options(max.print = 99999999)
#Carrega functions
library(tools)
source(file_path_as_absolute("functions.R"))

FILE_READ <- "testes/data_my.Rda"
FIT_SAVE <- "testes/fit_data_my.Rda"

#dat = read.csv("teste/data_no.csv", header = TRUE)
#save(dat, file = FILE_READ)
load(FILE_READ)

if (!require("doParallel")) {
  install.packages("doParallel")
}
library(doParallel); 
cl <- makeCluster(8); 

dadosFinal <- subset(dat, select = -c(id) )

if (!require("rowr")) {
  install.packages("rowr")
}
library(rowr)

print("NV")
library(caret)
trainAlgoritmo <- function(dadosP) {
  fit_nv <- train(x = subset(dadosP, select = -c(alc)),
                  y = dadosP$alc,
                  method = "nb", 
                  trControl = trainControl(method = "cv", number = 10)
  ) 
  return (fit_nv)
}

print("Treinando")
fit <- trainAlgoritmo(dadosFinal)
fit
#usekernel  Accuracy   Kappa    
#FALSE      0.7302515  0.4055699
#TRUE      0.7284738  0.4026617

print("Treinei")
save(fit, file=FIT_SAVE)

print("Prevendo")
bh_pred <- predict(fit, dadosFinal)
a <- table(bh_pred, dadosFinal$alc)
print(a)
stopCluster(cl)