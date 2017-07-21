load(file="denovo_99.Rda")

library(tools)
library(caret)

if (!require("doMC")) {
  install.packages("doMC")
}
library(doMC)

registerDoMC(8)

print("Treinando")
fit <- train(x = subset(maFinal, select = -c(resposta)),
             y = maFinal$resposta, 
             method = "svmRadial", 
             trControl = trainControl(method = "cv", number = 5)
) 
fit

if (!require("mlbench")) {
  install.packages("mlbench")
}
library(mlbench)
importance <- varImp(fit, scale=FALSE)
head(importance)
plot(importance, top = 40)

save.image(file="bora.Rda")