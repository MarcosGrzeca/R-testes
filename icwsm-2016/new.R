load(file="denovo_99_tntando.Rda")

library(tools)
library(caret)
library(doMC)

registerDoMC(8)

split=0.75
trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
data_train <- maFinal[ trainIndex,]
data_test <- maFinal[-trainIndex,]


print("Treinando")
fit <- train(x = subset(data_train, select = -c(resposta)),
             y = data_train$resposta, 
             method = "nb", 
             trControl = trainControl(method = "cv", number = 10)
) 
fit

if (!require("mlbench")) {
  install.packages("mlbench")
}
library(mlbench)
importance <- varImp(fit, scale=FALSE)
head(importance, top = 40)
plot(importance, top = 40)

save.image(file="bora20.Rda")