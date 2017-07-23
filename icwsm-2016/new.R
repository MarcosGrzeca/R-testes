load(file="denovo_99_tntando.Rda")

library(tools)
library(caret)

library(doMC)

registerDoMC(6)

#dim(maFinal)
#nzv <- nearZeroVar(maFinal)
#nzv
#maFinal <- maFinal[, -nzv]
#dim(maFinal)


set.seed(10)
split=0.70
trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
data_test <- maFinal[-trainIndex,]


print("Treinando")
fit <- train(x = subset(data_train, select = -c(resposta)),
             y = data_train$resposta, 
             method = "svmLinear", 
             trControl = trainControl(method = "cv", number = 5, savePred=T)
             #,preProc=c("center", "scale", "nzv")
) 
fit

#if (!require("mlbench")) {
#  install.packages("mlbench")
#}

#library(mlbench)
#importance <- varImp(fit, scale=FALSE)
#head(importance, top = 40)
#plot(importance, top = 40)

#save.image(file="bora20.Rda")
#load("bora20.Rda")

#data_test$Class <- as.character(data_test$resposta)

#aa <- predict(fit, subset(maFinal, select = -c(resposta)))
#confusionMatrix(data = aa, maFinal$resposta)
aa <- predict(fit, subset(data_test, select = -c(resposta)))
confusionMatrix(data = aa, data_test$resposta, positive="1")
