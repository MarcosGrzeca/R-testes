load(file="denovo_99_tntando.Rda")

library(tools)
library(caret)

library(doMC)

registerDoMC(6)

colnames(maFinal)

maFinal = subset(maFinal, select = -c(emoticonPos, emoticonNeg, emotiom, emotiomH, personCount, localCount, organizationCount, moneyCount))

#set.seed(10)
#split=0.80
#trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
#data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
#data_test <- maFinal[-trainIndex,]


print("Treinando")
fit <- train(x = subset(maFinal, select = -c(resposta)),
             y = maFinal$resposta, 
             method = "svmLinear", 
             trControl = trainControl(method = "cv", number = 10, savePred=T)
) 
#dentro do tr metric = "ROC"
#,preProc=c("center", "scale", "nzv")
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
aa <- predict(fit, subset(maFinal, select = -c(resposta)))
confusionMatrix(data = aa, maFinal$resposta, positive="1")


trellis.par.set(caretTheme())
plot(fit) 
#plot(gbmFit2, metric = "Kappa")


##Comparar os modelos
#resamps <- resamples(list(GBM = gbmFit3,
#                          SVM = svmFit,
#                          RDA = rdaFit))
#resamps

#trellis.par.set(theme1)
#bwplot(resamps, layout = c(3, 1))

#trellis.par.set(caretTheme())
#dotplot(resamps, metric = "ROC")