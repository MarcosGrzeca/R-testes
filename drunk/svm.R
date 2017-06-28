library(tools)

load("alemao_base_completa.Rda")

print("SVM")

library(caret)

svm_train <- function(dadosP) {
  fit_nv <- train(x = dadosP[,2:ncol(dadosP)], 
                  y = dadosP$alc, 
                  method = "svmRadial", 
                  trControl = trainControl(method = "cv", number = 10)
  ) 
  return (fit_nv)
}

fit <- svm_train(dadosFinal)
save(fit, file="fit_svm.Rda")

bh_pred <- predict(fit, dadosFinal)
save(bh_pred, file="bh_pred_svm.Rda")

precision <- posPredValue(bh_pred, dadosFinal$alc)
print(paste("Precision", precision, sep=" "))
recall <- sensitivity(bh_pred, dadosFinal$alc)
print(paste("Recall", recall, sep=" "))
F1 <- (2 * precision * recall) / (precision + recall)
print(paste("F1", F1, sep=" "))

a <- table(bh_pred, dadosFinal$alc)

uarA <- a[1,1] / (a[1,1] + a[1,2])
uarNA <- a[2,2] / (a[2,2] + a[2,1])
if (uarNA == "NaN"){
  uarNA = 0
}
uar = (uarA + uarNA) / 2
uar

#TP FP
#FN TN


save.image(file="svm.RData")