library(caret)

if (!require("doParallel")) {
  install.packages("doParallel")
}
library(doParallel)

cl<-makeCluster(3)
registerDoParallel(cl)

print("Treinando")
fit <- trainAlgoritmo(dadosFinal)
save(fit, file=PATH_FIT)

print("Prevendo")

bh_pred <- predict(fit, dadosFinal)
save(bh_pred, file=PATH_PRED)

print("Resultados")

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

save.image(file=PATH_IMAGE)
stopCluster(cl)