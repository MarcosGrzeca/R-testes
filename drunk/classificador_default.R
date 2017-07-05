library(caret)

if (!require("doMC")) {
  install.packages("doMC")
}
library(doMC)

registerDoMC(4)

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


print("Resultados MARCOS")
precisao <- a[2,2]  / (a[2,2] + a[1,2])
print(paste("Precision", precisao, sep=" "))

revocacao <- a[2,2] / (a[2,2] + a[2,1])
print(paste("Revocação", revocacao, sep=" "))

f1 <- (2*precisao*revocacao)/(precisao+revocacao)
print(paste("F1", f1, sep=" "))
save.image(file="resultados/image_q2.RData")

save.image(file=PATH_IMAGE)
#stopCluster(cl)