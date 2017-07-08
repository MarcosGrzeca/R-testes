if (!require("doParallel")) {
  install.packages("doParallel")
}
library(doParallel); 
cl <- makeCluster(6); 
registerDoParallel(cl);

library(caret)

#if (!require("doMC")) {
#  install.packages("doMC")
#}
#library(doMC)

#registerDoMC(cores = detectCores())

print("Treinando")
fit <- trainAlgoritmo(dadosFinal)
print("Treinei")
save(fit, file=PATH_FIT)

print("Prevendo")

bh_pred <- predict(fit, dadosFinal)
save(bh_pred, file=PATH_PRED)

print("Resultados")
a <- table(bh_pred, dadosFinal$alc)
print(a)
uarA <- a[1,1] / (a[1,1] + a[2,1])
uarNA <- a[2,2] / (a[2,2] + a[1,2])
if (uarNA == "NaN"){
  uarNA = 0
}
uar = (uarA + uarNA) / 2
uar

stopCluster(cl);
save.image(file=PATH_IMAGE)