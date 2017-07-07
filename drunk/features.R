library(tools)

#PATH_FIT <- "resultados/svm/fit.Rda"
#PATH_PRED <- "resultados/svm/pred.Rda"
#PATH_IMAGE <- "resultados/svm/image.RData"

load("rda/alemao_base_completa.Rda")

# load the library
if (!require("mlbench")) {
  install.packages("mlbench")
}

# ensure results are repeatable
set.seed(7)
# load the library
library(mlbench)
library(caret)
# load the dataset
# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(x = subset(dadosFinal, select = -c(alc)),
               y = dadosFinal$alc,
               method="svmRadial", preProcess="scale", trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)