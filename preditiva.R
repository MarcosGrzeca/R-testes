#NaiveBayes
library(e1071)

analisePreditiva <- function() {
  #Naive Bayes
  modelo <- naiveBayes(dados[1:21], dados[,22])
  dados$mediaPrevista <- predict(modelo, dados, type="class")
  
  clearConsole();
  matriz <- matrix(data=0,nrow=3,ncol=3)
  for(i in 1:nrow(dados)) {
    row <- dados[i,]
    matriz[row$media, row$mediaPrevista] <- matriz[row$media, row$mediaPrevista] + 1
  }
  matriz
}

library(e1071)
library('caret')

train_control <- trainControl(method="cv", number=10)
model <- train(dados[1:15], dados$media, method = "nb", trControl=train_control)



tune.control <- tune.control(random =FALSE, nrepeat=1, repeat.aggregate=mean,sampling=c("cross"),sampling.aggregate=mean, cross=10, best.model=TRUE, performances=TRUE)

tune.control <-  tune.control(random = FALSE, nrepeat = 1, repeat.aggregate = mean,
                              sampling = c("cross", "fix", "bootstrap"), sampling.aggregate = mean,
                              sampling.dispersion = sd,
                              cross = 10, fix = 2/3, nboot = 10, boot.size = 9/10, best.model = TRUE,
                              performances = TRUE, error.fun = NULL) 
str(dados)
modelo_nb <- naiveBayes(dados[1:14], dados$media, tune.control)
test1$predict_nb <- predict(modelo_nb, test1, type="class")



# load the library
library(caret)
# load the iris dataset
data(iris)
# define training control
train_control <- trainControl(method="boot", number=100)
# train the model
model <- train(Species~., data=iris, trControl=train_control, method="nb")
# summarize results
print(model)

str(dadosTeste)
dadosTeste <- dados
dadosTeste$IN_TP_ENSINO[dadosTeste$IN_TP_ENSINO == ""] <- NA
dadosTeste$media[dadosTeste$media == ""] <- "D"

train_control <- trainControl(method="cv", number=10)
model <- train(media~., data=dadosTeste, method = "nb", trControl=train_control, na.remove = TRUE)
print(model)



# load the libraries
library(caret)
library(klaR)
# load the iris dataset
data(iris)
# define an 80%/20% train/test split of the dataset
split=0.80
trainIndex <- createDataPartition(dadosTeste$media, p=split, list=FALSE)
data_train <- dadosTeste[ trainIndex,]
data_test <- dadosTeste[-trainIndex,]
# train a naive bayes model
model <- NaiveBayes(media~., data=dadosTesste)
# make predictions
x_test <- data_test[,1:15]
y_test <- data_test[,16]
predictions <- predict(model, x_test)
# summarize results
confusionMatrix(predictions$class, y_test)

library(caret)
# load the iris dataset
data(iris)
# define training control
train_control <- trainControl(method="cv", number=10)
# fix the parameters of the algorithm
grid <- expand.grid(.fL=c(0), .usekernel=c(TRUE))
# train the model
model <- train(media~., data=dadosTeste, trControl=train_control, method="nb", type = "class", na.action = na.pass)
# summarize results
print(model)
