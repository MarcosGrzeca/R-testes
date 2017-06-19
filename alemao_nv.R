#Carregar bibliotecas
options(max.print = 99999999)

#Carrega functions
DIRETORIO = "C:\\Users\\Marcos\\Documents\\GitHub\\R-testes\\"
source(file=paste(DIRETORIO,"functions.R", sep = ""))

#Configurações
DATABASE <- "alemao"
clearConsole();
dados <- query("SELECT id, sex, age, aak, bak, wea, texto, alc FROM conversa")
dados$alc[dados$alc == "cna"] <- "na"
dados$alc <- as.factor(dados$alc)
dados <- as.data.frame(unclass(dados))

str(dados)

clearConsole()


library('caret')
train_control <- trainControl(method="cv", number=10)
fit <- train(dados, dados$alc, method = "nb", trControl=train_control)
fit

pred <- predict(fit, newdata=dados)
table("Predictions"= pred,  "Actual" = dados$alc )
conf.mat <- confusionMatrix(pred, dados$alc)
conf.mat
