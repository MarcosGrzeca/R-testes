#URL base
#https://rpubs.com/cen0te/naivebayes-sentimentpolarity

#Carregar bibliotecas
options(max.print = 99999999)
#Carrega functions
DIRETORIO = "C:\\Users\\Marcos\\Documents\\GitHub\\R-testes\\"
source(file=paste(DIRETORIO,"functions.R", sep = ""))

#Carregar dados
DATABASE <- "alemao"
clearConsole();
dados <- query("SELECT id, texto, alc FROM conversa LIMIT 500")
dados$alc[dados$alc == "cna"] <- "na"
dados$alc <- as.factor(dados$alc)
#dados$id <- as.integer(dados$id)
#dados <- as.data.frame(unclass(dados))

#Bag of words
library(tm)
library(RTextTools)
library(e1071)
library(dplyr)
library(caret)

# Library for parallel processing
#install.packages("doMC", repos="http://R-Forge.R-project.org")
library(doMC)
registerDoMC(cores=detectCores())  # Use all available cores
clearConsole()

glimpse(dados)

corpus <- Corpus(VectorSource(dados$texto))

#limpeza dos dados
corpus <- corpus %>%
  tm_map(content_transformer(tolower)) %>% 
  tm_map(removePunctuation) %>%
  #tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords(kind="de")) %>%
  tm_map(stripWhitespace)

dtm <- DocumentTermMatrix(corpus)

fivefreq <- findFreqTerms(dtm, 1)
length((fivefreq))

dtm <- DocumentTermMatrix(corpus, control=list(dictionary = fivefreq))

convert_count <- function(x) {
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y
}
trainNB <- apply(dtm, 2, convert_count)
trainNB[1:50, 1:2]

print("Resultado Final")
initFileLog("resultalemao.txt")
trainNB
finishFileLog("resultalemao.txt")

dumpArrf(trainNB, "dump_alemao.csv")

nrow(trainNB)
nrow(dados)

library('caret')
train_control <- trainControl(method="cv", number=10)
fit <- train(trainNB, dados$alc, method = "nb", trControl=train_control)
warnings()
fit

pred <- predict(fit, newdata=dados)
table("Predictions"= pred,  "Actual" = dados$alc )
conf.mat <- confusionMatrix(pred, dados$alc)
conf.mat
