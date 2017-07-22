options(max.print = 99999999)

#Carrega functions
library(tools)
source(file_path_as_absolute("functions.R"))

#Configuracoes
DATABASE <- "icwsm-2016"
clearConsole();
dadosQ1 <- query("SELECT id, q1 as resposta, textParser, hashtags, emoticonPos, emoticonNeg, personCount, localCount, organizationCount, moneyCount, sentiment, sentimentH FROM tweets")
dados <- dadosQ1
dados$resposta[is.na(dados$resposta)] <- 0
dados$resposta <- as.factor(dados$resposta)
clearConsole()

summary(dados$sentimentH)

summary(dados$sentiment)
desvio <- sd(dados$sentiment)
desvioH <- sd(dados$sentimentH)

dados$emotiom <- 0
dados$emotiom[dados$sentiment < 0] <- -1
dados$emotiom[dados$sentiment < -0.33] <- -2
dados$emotiom[dados$sentiment < -0.66] <- -3
dados$emotiom[dados$sentiment > 0] <- 1
dados$emotiom[dados$sentiment > 0.33] <- 2
dados$emotiom[dados$sentiment > 0.66] <- 3

dados$emotiomH <- 0
dados$emotiomH[dados$sentimentH < 0] <- -1
dados$emotiomH[dados$sentimentH < -0.5] <- -2
dados$emotiomH[dados$sentimentH > 0] <- 1
dados$emotiomH[dados$sentimentH > 0.5] <- 2

if (!require("text2vec")) {
  install.packages("text2vec")
}
library(text2vec)
library(data.table)
library(SnowballC)

setDT(dados)
setkey(dados, id)

stem_tokenizer1 =function(x) {
  tokens = word_tokenizer(x)
  lapply(tokens, SnowballC::wordStem, language="en")
}

prep_fun = tolower
tok_fun = word_tokenizer

it_train = itoken(dados$textParser, 
                  preprocessor = prep_fun, 
                  #tokenizer = stem_tokenizer1,
                  tokenizer = tok_fun,
                  ids = dados$id, 
                  progressbar = TRUE)
stop_words = tm::stopwords("en")
vocab = create_vocabulary(it_train, stopwords = stop_words)
vectorizer = vocab_vectorizer(vocab)
dtm_train_texto = create_dtm(it_train, vectorizer)

it_train = itoken(dados$hashtags, 
                  preprocessor = prep_fun, 
                  tokenizer = tok_fun, 
                  ids = dados$id, 
                  progressbar = TRUE)

vocabHashTags = create_vocabulary(it_train)
vectorizerHashTags = vocab_vectorizer(vocabHashTags)
dtm_train_hash_tags = create_dtm(it_train, vectorizerHashTags)

dataTexto <- as.matrix(dtm_train_texto)
dataFrameTexto <- as.data.frame(as.matrix(dtm_train_texto))
dataFrameHash <- as.data.frame(as.matrix(dtm_train_hash_tags))
clearConsole()

if (!require("doMC")) {
  install.packages("doMC")
}
library(doMC)

registerDoMC(4)

if (!require("rowr")) {
  install.packages("rowr")
}
library(rowr)

maFinal <- cbind.fill(dados, dataFrameTexto)
maFinal <- cbind.fill(maFinal, dataFrameHash)
maFinal <- subset(maFinal, select = -c(textParser, id, hashtags, sentiment, sentimentH))

save(maFinal, file="denovo_99_tntando.Rda")

library(tools)
library(caret)

if (!require("doMC")) {
  install.packages("doMC")
}
library(doMC)

registerDoMC(4)

print("Treinando")
fit <- train(x = subset(maFinal, select = -c(resposta)),
             y = maFinal$resposta, 
             method = "svmRadial", 
             trControl = trainControl(method = "cv", number = 5)
) 
fit

if (!require("mlbench")) {
  install.packages("mlbench")
}
library(mlbench)
importance <- varImp(fit, scale=FALSE)
head(importance)
#print(importance)
plot(importance, top = 40)