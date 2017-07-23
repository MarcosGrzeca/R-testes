#Carregar bibliotecas
options(max.print = 99999999)

#Carrega functions
library(tools)
source(file_path_as_absolute("functions.R"))

#Configuracoes
DATABASE <- "icwsm-2016"
clearConsole();
dadosQ1 <- query("SELECT id, q1 as resposta, textParser, textoParserEmoticom as textoCompleto, hashtags, emoticonPos, emoticonNeg, sentiment, sentimentH FROM tweets WHERE situacao = 'S'")
dados <- dadosQ1
dados$resposta[is.na(dados$resposta)] <- 0
dados$resposta <- as.factor(dados$resposta)
clearConsole()

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
#                  tokenizer = stem_tokenizer1,
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

library(rowr)

#sentimentos
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



maFinal <- cbind.fill(dados, dataFrameTexto)
maFinal <- cbind.fill(maFinal, dataFrameHash)
maFinal <- subset(maFinal, select = -c(textParser, id, hashtags, textoCompleto))
maFinal <- subset(maFinal, select = -c(sentiment, sentimentH, emotiomH))


FILE <- "exp1_bag_sentiment.Rda"
save(maFinal, file=FILE)
load(FILE)
library(tools)
library(caret)

if (!require("doMC")) {
  install.packages("doMC")
}
library(doMC)

registerDoMC(3)


set.seed(10)
split=0.80
trainIndex <- createDataPartition(maFinal$resposta, p=split, list=FALSE)
data_train <- as.data.frame(unclass(maFinal[ trainIndex,]))
data_test <- maFinal[-trainIndex,]

print("Treinando")
fit <- train(x = subset(data_train, select = -c(resposta)),
             y = data_train$resposta, 
             method = "svmLinear", 
             trControl = trainControl(method = "cv", number = 5, savePred=T)
             #,preProc=c("center", "scale", "nzv")
) 
fit

library(mlbench)

#data_test <- as.data.frame(unclass(data_test))
pred <- predict(fit, subset(data_test, select = -c(resposta)))
confusionMatrix(data = pred, data_test$resposta, positive="1")