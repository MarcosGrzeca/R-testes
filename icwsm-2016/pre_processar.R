#Carregar bibliotecas
options(max.print = 99999999)

#Constantes
CORES <- 4

#Carrega functions
library(tools)
source(file_path_as_absolute("functions.R"))

#Configuracoes
DATABASE <- "icwsm-2016"
clearConsole();
dadosQ1 <- query("SELECT id, q1 as resposta, textParser, hashtags, emoticonPos, emoticonNeg FROM tweets WHERE situacao = 'S'")
#dadosQ2 <- query("SELECT id, q2 as resposta, textParser, hashtags, emoticonPos, emoticonNeg FROM tweets WHERE situacao = 'S' AND q1 = '1' AND q2 IS NOT NULL")
#dadosQ3 <- query("SELECT id, q3 as resposta, textParser, hashtags, emoticonPos, emoticonNeg FROM tweets WHERE situacao = 'S' AND q2 = '1' AND q3 IS NOT NULL")

dados <- dadosQ1
dados$resposta[is.na(dados$resposta)] <- 0
dados$resposta <- as.factor(dados$resposta)
dados$emoticonPos[dados$emoticonPos > 0] <- 1
dados$emoticonPos[dados$emoticonPos == 0] <- 0
dados$emoticonNeg[dados$emoticonNeg > 0] <- 1
dados$emoticonNeg[dados$emoticonNeg == 0] <- 0
clearConsole()

if (!require("text2vec")) {
  install.packages("text2vec")
}
library(text2vec)
library(data.table)

setDT(dados)
setkey(dados, id)

prep_fun = tolower
tok_fun = word_tokenizer

it_train = itoken(dados$textParser, 
                  preprocessor = prep_fun, 
                  tokenizer = tok_fun, 
                  ids = dados$id, 
                  progressbar = TRUE)

vocab = create_vocabulary(it_train, ngram = c(2L, 2L))
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

dataFrameHashTag <- as.data.frame(as.matrix(dtm_train_hash_tags))

cols <- colnames(dataTexto)
aspectos <- sort(colSums(dataTexto), decreasing = TRUE)
manter <- round(length(aspectos) * 0.25)
aspectosManter <- c()
aspectosRemover <- c()

for(i in 1:length(aspectos)) {
  if (i <= manter) {
    aspectosManter <- c(aspectosManter, aspectos[i])
  } else {
    aspectosRemover <- c(aspectosRemover, aspectos[i])
  }
}
dataFrameTexto <- dataFrameTexto[names(aspectosManter)]
clearConsole()

if (!require("doMC")) {
  install.packages("doMC")
}
library(doMC)

registerDoMC(CORES)

if (!require("rowr")) {
  install.packages("rowr")
}
library(rowr)

maFinal <- cbind.fill(dados, dataFrameTexto)
maFinal <- cbind.fill(maFinal, dataFrameHashTag)
maFinal <- subset(maFinal, select = -c(textParser, id, hashtags) )

#id, q1, q2, q3, textParser, hashtags
save(maFinal, file="icwsm-2016_2.Rda")

library(tools)
library(caret)

set.seed(7)
inTrain <- createDataPartition(y = maFinal$resposta, p = .80, list = FALSE)
training <- maFinal[ inTrain,]
testing <- maFinal[-inTrain,]

if (!require("doMC")) {
  install.packages("doMC")
}
library(doMC)

registerDoMC(4)

print("Treinando")
fit <- train(x = subset(training, select = -c(resposta)),
                y = training$resposta, 
                method = "svmPoly", 
                trControl = trainControl(method = "cv", number = 5)
) 



fit
save(fit, file="resultados/fit2.Rda")
#load("resultados/fit.Rda")

print("Prevendo")

predicao <- predict(fit, subset(testing, select = -c(resposta)))
predicao
save(predicao, file="resultados/pred2.Rda")
#load("resultados/pred.Rda")

print("Resultados")
summary(testing$resposta)
a <- table(predicao, testing$resposta)
a

precisao <- a[2,2]  / (a[2,2] + a[1,2])
print(paste("Precision", precisao, sep=" "))

revocacao <- a[2,2] / (a[2,2] + a[2,1])
print(paste("Revocação", revocacao, sep=" "))

f1 <- (2*precisao*revocacao)/(precisao+revocacao)
print(paste("F1", f1, sep=" "))
save.image(file="resultados/image_q3.RData")

#results <- resamples(list(SVM=fit))
# collect resamples
#results <- resamples(list(LVQ=modelLvq, GBM=modelGbm, SVM=modelSvm))
# summarize the distributions
#summary(results)
# boxplots of results
#bwplot(results)
# dot plots of results
#dotplot(results)