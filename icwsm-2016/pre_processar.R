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
dados <- query("SELECT id, q1, q2, q3, textParser, hashtags, emoticonPos, emoticonNeg FROM tweets WHERE situacao = 'S'")

dados$q1 <- as.factor(dados$q1)
dados$q2 <- as.factor(dados$q2)
dados$q3 <- as.factor(dados$q3)

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

vocab = create_vocabulary(it_train, ngram = c(3L, 3L))
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

save(maFinal, file="icwsm-2016.Rda")