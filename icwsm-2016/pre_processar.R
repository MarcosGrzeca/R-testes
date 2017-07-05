#Carregar bibliotecas
options(max.print = 99999999)

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
dtm_train = create_dtm(it_train, vectorizer)

ncol(dtm_train)
nrow(dtm_train)
dtm_train

data <- as.matrix(dtm_train)
dataFrame <- as.data.frame(as.matrix(dtm_train))
view(data)

#EXTREMAMENTE LENTO
cols <- colnames(data)
#FAZER NO BRAÇO
#for(i in 1:nrow(dataM)) {
#  for(j in 1:ncol(dataM)) {
#    dadosFinal[i][[cols[j]]] <- dataM[i, j]
#  }
#}


aspectos <- sort(colSums(data), decreasing = TRUE)
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
aspectosManter

#save(dadosFinal, file="alemao_bag_processado.Rda")
#load("alemao.Rda")

print("SVM")
source(file_path_as_absolute("classificadores.R"))
final <- classificar(dadosFinal)


if (!require("quanteda")) {
  install.packages("quanteda")
}
library(quanteda)

if (!require("qdap")) {
  install.packages("qdap")
}
library(qdap)
frequent_terms <- freq_terms(dados$textParser,20)
vocab
frequent_terms <- freq_terms(vocab, 3)
frequent_terms
