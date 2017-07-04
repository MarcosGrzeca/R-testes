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
vocab

if (!require("qdap")) {
  install.packages("qdap")
}

library(qdap)
frequent_terms <- freq_terms(dados$textParser,20)
frequent_terms <- freq_terms(vocab, 3)
frequent_terms
vocab


#pruned_vocab = prune_vocabulary(vocab,
                                #term_count_min = 10, 
                                #doc_proportion_max = 0.5,
                                #doc_proportion_min = 0.25
                                #max_number_of_terms = 200)
vectorizer = vocab_vectorizer(frequent_terms)
vectorizer

dtm_train = create_dtm(it_train, vectorizer)

ncol(dtm_train)
nrow(dtm_train)
dtm_train

dataM <- as.data.frame(as.matrix(dtm_train))
view(dataM)

convert_count <- function(x) {
  y <- ifelse(x > 0, 1,0)
  #y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  #y <- factor(y, levels=c(0,1), labels=c(0, 1))
  y
}
#teste com word count
#dataM <- apply(dataM, 2, convert_count)

dadosFinal <- subset(dados, select = -c(texto, id) )

#dump(dadosFinal, "exp1_alc.csv")
dump(dataM, "exp1_bag.csv")
dadosFinal <- read.csv(file="base_completa/exp1_bag.csv", header=TRUE, sep=",")
save(dadosFinal, file="alemao_base_completa.Rda")

#EXTREMAMENTE LENTO
#cols <- colnames(dataM)
#FAZER NO BRAÇO
#for(i in 1:nrow(dataM)) {
#  for(j in 1:ncol(dataM)) {
#    dadosFinal[i][[cols[j]]] <- dataM[i, j]
#  }
#}

#for(i in 1:ncol(dataM)) {
  #dadosFinal[[cols[i]]] <- as.integer(dadosFinal[[cols[i]]])
#}

#save(dadosFinal, file="alemao_bag_processado.Rda")
#load("alemao.Rda")

print("SVM")
source(file_path_as_absolute("classificadores.R"))
final <- classificar(dadosFinal)