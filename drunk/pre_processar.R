#Carregar bibliotecas
print("glm")
options(max.print = 99999999)

#https://cran.r-project.org/web/packages/text2vec/vignettes/text-vectorization.html
#Pre-processar texto https://rpubs.com/MajstorMaestro/256588

#Carrega functions
library(tools)
source(file_path_as_absolute("functions.R"))

#Configuracoes
DATABASE <- "alemao"
clearConsole();
#dados <- query("(SELECT id, texto, alc, repetitions, longpauses FROM conversa WHERE alc = 'a' LIMIT 2000) UNION (SELECT id, texto, alc, repetitions, longpauses FROM conversa WHERE alc = 'na' LIMIT 2000)")

#dados <- query("SELECT id, texto, alc, repetitions, longpauses FROM conversa")
#dados$alc[dados$alc == "cna"] <- "na"
#dados$alc <- as.factor(dados$alc)
#dados$texto <- gsub('"', 'aspas', dados$texto)

#save(dados, file="database.Rda")
load("database.Rda")

dados$repetitions[dados$repetitions > 0] <- 1
dados$repetitions[dados$repetitions == 0] <- 0

dados$longpauses[dados$longpauses > 0] <- 1
dados$longpauses[dados$longpauses == 0] <- 0

clearConsole()

library(text2vec)
library(data.table)

setDT(dados)
setkey(dados, id)

prep_fun = tolower
tok_fun = word_tokenizer

stop_words = tm::stopwords("de")

it_train = itoken(dados$texto, 
                  preprocessor = prep_fun, 
                  tokenizer = tok_fun, 
                  ids = dados$id, 
                  progressbar = TRUE)

#vocab = create_vocabulary(it_train, ngram = c(1L, 2L), stopwords = stop_words)
vocab = create_vocabulary(it_train, stopwords = stop_words)

#pruned_vocab = prune_vocabulary(vocab, 
#                                term_count_min = 10, 
#                                doc_proportion_max = 0.5,
#                                doc_proportion_min = 0.001)
#vectorizer = vocab_vectorizer(pruned_vocab)

vectorizer = vocab_vectorizer(vocab)
dtm_train = create_dtm(it_train, vectorizer)
dataM <- as.data.frame(as.matrix(dtm_train))

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