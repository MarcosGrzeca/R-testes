options(max.print = 99999999)
#Carrega functions
library(tools)
source(file_path_as_absolute("functions.R"))

#Configuracoes
DATABASE <- "alemao"
clearConsole();
dados <- query("SELECT id, texto, alc, repetitions, longpauses FROM conversa WHERE type = 'D'")

dados$alc[dados$alc == "cna"] <- "na"
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

vocab = create_vocabulary(it_train, stopwords = stop_words)

vectorizer = vocab_vectorizer(vocab)
dtm_train = create_dtm(it_train, vectorizer)
dataM <- as.data.frame(as.matrix(dtm_train))

convert_count <- function(x) {
  y <- ifelse(x > 0, 1,0)
  #y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y <- factor(y, levels=c(0,1), labels=c(0, 1))
  y
}

if (!require("doParallel")) {
  install.packages("doParallel")
}
library(doParallel); 
cl <- makeCluster(5); 
registerDoParallel(cl);
#teste com word count
dataM <- apply(dataM, 2, convert_count)
dadosFinal <- subset(dados, select = -c(texto, id) )

if (!require("rowr")) {
  install.packages("rowr")
}
library(rowr)

dadosFinal <- cbind.fill(dadosFinal, dataM)
save(dadosFinal, file="alemao_dialogo.Rda")

print("NV")
library(caret)
trainAlgoritmo <- function(dadosP) {
  fit_nv <- train(x = subset(dadosP, select = -c(alc)),
                  y = dadosP$alc,
                  method = "nb", 
                  trControl = trainControl(method = "cv", number = 10)
  ) 
  return (fit_nv)
}

print("Treinando")
fit <- trainAlgoritmo(dadosFinal)
fit
#usekernel  Accuracy   Kappa    
#FALSE      0.7302515  0.4055699
#TRUE      0.7284738  0.4026617

print("Treinei")
save(fit, file="fit_alemao_dialogo.Rda")

print("Prevendo")
bh_pred <- predict(fit, dadosFinal)
a <- table(bh_pred, dadosFinal$alc)
print(a)
stopCluster(cl);

if (!require("openNLP")) {
  install.packages("openNLP")
}
require("openNLP")

if (!require("NLP")) {
  install.packages("NLP")
}
require("NLP")



library(rJava)
install.packages(c("NLP", "openNLP", "RWeka", "qdap"))
