#Carregar bibliotecas
options(max.print = 99999999)

#https://cran.r-project.org/web/packages/text2vec/vignettes/text-vectorization.html
#Pre-processar texto https://rpubs.com/MajstorMaestro/256588

#Carrega functions
DIRETORIO = "C:\\Users\\Marcos\\Documents\\GitHub\\R-testes\\"
source(file=paste(DIRETORIO,"functions.R", sep = ""))

#Configuracoes
DATABASE <- "alemao"
clearConsole();
dados <- query("SELECT id, texto, alc, repetitions, longpauses FROM conversa LIMIT 5000")
dados$alc[dados$alc == "cna"] <- "na"
dados$alc <- as.factor(dados$alc)

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

stop_words = c("aber", "als", "am", "an", "auch", "auf", "aus", "bei", "bin", "bis", "bist", "da", "dadurch", "daher", "darum", "das", "daß", "dass", "dein", "deine", "dem", "den", "der", "des", "dessen", "deshalb", "die", "dies", "dieser", "dieses", "doch", "dort", "du", "durch", "ein", "eine", "einem", "einen", "einer", "eines", "er", "es", "euer", "eure", "für", "hatte", "hatten", "hattest", "hattet", "hier", "hinter", "ich", "ihr", "ihre", "im", "in", "ist", "ja", "jede", "jedem", "jeden", "jeder", "jedes", "jener", "jenes", "jetzt", "kann", "kannst", "können", "könnt", "machen", "mein", "meine", "mit", "muß", "mußt", "musst", "müssen", "müßt", "nach", "nachdem", "nein", "nicht", "nun", "oder", "seid", "sein", "seine", "sich", "sie", "sind", "soll", "sollen", "sollst", "sollt", "sonst", "soweit", "sowie", "und", "unser", "unsere", "unter", "vom", "von", "vor", "wann", "warum", "was", "weiter", "weitere", "wenn", "wer", "werde", "werden", "werdet", "weshalb", "wie", "wieder", "wieso", "wir", "wird", "wirst", "wo", "woher", "wohin", "zu", "zum", "zur", "über")


it_train = itoken(dados$texto, 
                  preprocessor = prep_fun, 
                  tokenizer = tok_fun, 
                  ids = dados$id, 
                  progressbar = TRUE)

vocab = create_vocabulary(it_train, stopwords = stop_words)
vectorizer = vocab_vectorizer(vocab)
dtm_train = create_dtm(it_train, vectorizer)

#col_headings <- c('id','texto','alc')
#names(dados) <- col_headings

vectorizer = vocab_vectorizer(vocab)
dtm_train = create_dtm(it_train, vectorizer)



dataM <- as.data.frame(as.matrix(dtm_train))
dataM <- c(dados, dataM)
dataM <- as.data.frame(unclass(dataM))
dataM <- subset(dataM, select = -c(texto, id) )

convert_count <- function(x) {
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c(NA, 1))
  y
}

#dataMa <- apply(dataM, 2, convert_count)
#dataMa[1:2,]
dataM[1:2,]

#export(total, "dump_enem_total.arff")

dataM[1,2:ncol(dataM)]

library(caret)
train_control <- trainControl(method="cv", number=10)
fit <- train(
  x = dataM[,2:ncol(dataM)], y = dataM$alc, method = "nb", 
  trControl = trainControl(method = "cv", number = 10)) 
fit

  #Naive Bayes
library(e1071)
model <- naiveBayes(alc ~ ., data = dataM, trControl=train_control)
#model <- naiveBayes(dataM[2:ncol(dataM)], dataM[1:1])
model

pred <- predict(model, dataM)
table(pred, data$alc)


plot(model)
dataM[1:nrow(dataM),2:ncol(dataM)]

str(nrow(dataM))
str(ncol(dataM))
predict(model, dataM[1:nrow(dataM),2:ncol(dataM)], type = "raw")


library('caret')
train_control <- trainControl(method="cv", number=10)
#create model
fit <- train(alc ~ ., data = dataM, method = "nb")



data(iris)
m <- naiveBayes(Species ~ ., data = iris)
## alternatively:
m <- naiveBayes(iris[,-5], iris[,5])
m
table(predict(m, iris), iris[,5])
