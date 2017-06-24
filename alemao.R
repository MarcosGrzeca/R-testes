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
dados <- query("SELECT id, texto, alc, repetitions, longpauses FROM conversa LIMIT 3000")
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

vectorizer = vocab_vectorizer(vocab)
dtm_train = create_dtm(it_train, vectorizer)

dataM <- as.data.frame(as.matrix(dtm_train))
dataM <- c(dados, dataM)
dataM <- as.data.frame(unclass(dataM))
dataM <- subset(dataM, select = -c(texto, id) )

library(caret)
fit <- train(
  x = dataM[,2:ncol(dataM)], y = dataM$alc, method = "nb", 
  trControl = trainControl(method = "cv", number = 10)) 
fit

#mod_fit <- train(Class ~ Age + ForeignWorker + Property.RealEstate + Housing.Own +  CreditHistory.Critical,  data=training, method="glm", family="binomial")

bh_pred <- predict(fit, dataM)
bh_pred

a <- table(bh_pred, dataM$alc)

uarA <- a[1,1] / (a[1,1] + a[1,2])
uarNA <- a[2,2] / (a[2,2] + a[2,1])
if (uarNA == "NaN"){
  uarNA = 0
}
uar = (uarA + uarNA) / 2
uar

#TP FP
#FN TN