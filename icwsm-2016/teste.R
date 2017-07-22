#Carregar bibliotecas
options(max.print = 99999999)

#Carrega functions
library(tools)
source(file_path_as_absolute("functions.R"))

#Configuracoes
DATABASE <- "icwsm-2016"
clearConsole();
dadosQ1 <- query("SELECT id, q1 as resposta, textParser, hashtags FROM tweets WHERE situacao = 'S'")
#dadosQ2 <- query("SELECT id, q2 as resposta, textParser, hashtags, emoticonPos, emoticonNeg FROM tweets WHERE situacao = 'S' AND q1 = '1' AND q2 IS NOT NULL")
#dadosQ3 <- query("SELECT id, q3 as resposta, textParser, hashtags, emoticonPos, emoticonNeg FROM tweets WHERE situacao = 'S' AND q2 = '1' AND q3 IS NOT NULL")

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
                  #tokenizer = stem_tokenizer1,
                  tokenizer = tok_fun,
                  ids = dados$id, 
                  progressbar = TRUE)
stop_words = tm::stopwords("en")

#vocab = create_vocabulary(it_train, ngram = c(1L, 1L), stopwords = stop_words)
vocab = create_vocabulary(it_train, stopwords = stop_words)

vocab
vectorizer = vocab_vectorizer(vocab)

#pruned_vocab = prune_vocabulary(vocab, 
#                                term_count_min = 25#, 
#                                #doc_proportion_max = 0.99,
#                                #doc_proportion_min = 0.0001
#                                )

#print("Resultado Final")
#initFileLog("mais_teste.txt")
#view(pruned_vocab)
#finishFileLog("mais_teste.txt")

#inspect(pruned_vocab)
#dump(as.data.frame(pruned_vocab), "testes/mais.csv")

#vectorizer = vocab_vectorizer(pruned_vocab)
dtm_train_texto = create_dtm(it_train, vectorizer)

it_train = itoken(dados$hashtags, 
                  preprocessor = prep_fun, 
                  tokenizer = tok_fun, 
                  #tokenizer = stem_tokenizer1, 
                  ids = dados$id, 
                  progressbar = TRUE)

vocabHashTags = create_vocabulary(it_train)

#pruned_vocab_hash = prune_vocabulary(vocabHashTags, 
#                                term_count_min = 3, 
#                                doc_proportion_max = 0.99,
#                                doc_proportion_min = 0.0001)
#pruned_vocab_hash
vectorizerHashTags = vocab_vectorizer(vocabHashTags)
dtm_train_hash_tags = create_dtm(it_train, vectorizerHashTags)

dataTexto <- as.matrix(dtm_train_texto)
dataFrameTexto <- as.data.frame(as.matrix(dtm_train_texto))
dataFrameHash <- as.data.frame(as.matrix(dtm_train_hash_tags))
clearConsole()

if (!require("pacman")) install.packages("pacman")
pacman::p_load_current_gh("trinker/lexicon", "trinker/sentimentr")

if (!require("pacman")) install.packages("pacman")
pacman::p_load(sentimentr)

sentiments <- sentiment_by(dados$textParser)
dados$emotiom <- 0
dados$emotiom[sentiments$ave_sentiment < -0.5] <- -2
dados$emotiom[sentiments$ave_sentiment < 0] <- -1
dados$emotiom[sentiments$ave_sentiment > 0] <- 1
dados$emotiom[sentiments$ave_sentiment > 0.5] <- 2

sentiments <- sentiment_by(dados$hashtags)
dados$hashEmo <- 0
dados$hashEmo[sentiments$ave_sentiment < -0.5] <- -2
dados$hashEmo[sentiments$ave_sentiment < 0] <- -1
dados$hashEmo[sentiments$ave_sentiment > 0] <- 1
dados$hashEmo[sentiments$ave_sentiment > 0.5] <- 2

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
maFinal <- subset(maFinal, select = -c(textParser, id, hashtags))

save(maFinal, file="denovo_99_completo.Rda")
#dump(maFinal, "testes/maFinal_no.csv")
#maFinal = read.csv("testes/tweet_data_my.csv", header = TRUE)
#maFinal = read.csv("testes/maFinal_no.csv", header = TRUE)


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

#C     Accuracy   Kappa
#0.25  0.5603493  0    
#0.50  0.5603493  0    
#1.00  0.5603493  0  

#C     RMSE       Rsquared   
#0.25  0.6012984  0.001059074
#0.50  0.6011936  0.001059074
#1.00  0.6011833  0.001676222

#C     RMSE       Rsquared   
#0.25  0.6011149  0.001161257
#0.50  0.6009995  0.001161257
#1.00  0.6009580  0.001144398

#install.packages("tm")
#install.packages("Rstem")
#install.packages("sentimentr")
#Recall
#https://www.r-bloggers.com/sentiment-analysis-with-machine-learning-in-r/


if (!require("pacman")) install.packages("pacman")
pacman::p_load_current_gh("trinker/lexicon", "trinker/sentimentr")

if (!require("pacman")) install.packages("pacman")
pacman::p_load(sentimentr)

sentiment(maFinal$pg)
a <- sentiment_by(dadosQ1$textParser)
a
a$ave_sentiment[1:10]

summary(a$ave_sentiment)

#dadosQ1
#(out <- with(presidential_debates_2012, sentiment_by(dialogue, list(person, time))))
#(out <- with(dadosQ1, sentiment_by(dadosQ1$textParser, list(id))))
#plot(out)


if (!require("rJava")) {
  library(rJava)
}
if (!require("NLP")) {
  install.packages(c("NLP", "openNLP", "RWeka", "qdap"))
}

install.packages("openNLPmodels.en",
                 repos = "http://datacube.wu.ac.at/",
                 type = "source")

library(NLP)
library(openNLP)
library(RWeka)

library(NLP)
library(openNLP)
library(magrittr)

bora <- as.String(maFinal$textParser[1:50])

word_ann <- Maxent_Word_Token_Annotator()
sent_ann <- Maxent_Sent_Token_Annotator()

bio_annotations <- annotate(bora, list(sent_ann, word_ann))
bio_annotations
class(bio_annotations)
head(bio_annotations)

bio_doc <- AnnotatedPlainTextDocument(bora, bio_annotations)
bio_doc
sents(bio_doc) %>% head(2)

person_ann <- Maxent_Entity_Annotator(kind = "person")
location_ann <- Maxent_Entity_Annotator(kind = "location")
organization_ann <- Maxent_Entity_Annotator(kind = "organization")

pipeline <- list(sent_ann,
                 word_ann,
                 person_ann,
                 location_ann,
                 organization_ann)
bio_annotations <- annotate(bora, pipeline)
bio_doc <- AnnotatedPlainTextDocument(bora, bio_annotations)

# Extract entities from an AnnotatedPlainTextDocument
entities <- function(doc, kind) {
  s <- doc$content
  a <- annotations(doc)[[1]]
  if(hasArg(kind)) {
    k <- sapply(a$features, `[[`, "kind")
    s[a[k == kind]]
  } else {
    s[a[a$type == "entity"]]
  }
}

entities(bio_doc, kind = "person")
entities(bio_doc, kind = "location")
entities(bio_doc, kind = "organization")


#com analise de sentimentos das palabras e hashtags
#C     Accuracy   Kappa
#0.25  0.5603493  0
#0.50  0.5603493  0
#1.00  0.5603493  0


library(NLP)
library(openNLP)
library(magrittr)

filenames <- Sys.glob("files/*.txt")
filenames

texts <- filenames %>%
  lapply(readLines) %>%
  lapply(paste0, collapse = " ") %>%
  lapply(as.String)

names(texts) <- basename(filenames)
str(texts, max.level = 1)

annotate_entities <- function(doc, annotation_pipeline) {
  annotations <- annotate(doc, annotation_pipeline)
  AnnotatedPlainTextDocument(doc, annotations)
}

itinerants_pipeline <- list(
  Maxent_Sent_Token_Annotator(),
  Maxent_Word_Token_Annotator(),
  Maxent_Entity_Annotator(kind = "person"),
  Maxent_Entity_Annotator(kind = "location")
)

texts_annotated <- as.String(texts) %>%
  lapply(annotate_entities, itinerants_pipeline)

entities <- function(doc, kind) {
  s <- doc$content
  a <- annotations(doc)[[1]]
  if(hasArg(kind)) {
    k <- sapply(a$features, `[[`, "kind")
    s[a[k == kind]]
  } else {
    s[a[a$type == "entity"]]
  }
}

places <- texts_annotated %>%
  lapply(entities, kind = "location")

people <- texts_annotated %>%
  lapply(entities, kind = "person")

places %>%
  sapply(length)


places %>%
  lapply(unique) %>%
  sapply(length)


people %>%
  sapply(length)

if (!require("ggmap")) {
  install.packages("ggmap")
}
library(ggmap)

places[["cartwright-peter.txt"]]
people[["cartwright-peter.txt"]]
people

all_places <- union(places[["pratt-parley.txt"]], places[["cartwright-peter.txt"]]) %>% union(places[["lee-jarena.txt"]])
all_places

