#Carregar bibliotecas
options(max.print = 99999999)

#https://cran.r-project.org/web/packages/text2vec/vignettes/text-vectorization.html
#Pr?-processar texto https://rpubs.com/MajstorMaestro/256588

#Carrega functions
DIRETORIO = "C:\\Users\\Marcos\\Documents\\GitHub\\R-testes\\"
source(file=paste(DIRETORIO,"functions.R", sep = ""))

#Configura??es
DATABASE <- "alemao"
clearConsole();
dados <- query("SELECT id, texto, alc FROM conversa")
dados$alc[dados$alc == "cna"] <- "na"
dados$alc <- as.factor(dados$alc)
str(dados)

clearConsole()

library(text2vec)
library(data.table)

setDT(dados)
setkey(dados, id)
set.seed(2016L)

prep_fun = tolower
tok_fun = word_tokenizer

stop_words = c("aber","als","am","an","auch","auf","aus","bei","bin","bis","bist","da","dadurch","daher","darum","das","da?","dass","dein","deine","dem","den","der","des","dessen","deshalb","die","dies","dieser","dieses","doch","dort","du","durch","ein","eine","einem","einen","einer","eines","er","es","euer","eure","f?r","hatte","hatten","hattest","hattet","hier","hinter","ich","ihr","ihre","im","in","ist","ja","jede","jedem","jeden","jeder","jedes","jener","jenes","jetzt","kann","kannst","k?nnen","k?nnt","machen","mein","meine","mit","mu?","mu?t","musst","m?ssen","m??t","nach","nachdem","nein","nicht","nun","oder","seid","sein","seine","sich","sie","sind","soll","sollen","sollst","sollt","sonst","soweit","sowie","und","unser","unsere","unter","vom","von","vor","wann","warum","was","weiter","weitere","wenn","wer","werde","werden","werdet","weshalb","wie","wieder","wieso","wir","wird","wirst","wo","woher","wohin","zu","zum","zur","?ber")

it_train = itoken(dados$texto, 
                  preprocessor = prep_fun, 
                  tokenizer = tok_fun, 
                  ids = dados$id, 
                  progressbar = TRUE)

vocab = create_vocabulary(it_train, stopwords = stop_words)
#vocab = create_vocabulary(it_train)
vectorizer = vocab_vectorizer(vocab)
dtm_train = create_dtm(it_train, vectorizer)

col_headings <- c('id','texto','alc')
names(dados) <- col_headings

vectorizer = vocab_vectorizer(vocab)
dtm_train = create_dtm(it_train, vectorizer)
#identical(rownames(dtm_train), dados$id)

data <- as.data.frame(as.matrix(dtm_train))
total <- c(dados, data)
total <- as.data.frame(total) 
total <- subset(total, select = -c(texto, id) )
#export(total2, "alemao_bag.csv")

total <- as.data.frame(unclass(total))

export(total, "dump_enem_total.arff")

library('caret')

library(e1071)
model <- naiveBayes(alc ~ ., data = total)
model

train_control <- trainControl(method="cv", number=1)
#create model
fit <- train(alc ~ ., data = total, method = "nb", trControl=train_control)
fit


# load the library
library(caret)
# load the iris dataset
data(iris)
# define training control
train_control <- trainControl(method="cv", number=10)
# fix the parameters of the algorithm
grid <- expand.grid(.fL=c(0), .usekernel=c(FALSE))
# train the model
model <- train(Species~., data=iris, trControl=train_control, method="nb")
# summarize results
print(model)
             
library(glmnet)
NFOLDS = 5
t1 = Sys.time()
glmnet_classifier = cv.glmnet(x = dtm_train, y = train[['alc']], 
                              family = 'binomial', 
                              # L1 penalty
                              alpha = 1,
                              # interested in the area under ROC curve
                              type.measure = "auc",
                              # 5-fold cross-validation
                              nfolds = NFOLDS,
                              # high value is less accurate, but has faster training
                              thresh = 1e-3,
                              # again lower number of iterations for faster training
                              maxit = 1e3)
print(difftime(Sys.time(), t1, units = 'sec'))

plot(glmnet_classifier)

print(paste("max AUC =", round(max(glmnet_classifier$cvm), 4)))

# Note that most text2vec functions are pipe friendly!
it_test = test$texto %>% 
  prep_fun %>% 
  tok_fun %>% 
  itoken(ids = test$id, 
         # turn off progressbar because it won't look nice in rmd
         progressbar = FALSE)

dtm_test = create_dtm(it_test, vectorizer)

preds = predict(glmnet_classifier, dtm_test, type = 'response')[,1]
glmnet:::auc(test$alc, preds)

