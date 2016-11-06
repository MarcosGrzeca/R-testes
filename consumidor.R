#Carregar bibliotecas
library(arules)

#verificar atributos relacionados
library(mlbench)
library(caret)



#Carrega functions
source(file="C:\\Users\\Marcos\\Documents\\GitHub\\R-testes\\functions.R")

#Configurações
DATABASE <- "enem"

clearConsole();
#SQL apriori para tipoEscola
#dadosBrutos <- query("SELECT COD_MUNICIPIO_RESIDENCIA, COD_ESCOLA, COD_MUNICIPIO_ESC, ID_LOCALIZACAO_ESC, SIT_FUNC_ESC, TP_SEXO, NACIONALIDADE, COD_MUNICIPIO_NASCIMENTO, UF_NASCIMENTO, ST_CONCLUSAO, ANO_CONCLUIU, IN_TP_ENSINO, TP_ESTADO_CIVIL, TP_COR_RACA, IN_CERTIFICADO, COD_MUNICIPIO_PROVA, ID_PROVA_CN, ID_PROVA_CH, ID_PROVA_LC, ID_PROVA_MT, TP_LINGUA, Q001, Q002, Q003, Q004, Q005, Q006, Q007, Q008, Q009, Q010, Q011, Q012, Q013, Q014, Q015, Q016, Q017, Q018, Q019, Q020, Q021, Q022, Q023, Q024, Q025, Q026, Q027, Q028, Q029, Q030, Q031, Q032, Q033, Q034, Q035, Q036, Q037, Q038, Q039, Q040, Q041, Q042, Q043, Q044, Q045, Q046, Q047, Q048, Q049, Q050, Q051, Q052, Q053, Q054, Q055, Q056, Q057, Q058, Q059, Q060, Q061, Q062, Q063, Q064, Q065, Q066, Q067, Q068, Q069, Q070, Q071, Q072, Q073, Q074, Q075, Q076, classeIBGE, media, totalCursos, faixaEtaria, necess_especiais, ate_necess_especiais, tipoEscola FROM enem WHERE tipoEscola IS NOT NULL")
dadosBrutos <- query("SELECT COD_MUNICIPIO_RESIDENCIA, COD_ESCOLA, COD_MUNICIPIO_ESC, ID_LOCALIZACAO_ESC, SIT_FUNC_ESC, TP_SEXO, NACIONALIDADE, COD_MUNICIPIO_NASCIMENTO, UF_NASCIMENTO, ST_CONCLUSAO, ANO_CONCLUIU, IN_TP_ENSINO, TP_ESTADO_CIVIL, TP_COR_RACA, IN_CERTIFICADO, COD_MUNICIPIO_PROVA, ID_PROVA_CN, ID_PROVA_CH, ID_PROVA_LC, ID_PROVA_MT, TP_LINGUA, Q001, Q002, Q003, Q004, Q005, Q006, Q007, Q008, Q009, Q010, Q011, Q012, Q013, Q014, Q015, Q016, Q017, Q018, Q019, Q020, Q021, Q022, Q023, Q024, Q025, Q026, Q027, Q028, Q029, Q030, Q031, Q032, Q033, Q034, Q035, Q036, Q037, Q038, Q039, Q040, Q041, Q042, Q043, Q044, Q045, Q046, Q047, Q048, Q049, Q050, Q051, Q052, Q053, Q054, Q055, Q056, Q057, Q058, Q059, Q060, Q061, Q062, Q063, Q064, Q065, Q066, Q067, Q068, Q069, Q070, Q071, Q072, Q073, Q074, Q075, Q076, classeIBGE, media, totalCursos, faixaEtaria, necess_especiais, ate_necess_especiais, tipoEscola FROM enem")

str(dadosBrutos)

dadosBrutos$ANO_CONCLUIU[dadosBrutos$ANO_CONCLUIU == ""] <- "2014"
dadosBrutos$IN_CERTIFICADO[dadosBrutos$IN_CERTIFICADO == ""] <- NA
dadosBrutos$COD_ESCOLA[dadosBrutos$COD_ESCOLA == ""] <- NA
dadosBrutos$COD_MUNICIPIO_ESC[dadosBrutos$COD_MUNICIPIO_ESC == ""] <- NA
dadosBrutos$ID_LOCALIZACAO_ESC[dadosBrutos$ID_LOCALIZACAO_ESC == ""] <- NA
dadosBrutos$SIT_FUNC_ESC[dadosBrutos$SIT_FUNC_ESC == ""] <- NA
dadosBrutos$Q025[dadosBrutos$Q025 == ""] <- NA
dadosBrutos$Q031[dadosBrutos$Q031 == ""] <- NA
dadosBrutos$Q032[dadosBrutos$Q032 == ""] <- NA
dadosBrutos$Q034[dadosBrutos$Q034 == ""] <- NA
dadosBrutos$Q035[dadosBrutos$Q035 == ""] <- NA
dadosBrutos$Q041[dadosBrutos$Q041 == ""] <- NA
dadosBrutos$Q042[dadosBrutos$Q042 == ""] <- NA
dadosBrutos$Q043[dadosBrutos$Q043 == ""] <- NA
dadosBrutos$Q044[dadosBrutos$Q044 == ""] <- NA
dadosBrutos$Q045[dadosBrutos$Q045 == ""] <- NA
dadosBrutos$Q046[dadosBrutos$Q046 == ""] <- NA
dadosBrutos$Q047[dadosBrutos$Q047 == ""] <- NA
dadosBrutos$Q048[dadosBrutos$Q048 == ""] <- NA
dadosBrutos$Q049[dadosBrutos$Q049 == ""] <- NA
dadosBrutos$Q050[dadosBrutos$Q050 == ""] <- NA
dadosBrutos$Q051[dadosBrutos$Q051 == ""] <- NA
dadosBrutos$Q052[dadosBrutos$Q052 == ""] <- NA
dadosBrutos$Q053[dadosBrutos$Q053 == ""] <- NA
dadosBrutos$Q054[dadosBrutos$Q054 == ""] <- NA
dadosBrutos$Q055[dadosBrutos$Q055 == ""] <- NA
dadosBrutos$Q056[dadosBrutos$Q056 == ""] <- NA
dadosBrutos$Q057[dadosBrutos$Q057 == ""] <- NA
dadosBrutos$Q058[dadosBrutos$Q058 == ""] <- NA
dadosBrutos$Q059[dadosBrutos$Q059 == ""] <- NA
dadosBrutos$Q060[dadosBrutos$Q060 == ""] <- NA
dadosBrutos$Q061[dadosBrutos$Q061 == ""] <- NA
dadosBrutos$Q062[dadosBrutos$Q062 == ""] <- NA
dadosBrutos$Q063[dadosBrutos$Q063 == ""] <- NA
dadosBrutos$Q064[dadosBrutos$Q064 == ""] <- NA
dadosBrutos$Q065[dadosBrutos$Q065 == ""] <- NA
dadosBrutos$Q066[dadosBrutos$Q066 == ""] <- NA
dadosBrutos$Q067[dadosBrutos$Q067 == ""] <- NA
dadosBrutos$Q068[dadosBrutos$Q068 == ""] <- NA
dadosBrutos$Q069[dadosBrutos$Q069 == ""] <- NA
dadosBrutos$Q070[dadosBrutos$Q070 == ""] <- NA
dadosBrutos$Q071[dadosBrutos$Q071 == ""] <- NA
dadosBrutos$Q072[dadosBrutos$Q072 == ""] <- NA
dadosBrutos$Q073[dadosBrutos$Q073 == ""] <- NA
dadosBrutos$Q074[dadosBrutos$Q074 == ""] <- NA
dadosBrutos$Q075[dadosBrutos$Q075 == ""] <- NA
dadosBrutos$Q076[dadosBrutos$Q076 == ""] <- NA
dadosBrutos$Q076[dadosBrutos$Q076 == "\r"] <- NA

dados <- as.data.frame(unclass(dadosBrutos))
str(dados)

dados$Q004 <- discretize(dados$Q004, method = "interval", 20)
dados$Q040 <- discretize(dados$Q040, method = "interval", 13)
dados$TP_LINGUA <- discretize(dados$TP_LINGUA, method = "interval", 2)
dados$ID_PROVA_CN <- discretize(dados$ID_PROVA_CN, method = "interval", 5)
dados$ID_PROVA_CH <- discretize(dados$ID_PROVA_CH, method = "interval", 5)
dados$ID_PROVA_LC <- discretize(dados$ID_PROVA_LC, method = "interval", 5)
dados$ID_PROVA_MT <- discretize(dados$ID_PROVA_MT, method = "interval", 5)
dados$NACIONALIDADE <- discretize(dados$NACIONALIDADE, method = "interval", 4)
dados$COD_MUNICIPIO_NASCIMENTO <- discretize(dados$COD_MUNICIPIO_NASCIMENTO, method = "interval", 1879)
dados$COD_MUNICIPIO_PROVA <- discretize(dados$COD_MUNICIPIO_PROVA, method = "interval", 239)
dados$COD_MUNICIPIO_RESIDENCIA <- discretize(dados$COD_MUNICIPIO_RESIDENCIA, method = "interval", 185)
dados$TP_COR_RACA <- discretize(dados$TP_COR_RACA, method = "interval", 6)
dados$TP_ESTADO_CIVIL <- discretize(dados$TP_ESTADO_CIVIL, method = "interval", 4)
dados$ST_CONCLUSAO <- discretize(dados$ST_CONCLUSAO, method = "interval", 4)
dados$necess_especiais <- discretize(dados$necess_especiais, method = "interval", 2)
dados$ate_necess_especiais <- discretize(dados$ate_necess_especiais, method = "interval", 2)

str(dados)


clearConsole();

#Execução apriori
#rules <- apriori(dados,parameter = list(minlen=2, supp=0.04, conf=0.7), control = list(verbose=F))
#rules <- apriori(titanic,parameter = list(minlen=2, supp=0.005, conf=0.8), control = list(verbose=F))
#rules <- apriori(dados, parameter = list(minlen=2, supp=0.001, conf=0.5), appearance = list(rhs=c("AvaliacaoReclamacao=Não Resolvida"), default="lhs"), control = list(verbose=F))
#rules <- apriori(dados, parameter = list(minlen=2, supp=0.001, conf=0.5), control = list(verbose=F))
#rules <- apriori(dados, parameter = list(minlen=2, supp=0.001, conf=0.5), appearance = list(rhs=c("AvaliacaoReclamacao="), default="lhs"), control = list(verbose=F))

#dadosteste2 = vector()
#dadosteste2$Q036 = dados$Q036;
#dadosteste2$tipoEscola = dados$tipoEscola;
#str(dadosteste2)
#dadosteste2 <- as.data.frame(unclass(dadosteste2))

clearConsole();
#rules <- apriori(dados, parameter = list(minlen=2, supp=0.8, conf=0.7), appearance = list(rhs=c("TP_ESCOLA=1", "TP_ESCOLA=2"), default="lhs"), control = list(verbose=F))

#rules <- apriori(dados, parameter = list(minlen=2, supp=0.1, conf=0.5, maxtime =80), appearance = list(rhs=c("tipoEscola=PR"), default="lhs"), control = list(verbose=F))

#Apriori todas medias
#rules <- apriori(dados, parameter = list(minlen=2, supp=0.35, conf=0.5, maxtime =100), appearance = list(rhs=c("media=A", "media=B", "media=C"), default="lhs"), control = list(verbose=F))

rules <- apriori(dados, parameter = list(minlen=2, supp=0.05, conf=0.5, maxtime =120), appearance = list(rhs=c("media=A"), default="lhs"), control = list(verbose=F))

#Reordenar regras
rules.sorted <- sort(rules, by="lift")
inspect(rules)

# find redundant rules
subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)
# remove redundant rules
rules.pruned <- rules.sorted[!redundant]
clearConsole();

print("Resultado Final")
initFileLog("result.txt")
inspect(rules.pruned)
finishFileLog("result.txt")

x <- boxplot(dadosBrutos$mediaNumerica, data=dadosBrutos, main="BoxPlot Média")
str(x)

#Verificar correlacionados
correlationMatrix <- cor(dadosBrutos[,1:171])
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# print indexes of highly correlated attributes
str(highlyCorrelated)

str(dados)

summary(dados)


min(dadosBrutos$mediaNumerica)
max(dadosBrutos$mediaNumerica)
mean(dadosBrutos$mediaNumerica)

media <- sd(dadosBrutos$mediaNumerica)
str(media)