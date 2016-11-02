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
#dadosBrutos <- query("SELECT MesAbertura, SegmentoMercado, NomeFantasia, AvaliacaoReclamacao FROM consumidor")

dadosBrutos <- query("SELECT mediaNumerica FROM enem WHERE  UF_RESIDENCIA = 'PE' AND IN_PRESENCA_CN = 1 AND IN_PRESENCA_CH = 1 AND IN_PRESENCA_LC = 1 AND IN_PRESENCA_MT = 1 AND IN_STATUS_REDACAO <> 6 ")
dadosBrutos$mediaBoxPlot <- as.data.frame(unclass(dadosBrutos$mediaBoxPlot))
dadosBrutos$classeIBGE <- as.data.frame(unclass(dadosBrutos$classeIBGE))
str(dadosBrutos)

summary(dadosBrutos)
str(dadosBrutos$teste2)

#testes
dadosBrutos$teste <- 0;
dadosBrutos$teste2 <- c("small", "large");
dadosBrutos$teste[dadosBrutos$Q001 == "B"] <- "1"

#cbind unifica conjuntos



min(dadosBrutos$mediaNumerica)
max(dadosBrutos$mediaNumerica)
mean(dadosBrutos$mediaNumerica)

media <- sd(dadosBrutos$mediaNumerica)
str(media)


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


#Discretizar mês abertura
#dadosBrutos$MesAbertura <- discretize(dadosBrutos$MesAbertura, method = "frequency", 12)
#dadosBrutos$MesAbertura <- discretize(dadosBrutos$MesAbertura, categories = 12)


#Converter caracteres para factor
dados <- as.data.frame(unclass(dadosBrutos))
str(dados)

#Selecionar atributos
#dados <- subset(dadosBrutos, select=c("NomeFantasia", "Assunto", "Problema", "AvaliacaoReclamacao", "Sexo"))
clearConsole();

#Execução apriori
#rules <- apriori(dados,parameter = list(minlen=2, supp=0.04, conf=0.7), control = list(verbose=F))
#rules <- apriori(titanic,parameter = list(minlen=2, supp=0.005, conf=0.8), control = list(verbose=F))
#rules <- apriori(dados, parameter = list(minlen=2, supp=0.001, conf=0.5), appearance = list(rhs=c("AvaliacaoReclamacao=Não Resolvida"), default="lhs"), control = list(verbose=F))
#rules <- apriori(dados, parameter = list(minlen=2, supp=0.001, conf=0.5), control = list(verbose=F))
#rules <- apriori(dados, parameter = list(minlen=2, supp=0.001, conf=0.5), appearance = list(rhs=c("AvaliacaoReclamacao="), default="lhs"), control = list(verbose=F))

rules <- apriori(dados, parameter = list(minlen=2, supp=0.04, conf=0.5), appearance = list(rhs=c("Area=Telecomunicações"), default="lhs"), control = list(verbose=F))

#Reordenar regras
rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)

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