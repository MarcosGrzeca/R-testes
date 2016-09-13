#Carregar bibliotecas
library(arules)

#Carrega functions
source(file="C:\\Users\\Marcos\\Documents\\GitHub\\R-testes\\functions.R")

#Configurações
DATABASE <- "consumidor"
str(DATABASE)

clearConsole();
dadosBrutos <- query("SELECT NomeFantasia, Assunto, Problema, AvaliacaoReclamacao, Sexo FROM consumidor")
str(dadosBrutos)

#Converter caracteres para factor
dados <- as.data.frame(unclass(dadosBrutos))

#Selecionar atributos
#dados <- subset(dadosBrutos, select=c("NomeFantasia", "Assunto", "Problema", "AvaliacaoReclamacao", "Sexo"))
clearConsole();

#Execução apriori
#rules <- apriori(titanic,parameter = list(minlen=2, supp=0.05, conf=0.8), control = list(verbose=F))
#rules <- apriori(titanic,parameter = list(minlen=2, supp=0.005, conf=0.8), control = list(verbose=F))
rules <- apriori(dados, parameter = list(minlen=2, supp=0.001, conf=0.5), appearance = list(rhs=c("AvaliacaoReclamacao=Não Resolvida"), default="lhs"), control = list(verbose=F))

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