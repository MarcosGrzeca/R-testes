#Carregar bibliotecas
library(arules)
library(RMySQL)


#Functions
clearConsole <- function(){
  cat("\014")
}

initFileLog <- function(nome){
  zz <- file(nome, open = "wt")
  sink(zz)
  sink(zz, type = "message")
}

finishFileLog <- function(nome){
  sink(type = "message")
  sink()
  file.show(nome)
}

query <- function(sql) {
  mydb = dbConnect(MySQL(), user='root', password='', dbname='consumidor', host='localhost')
  rs = dbSendQuery(mydb, sql);
  dataBD <- fetch(rs, n=-1)
  #dataBD <- fetch(rs, getNumRows(mydb, "WAVELENGTH"))
  huh <- dbHasCompleted(rs)
  dbClearResult(rs)
  dbDisconnect(mydb)
  return (dataBD)
}

clearConsole();
date <- query("SELECT * FROM consumidor")
str(date);


#Fim Functions
clearConsole();

#Selecionar atributos
dados <- subset(dadosBrutos, select=c("NomeFantasia", "Assunto", "Problema", "AvaliacaoReclamacao", "Sexo"))
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