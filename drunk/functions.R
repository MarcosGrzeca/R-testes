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
  dbDataType(RMySQL::MySQL(), "a")
  mydb = dbConnect(MySQL(), user='root', password='', dbname=DATABASE, host='localhost')
  rs = dbSendQuery(mydb, sql);
  dataBD <- fetch(rs, n=-1)
  #dataBD <- fetch(rs, getNumRows(mydb, "WAVELENGTH"))
  huh <- dbHasCompleted(rs)
  dbClearResult(rs)
  dbDisconnect(mydb)
  return (dataBD)
}


#Exportar para ARFF
library("rio")

dump <- function(dadosExport, arquivo) {
  #Exportar para CSV
  write.table(dadosExport, file = arquivo, append = FALSE, quote = TRUE, sep = ",", eol = "\n", na = "?", dec = ".", row.names = FALSE, col.names = TRUE, qmethod = c("escape", "double"), fileEncoding = "")
  #export(dados, "dump_enem_total.arff")
}