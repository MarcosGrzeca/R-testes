library(tools)
source(file_path_as_absolute("functions.R"))

load("alemao_base_completa.Rda")
     
print("NAIVE BAYES")
source(file_path_as_absolute("classificadores.R"))
classificar(dadosFinal)
