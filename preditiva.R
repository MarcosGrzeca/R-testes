#NaiveBayes
library(e1071)

analisePreditiva <- function() {
  #Naive Bayes
  modelo <- naiveBayes(dados[1:21], dados[,22])
  dados$mediaPrevista <- predict(modelo, dados, type="class")
  
  clearConsole();
  matriz <- matrix(data=0,nrow=3,ncol=3)
  for(i in 1:nrow(dados)) {
    row <- dados[i,]
    matriz[row$media, row$mediaPrevista] <- matriz[row$media, row$mediaPrevista] + 1
  }
  matriz
}