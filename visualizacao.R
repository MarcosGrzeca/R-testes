library(arulesViz)

criarVisualizacos <- function() { 
  plot(rules, method = "grouped", measure = "lift", shading = "lift")
  
  
  plot(rules, method = "graph", control = list(verbose = TRUE)) 
  
  plot(rules, method = "grouped", control = list(verbose = TRUE), interactive = TRUE) 
  
  plot(rules, method = "paracoord", control = list(reorder= TRUE))
  #plot(x, method = NULL, measure = "support", shading = "lift", interactive = FALSE, data = NULL, control = NULL, ...)
  
  
  
  plot(rules, method = "graph", interactive = TRUE)
  plot(rules, method = "paracoord")
  plot(rules)
  plot(rules, method = "doubledecker")
  
  #visualizacao
  supp <- 0.04
  epsilon <- 0.1
  c <- 0.1
  n <- -2 * log(c)/ (supp * epsilon^2)
  rulesSample <- sample(rules, n, replace = TRUE)
  itemFrequencyPlot(rulesSample, population = dados, support = supp, lift = TRUE, cex.names = 0.9)
  
  #BoxPlot
  x <- boxplot(dadosBrutos$mediaNumerica, data=dadosBrutos, main="BoxPlot Média")
  summary(dados)
  
  min(dadosBrutos$mediaNumerica)
  max(dadosBrutos$mediaNumerica)
  mean(dadosBrutos$mediaNumerica)
  
  media <- sd(dadosBrutos$mediaNumerica)
  str(media)
}