library(arulesViz)
library(plotly)


regrasGrafico <- regrasA[1:40]
regrasGrafico <- regrasC[1:57]
inspect(regrasGrafico)


teste <- regrasGrafico

plot(regrasGrafico[1:4], method="graph")

plot(regrasGrafico, method = "grouped", measure = "lift", shading = "support", control = list(k = 10))

plot(regrasGrafico[1,], method = "doubledecker", data = regrasGrafico)


plot(regrasGrafico, method = "grouped", measure = "lift", shading = "support", control = list(k = 20, aggr.fun = "max"))

plot(regrasGrafico, method = "grouped", measure = "order", shading = "lift")

inspectDT(regrasGrafico);


plotly_arules(regrasGrafico, method = "scatterplot", measure = c("support", "confidence"), shading = "lift", max = 1000)
plotly_arules(regrasGrafico, measure = c("support", "confidence"), shading = "order")



plot(regrasC[1:57], method = "grouped", measure = "support", shading = "lift")

plot(regrasGrafico, method = "grouped", measure = "lift", shading = "support")

plot(regrasGrafico)

  
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