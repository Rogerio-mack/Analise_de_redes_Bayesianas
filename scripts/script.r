

### 2) Rede bayesiana: conceitos básicos

#Rede bayesiana: instalação
if(!require("bnlearn"))
	install.packages('bnlearn',dependencies=T)

if(!require("Rgraphviz")){
	if (!require("BiocManager", quietly = TRUE))
	    install.packages("BiocManager")

	BiocManager::install("Rgraphviz")
}


#Rede bayesiana: carregar pacotes
library(bnlearn)
library(Rgraphviz)
set.seed(1243661)
data(asia)

#Rede bayesiana: dados de exemplo
summary(asia)


#Rede bayesiana: construção
variaveis = c("D","T","L","B","A","S","X","E")
e = empty.graph(variaveis)

e = set.arc(e, "A","T")
e = set.arc(e, "T","E")
e = set.arc(e, "E","X")
e = set.arc(e, "S","L")
e = set.arc(e, "S","B")
e = set.arc(e, "L","E")
e = set.arc(e, "E","D")
e = set.arc(e, "B","D")

graphviz.plot(e, layout = "dot")

e$arcs

#Rede bayesiana: controlar ciclos
acyclic(e)

#Rede bayesiana: nós
root.nodes(e)
leaf.nodes(e)

children(e,"A")
parents(e,"E")

#Rede bayesiana: estruturas “V”
vstructs(e)

#Rede bayesiana: d-separação (exemplos)
dsep(e, "S", "A") 
dsep(e, "S", "E")
dsep(e, "S", "E", "L")
dsep(e, "B", "L", "S")
 

#Rede bayesiana: cobertura de Markov (exemplos)
mb(e, node = 'L')
mb(e, node = 'E')



### 3) Estimação de parâmetros e Inferência

#Estimação de parâmetros
fit = bn.fit(e, asia, method = "mle")
fit
graphviz.chart(fit, type = "barprob")

#Ajuste / desempenho do modelo: BIC
score(e, asia, type = "bic")

#Ajuste / desempenho do modelo: Erro de classificação
cvA = bn.cv(asia, bn = e, k = 10, fit = "mle", loss="pred", loss.args = list(target = "X"))
loss(cvA)

#Inferência aproximada: Logic sampling, exemplo (etapa 1)
rFit = rbn(fit,100000)
head(rFit,5)

#Inferência aproximada: Logic sampling, exemplo (etapa 2)
table(rFit[,c("L","S")])

#Inferência aproximada: Logic sampling, exemplo
cpquery(fit, event = (L == "yes"), evidence = (S == "yes"), n=10000000)

#Inferência aproximada: mais exemplos
cpquery(fit, event = (L == "yes"), evidence = (S == "yes") | (S == "no"), n=10000000)
cpquery(fit, event = (X == "yes"), evidence = ((T == "yes") & (S == "yes")), n=10000000)
cpquery(fit, event = (X == "yes"), evidence = ((T == "no") & (S == "yes")), n=10000000)

#Inferência aproximada: mais exemplos (2)
sim_sim = cpquery(fit, event = (L == "yes"), evidence = (S == "yes"), n=10000000) 
sim_nao = cpquery(fit, event = (L == "yes"), evidence = (S == "no"), n=10000000) 
nao_nao = cpquery(fit, event = (L == "no"), evidence = (S == "no"), n=10000000) 
nao_sim = cpquery(fit, event = (L == "no"), evidence = (S == "yes"), n=10000000) 

(sim_sim / nao_sim) / (sim_nao / nao_nao)


fitGLM = glm(L ~ S, data=asia,family=binomial())
summary(fitGLM)

exp(coef(fitGLM))[2]




### 4) Construção de redes Bayesianas orientadas por dados

#Algoritmos baseados em restrições: Exemplo
dagT = pc.stable(asia) 
graphviz.plot(dagT, layout = "dot")

dagT = set.arc(dagT, "S", "L")
dagT = set.arc(dagT, "S", "B")
dagT = set.arc(dagT, "B", "D")

fit = bn.fit(dagT, asia, method = "mle")
graphviz.chart(fit, type = "barprob")


#Algoritmos baseados em pontuação: Exemplo
dagHC = hc(asia, score = "bde") 
graphviz.plot(dagHC, layout = "dot")

fit = bn.fit(dagHC, asia, method = "mle")
graphviz.chart(fit, type = "barprob")


dagTABU = tabu(asia, score = "bde") 
graphviz.plot(dagTABU, layout = "dot")

fit = bn.fit(dagTABU, asia, method = "mle")
graphviz.chart(fit, type = "barprob")


#Outro exemplo: dados de trombose coronária
data(coronary)
set.seed(124321)
dfDados = as.data.frame(coronary)
colnames(dfDados) = c("Smoking","M_Work","P_Work","Pressure","Proteins","Family")
dfM = as.data.frame(model.matrix(~Smoking+Pressure+Family+Proteins,data=dfDados))

dfM[,"Y"] = as.integer(6*dfM$Smokingyes + 6*dfM$`Pressure>140` + 10*dfM$Familypos + 1*dfM$`Proteins>3` + rnorm(nrow(dfM), mean = 5, sd = 25) > 12)

dfDados[,"Y"] = relevel(as.factor(dfM$Y),ref="0")
summary(dfDados)


dagCru = tabu(dfDados, score = "bde")
plot(dagCru)



#Outro exemplo: fazem sentido as arestas?
dfL = tiers2blacklist(list("Family",
		c("M_Work","P_Work"),
		"Smoking",
		c("Proteins","Pressure"),
		"Y"))
dfL
dagComNiveis = tabu(dfDados, score = "bde", blacklist = dfL)
plot(dagComNiveis)

