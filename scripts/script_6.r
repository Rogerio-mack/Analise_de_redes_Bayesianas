library(bnlearn)
library(Rgraphviz)
set.seed(124321)

### 6) Dados de custo dos cuidados de saúde

#Dados de custo dos cuidados de saúde: carregar dados
hcDados = read.table("healthcare.txt", header = TRUE)

hcDados[,"D"] = relevel(cut(hcDados$D, breaks=quantile(hcDados$D,probs=c(0,.5,1)),labels=c("baixo","alto"), include.lowest=TRUE),ref="baixo")
hcDados[,"I"] = relevel(cut(hcDados$I, breaks=quantile(hcDados$I,probs=c(0,.5,1)),labels=c("baixo","alto"), include.lowest=TRUE),ref="baixo")
hcDados[,"O"] = relevel(cut(hcDados$O, breaks=quantile(hcDados$O,probs=c(0,.5,1)),labels=c("baixo","alto"), include.lowest=TRUE),ref="baixo")
hcDados[,"T"] = relevel(cut(hcDados$T, breaks=quantile(hcDados$T,probs=c(0,.5,1)),labels=c("baixo","alto"), include.lowest=TRUE),ref="baixo")
hcDados[,"A"] = relevel(as.factor(hcDados$A),ref="adult")
hcDados[,"C"] = relevel(as.factor(hcDados$C),ref="none")
hcDados[,"H"] = relevel(as.factor(hcDados$H),ref="none")

summary(hcDados)


#Dados de custo dos cuidados de saúde: construir uma rede
dagCru = tabu(hcDados, score = "bde")
plot(dagCru)

fit = bn.fit(dagCru, hcDados, method = "mle")
graphviz.chart(fit, type = "barprob")



