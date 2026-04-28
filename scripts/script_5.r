
library(bnlearn)
library(Rgraphviz)
set.seed(124321)


### 5) Rede média: bootstrapping

#Rede média: bootstrapping (exemplo)
boot = boot.strength(data = asia, R=1000, algorithm = 'hc', algorithm.args = list(score = 'bde'))

par(mar=c(3,3,3,3))
plot(boot)
  
boot.avg = averaged.network(boot)
undirected.arcs(boot.avg)
graphviz.plot(boot.avg, layout = "dot")

fit = bn.fit(boot.avg, asia, method ="mle")
graphviz.chart(fit, type = "barprob")


#Rede média: bootstrapping (outro limiar)
boot.avg = averaged.network(boot,threshold = 0.3)
undirected.arcs(boot.avg)
graphviz.plot(boot.avg, layout = "dot")

fit = bn.fit(boot.avg, asia, method ="mle")
graphviz.chart(fit, type = "barprob")

