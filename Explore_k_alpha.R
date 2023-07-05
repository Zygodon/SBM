library(signnet)
library(tidygraph)
library(ggraph)
library(tidyverse)

g0 <- readRDS("~/SBM/Qg0.rds")
g1 <- g0 |> 
  activate(edges)|>
  mutate(sign=ifelse(sgn=='associative', 1, -1))|>
  activate(nodes)|>
  filter(count>99)

# Explore alpha and k
d<-tibble(k=0,alpha =0, C=0)
for (k in 2:6){
  for(a in 1:10){
   alpha<-0.85+(a/100)
   clu <- signed_blockmodel(g1, k = k, alpha = alpha, annealing = T)
   d<- d|>add_row(k=k, alpha=alpha, C=clu$criterion)
   cat(k, a, clu$criterion, '\n')
  }
}
d|>ggplot(aes(k,C, colour=as.factor(alpha)))+geom_point() + geom_smooth()

# Explore k for fixed alpha
d1<-tibble(i=0, k=0, C=0)
for(i in 1:50){
  for (k in 2:6){
    # alpha<-0.87
    alpha<-0.95
    clu <- signed_blockmodel(g1, k = k, alpha = alpha, annealing = T)
    d1<- d1|>add_row(i = i, k=k, C=clu$criterion)
    cat(i, k, clu$criterion, '\n')
  }
}

d1<-d1|>filter(i>0)
d1|>ggplot(aes(k,C))+geom_point(alpha=0.25) + geom_smooth() # +geom_jitter()

d2<-d1|>filter(k==4)|>select(C)

# Get 'best' model
repeat{
  clu <- signed_blockmodel(g1, k = 3, alpha = 0.9, annealing = TRUE)
  if (clu$criterion < 17.5) break
}
blocks <- clu$membership |> as_tibble()
