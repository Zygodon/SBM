library(signnet)
library(igraph)
library(tidygraph)
library(graphlayouts)
library(ggraph)
library(concaveman)
library(ggforce)
library(tidyverse)

g0 <- readRDS("~/SBM/Qg0.rds")
g1 <- g0 |> 
  activate(edges)|>
  mutate(sign=ifelse(sgn=='associative', 1, -1))|>
  activate(nodes)|>
  filter(count>99)

# Count how many times species A was in the same block as species B.
cm<-matrix(data=0, nrow=36, ncol=36, byrow=TRUE)
# clu <- signed_blockmodel(g1, k = 3, alpha = 0.9, annealing = TRUE)
# old_membership <- clu$membership

for(t in 1:500){
  m<-matrix(data=0, nrow=36, ncol=36, byrow=TRUE)
  clu <- signed_blockmodel(g1, k = 3, alpha = 0.9, annealing = TRUE)
  for(i in 1:36){
    for(j in 1:36){
      m[i,j]<-ifelse(clu$membership[i]==clu$membership[j], 1,0)
    }
  }
  cm<-cm+m
  print(t)
}
head(cm)



