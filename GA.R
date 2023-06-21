### libraries #########################
library(tidyverse)
library(sbm)
library(GA)
library(ROCit)

ObjF <- function(chrom){
  g <- sg_ass |> activate(nodes)|>filter(name %in% spp[which(chrom==1)])
  M <- as_adj(g, type = "both", sparse = F)
  the_model <- estimateSimpleSBM(M, 'bernoulli', estimOptions = list(plot = F )) #TRUE))
  Xm <- the_model$expectation
  fit <- tibble(x = as.vector(as.matrix(Xm)), y = as.vector(as.matrix(M)))
  roc <- rocit(score = fit$x, class = fit$y)
  return(roc$AUC)
}

g0 <- readRDS("~/SBM/Qg0.rds")

sg_ass <- g0|> activate(nodes)|>
  filter(count > 99)|>
  activate(edges)|> 
  filter(lor > 0)

spp<-sg_ass|>activate(nodes)|>as_tibble()|>pluck(1)
n_spp <- with_graph(sg_ass, graph_order()) #36
chrom<-rbinom(n_spp, 1, 0.7)
print(ObjF(chrom))


