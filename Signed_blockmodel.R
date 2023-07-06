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

# Get 'best' model
repeat{
  clu <- signed_blockmodel(g1, k = 3, alpha = 0.9, annealing = TRUE)
  if (clu$criterion < 17.5) break
}
blocks <- clu$membership |> as_tibble()
g1<-g1|>activate(nodes)|>mutate(block=clu$membership)
tbl<-g1|>activate(nodes)|>as_tibble()|>select(name, block)

write_rds(g1, "Signed_g1.rds")
write_rds(clu, "Signed_blockmodel.rds")
