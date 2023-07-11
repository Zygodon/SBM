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

# Run signed_blockmodel 500 times. Record species I and J in the same block
# ON EACH RUN. Sum the matrices for each run to get an estimate of consistency.
# That is, how consistently I and J are in the same block, independent of its 
# block label.
self_cm<-matrix(data=0, nrow=36, ncol=36, byrow=TRUE)

for(t in 1:50){
  m_self<-matrix(data=0, nrow=36, ncol=36, byrow=TRUE)
  clu <- signed_blockmodel(g1, k = 3, alpha = 0.64, annealing = TRUE)
  # repeat{
  #   clu <- signed_blockmodel(g1, k = 3, alpha = 0.87, annealing = TRUE)
  #   if (clu$criterion < 22.1) break
  # }
  for(i in 1:36){
    for(j in 1:36){
      m_self[i,j]<-ifelse(clu$membership[i]==clu$membership[j], 1,0)
    }
  }
  self_cm<-self_cm+m_self
  print(t)
}
head(self_cm)
# plot(density(self_cm))

hist(self_cm[which(upper.tri(self_cm, diag=F))])
plot(density(self_cm[which(upper.tri(self_cm, diag=F))]))

colnames(self_cm)<-g1|>activate(nodes)|>select(name)|>as_tibble()|>pluck(1)
rownames(self_cm)<-g1|>activate(nodes)|>select(name)|>as_tibble()|>pluck(1)

##### Develop ...
consistencies<-as_tbl_graph(self_cm)|>activate(edges)|>select(weight)|>as_tibble()
g2<-g2|>activate(edges)|>left_join(consistencies) # sign -1 edges NA
g2<-g2|>
  activate(edges)|>
  mutate(consist=ifelse(is.na(weight),0, weight))|>
  select(-weight)
g2<-g2|>
  activate(edges)|>
  mutate(c=ifelse(between(consist, 17, 38), FALSE,TRUE))

# Remove the confused dyads

g3<-g2|>
  filter(c)|>
  select(-c)

clu <- signed_blockmodel(g3, k = 3, alpha = 0.64, annealing = TRUE)

g3<-g3|>activate(nodes)|>mutate(block=clu$membership)

ggblock(
  g3,
  clu$membership,
  show_blocks = T,
  show_labels = T
)

# Get the confused dyads
tbl<-g2|>activate(edges)|>filter(!c)|>as_tibble()|>select(A, B)
write.csv(tbl, 'Confused_dyads.csv')

# Buld model k=4, including dyads which were confused for k=3
clu4 <- signed_blockmodel(g1, k = 4, alpha = 0.64, annealing = TRUE)
ggblock(
  g1,
  clu4$membership,
  show_blocks = T,
  show_labels = T
)

for(i in 1:10){
  clu3 <- signed_blockmodel(g1, k = 3, alpha = 0.64, annealing = TRUE)
  print(clu3$membership|>as_tibble()|>group_by(value)|>count())
}

# Blockmodel with cluster counts 4, 19, 13
# write_rds(clu3, 'Signed_blockmodel_k3.rds')
### TAKE THIS AS THE DEFINITIVE SIGNED BLOCKMODEL.
g3<-g1|>activate(nodes)|>mutate(k3_group=clu3$membership)
tbl<-g3|>activate(nodes)|>as_tibble()|>select(name, k3_group)
write_csv(tbl, 'Signed_groups.csv')
ggblock(
  g3,
  clu3$membership,
  show_blocks = T,
  show_labels = T
)

