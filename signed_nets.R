# INVESTIGATIONS WITH PACKAGE SIGNNET.

library(signnet)
library(igraph)
library(tidygraph)
library(graphlayouts)
library(ggraph)
library(concaveman)
library(ggforce)
library(tidyverse)
library(colorspace)

g0 <- readRDS("~/SBM/Qg0.rds")
g1 <- g0 |> 
  activate(edges)|>
  mutate(sign=ifelse(sgn=='associative', 1, -1))|>
  activate(nodes)|>
  filter(count>99)

### EXAMINE SIGNED TRIANGLES ################
# NOTE: prior to and independent of any models.
# balance_score(g1, method = "frustration")
g1_triangles <- count_signed_triangles(g1)|>as_tibble()|>mutate(type=c('+++', '++-', '+--', '---'))
g1_triangles <- g1_triangles|>mutate(balance=ifelse(type %in% c('+++', '+--'), 'balanced', 'unbalanced'))

# Shuffle + and - edges to compare distribution with observed counts (g1_triangles)
# sgn <- g1 |> activate(edges)|>select(sign)|>as_tibble()
# smpl <- sgn|>pluck(3)|>sample() # shuffle
# g2 <- g1|>activate(edges)|>select(-sign)|>mutate(sign=smpl)

x <- seq(1:1000)
ShuffleSign <- function(x){
  sgn <- g1 |> activate(edges)|>select(sign)|>as_tibble()
  smpl <- sgn|>pluck(3)|>sample() #shuffle
  g2 <- g1|>activate(edges)|>select(-sign)|>mutate(sign=smpl)
  return(count_signed_triangles(g2))
}
st <- map_df(x, ShuffleSign)
summary(st)
piv_st <- st |> pivot_longer(cols=c(1,2,3,4), names_to = "type") 
piv_st <- piv_st|>mutate(balance=ifelse(type %in% c('+++', '+--'), 'balanced', 'unbalanced'))

# This is perhaps the most convincing evidence for intransitive competition.
plot1 <- ggplot(piv_st, aes(type, value, fill=balance)) + 
  geom_boxplot(alpha=0.5) +
  geom_point(data = g1_triangles, aes(x = type, y = value, colour=balance), pch=19, size=10, alpha=0.8)  +
  geom_point(data = g1_triangles, aes(x = type, y = value), colour = 'black', pch=8, size=3) +
  scale_colour_discrete_qualitative() +
  scale_fill_discrete_qualitative() 
  # scale_colour_brewer(palette = "Dark2") +
  # scale_fill_brewer(palette = "Dark2")
plot(plot1)

# MAKE SIGNED BLOCKMODEL ##########################
# signed blockmodel from package signnet. High value for alpha penalises in_block positives.
# clu <- signed_blockmodel(g1, k = 3, alpha = 0.9, annealing = TRUE)
clu <- signed_blockmodel(g1, k = 3, alpha = 0.9, annealing = TRUE)
table(clu$membership)
g1<-g1|>activate(nodes)|>mutate(tribe=clu$membership)
tribes <- g1|>activate(nodes)|>as_tibble() |> select(name, tribe)

bb <- layout_as_backbone(g1, keep = 1)
plot(ggraph(g1, layout = "manual", x = bb$xy[, 1], y = bb$xy[, 2]) +
  scale_edge_colour_manual(values = c("black", "orangered3"), guide = guide_legend("Sign")) +
  geom_edge_link(aes(colour=sgn), alpha=0.5) + 
  geom_node_point(aes(fill = as.factor(tribe), size=count), shape = 21) +
  geom_mark_hull(
    aes(x, y, group = tribe, fill = as.factor(tribe)),
    concavity = 4,
    expand = unit(2, "mm"),
    alpha = 0.25) +  
  scale_fill_brewer(palette = "Dark2") +
  facet_edges(~sgn) +
  theme_graph())

plot(ggblock(g1, clu$membership, show_blocks = TRUE, show_labels = TRUE))


