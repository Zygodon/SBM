
library(signnet)
library(igraph)
library(tidygraph)
library(graphlayouts)
library(ggraph)
library(concaveman)
library(ggforce)

g0 <- readRDS("~/SBM/Qg0.rds")
g1 <- g0 |> 
  activate(edges)|>
  mutate(sign=ifelse(sgn=='associative', 1, -1))|>
  activate(nodes)|>
  filter(count>99)

balance_score(g1, method = "frustration")

count_signed_triangles(g1)
clu <- signed_blockmodel(g1, k = 3, alpha = 0.9, annealing = TRUE)
table(clu$membership)
g1<-g1|>activate(nodes)|>mutate(tribe=clu$membership)
tbl<-g1|>activate(nodes)|>as_tibble()

bb <- layout_as_backbone(g1, keep = 1)
ggraph(g1, layout = "manual", x = bb$xy[, 1], y = bb$xy[, 2]) +
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
  theme_graph() #+

ggblock(g1, clu$membership, show_blocks = TRUE, show_labels = TRUE)
