### libraries #########################
library(tidyverse)
library(igraph)
library(ggraph)
library(tidygraph)
library(ggforce)
library(graphlayouts)
library(oaqc)
library(concaveman)
library(RColorBrewer)
library(sbm)
library(ROCit)

g0 <- readRDS("~/SBM/Qg0.rds")

# g1 <- g0 |> activate(nodes) |>
#   mutate(n_dyads = local_size(order = 1, mindist = 0))
g1 <- g0

sg_ass <- g1|> activate(nodes)|>
  filter(count > 99)|>
  mutate(n_dyads = local_size(order = 1, mindist = 0))|>
  mutate(ass_importance = centrality_degree())|>
  activate(edges)|> 
  filter(lor > 0)

# Experiemnt with group_edge_betweenness.
# sg_ass <- sg_ass |> activate(nodes) %>%
# mutate(group = group_edge_betweenness())

M <- as_adj(sg_ass, type = "both", sparse = F)
# and covariate matrix if needed
the_model <- estimateSimpleSBM(M, 'bernoulli', estimOptions = list(plot = F )) #TRUE))
print(the_model$connectParam)
Xm <- the_model$expectation
# to use in fit
fit <- tibble(x = as.vector(as.matrix(Xm)), y = as.vector(as.matrix(M)))
model <- glm( y ~ x, data = fit, family = binomial)
print(summary(model))

# ROC curve
roc <- ROCit::rocit(score = fit$x, class = fit$y) 
plot(roc)
print(summary(roc))

ciROC_emp95 <- ROCit::ciROC(roc, level = 0.95)
plot(ciROC_emp95, legend = TRUE)

# Add block memberships to the node properties
sg_ass <- sg_ass %>% activate("nodes") %>% mutate(ass_block = the_model$memberships)

# Add EDGE latent_community membership, NA for edges between blocks.
# latent_community assigned only to edges between dyads within a block.
sg_ass <- sg_ass %>%
  activate(nodes) %>%
  morph(to_split, ass_block) %>%
  activate(edges) %>%
  # All the edges with at least one end in latent_community
  mutate(ass_edge_block = .N()$ass_block[1]) %>%
  unmorph()

##################

sg_diss <- g1|> activate(nodes)|>
  filter(count > 99)|>
  activate(edges)|> 
  filter(lor < 0)

sg_diss <- sg_diss |> activate(nodes) %>%
  mutate(group = group_edge_betweenness())

M <- as_adj(sg_diss, type = "both", sparse = F)
# and covariate matrix if needed
the_model <- estimateSimpleSBM(M, 'bernoulli', estimOptions = list(plot = F )) #TRUE))
print(the_model$connectParam)

# Add LC memberships to the node properties
sg_diss <- sg_diss %>% activate("nodes") %>% mutate(diss_block = the_model$memberships)

# Add EDGE latent_community membership, NA for edges between blocks.
# latent_community assigned only to edges between dyads within a block.
sg_diss <- sg_diss %>%
  activate(nodes) %>%
  morph(to_split, diss_block) %>%
  activate(edges) %>%
  # All the edges with at least one end in latent_community
  mutate(diss_edge_block = .N()$diss_block[1]) %>%
  unmorph()

##################

#sg <- sg_diss %>% graph_join(sg_ass, join_by(name,count, frequency, n_dyads))
sg <- sg_diss %>% graph_join(sg_ass, join_by(name,count, frequency))

sg |> ggraph(layout = 'kk') +
  geom_edge_link(aes(colour=as.factor(sign(lor)))) +
  geom_node_point(aes(colour=as.factor(ass_block)), size = 8) +
  geom_node_text(aes(label = name), colour = 'black', vjust = 0.4) +
  ggtitle('Joining graphs') +
  theme_graph()

tbl <- sg|>activate(nodes)|>as_tibble() |> select(name, ass_block, diss_block)

bb <- layout_as_backbone(sg_ass, keep = 1)
ggraph(sg, layout = "manual", x = bb$xy[, 1], y = bb$xy[, 2]) +
  scale_edge_colour_manual(values = c("black", "orangered3"), guide = guide_legend("Sign")) +
  geom_edge_link(aes(colour=sgn), alpha=0.5) + 
  geom_node_point(aes(fill = as.factor(ass_block), size=ass_importance), shape = 21) +
  geom_mark_hull(
    aes(x, y, group = ass_block, fill = as.factor(ass_block)),
    concavity = 4,
    expand = unit(2, "mm"),
    alpha = 0.25) +  
  scale_fill_brewer(palette = "Dark2") +
  facet_edges(~sgn) +
  theme_graph() #+
  #theme(legend.position = "none")
  
############# Count in-edges and out-edges
  
# Add LC to the dyad ends
sg <- sg |> activate(edges) |> 
  mutate(block_A = the_model$memberships[from]) |>
  mutate(block_B = the_model$memberships[to])

edges<-sg|>
  activate(edges)|>
  as_tibble()|>
  select(from, to, A, B, sgn, block_A, block_B)
edges<-edges|>mutate(in_block=block_A==block_B)
edges|>select(sgn,in_block)|>group_by(in_block,sgn)|>count(sgn)|>print()

