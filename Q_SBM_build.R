# libraries #########################
library(tidyverse)
library(igraph)
library(ggraph)
library(tidygraph)
library(RColorBrewer)
library(sbm)

g0 <-  read_rds("Qg0.rds")

# Obtain the adjacency matrix ...
M <- as_adj(g0, type = "both", sparse = F)
# and covariate matrix if needed
P <- as_adj(g0, attr = "p_obs", type = "both", sparse = F)

# And finally, build the model!
# No covariate
# the_model <- estimateSimpleSBM(M, 'bernoulli', estimOptions = list(plot = T )) #TRUE))
# With covariate
the_model <- estimateSimpleSBM(M, 'bernoulli', covariates = list(P), estimOptions = list(plot =  F)) #TRUE))

pm <- as_tibble(the_model$connectParam)
pm1 <- pm %>%
  mutate_if(
    is.numeric,
    function(x) {
      formatC(x, digits = 3, format = "f")
    })
print(pm1)
rm(pm1)

print(the_model$ICL)

# Add LC memberships to the node properties
g1 <- g0 %>% activate("nodes") %>% mutate(latent_community = the_model$memberships)

# Generate block plot...
# Add EDGE latent_community membership, NA for edges between blocks.
# latent_community assigned only to edges between dyads within a block.
g1 <- g1 %>%
  activate(nodes) %>%
  morph(to_split, latent_community) %>%
  activate(edges) %>%
  # All the edges with at least one end in latent_community
  mutate(edge_latent_community = .N()$latent_community[1]) %>%
  unmorph()

# Matrix plot. Points are EDGES. Axes are NODES, i.e plants.
# Edges link the plant represented on the vertical axis to the
# corresponding plant on the horizontal axis.
plot(ggraph(
  g1, 'matrix', sort.by = latent_community) +
    scale_edge_colour_brewer(palette = "Accent",na.value="grey") +
    geom_edge_point(aes(colour = as.factor(edge_latent_community)), mirror = TRUE, edge_size = 2, edge_shape=16) +
    scale_y_reverse() +
    coord_fixed() +
    labs(edge_colour = 'latent_community') +
    ggtitle("SBM")) +
  # theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank())

# Plot edge sign
plot(ggraph(
  g1, 'matrix', sort.by = latent_community) +
    scale_edge_colour_manual(values = c("black", "red")) +
    geom_edge_point(aes(colour = sgn), mirror = TRUE, edge_size = 1) +
    scale_y_reverse() +
    coord_fixed() +
    ggtitle("SBM edge sign"))

# Save g1 here AFTER adding stuff from the_model (so g1 different from g0

write_rds(g1, "Qg1.rds")
## write_rds(the_model, "Q_SBM.rds")
write_rds(the_model, "Q_SBM_cov_P.rds")
