# libraries #########################
library("RMySQL")
library(tidyverse)
library(igraph)
library(ggraph)
library(tidygraph)
library(RColorBrewer)
library(sbm)

# Functions #######################
dbDisconnectAll <- function(){
  ile <- length(dbListConnections(MySQL())  )
  lapply( dbListConnections(MySQL()), function(x) dbDisconnect(x) )
  cat(sprintf("%s connection(s) closed.\n", ile))
}

###  RECOVER the_model #################
# the_model <-  read_rds("Q_SBM.rds")
# the_model <-  read_rds("Q_SBM_cov_L.rds")
the_model <-  read_rds("Q_SBM_cov_P.rds")

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

# Recover g1
g1 <- read_rds("Qg1.rds")

# Add LC memberships to the node properties
g1 <- g1 %>% activate("nodes") %>% mutate(latent_community = the_model$memberships)

# Generate block plot...
# Add EDGE latent_community membership, NA for edges between blocks.
# latent_community assigned only to edges between dyads within a block.
g1 <- g1 %>%
  activate(nodes) %>%
  morph(to_split, latent_community) %>%
  activate(edges) %>%
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

# Obtain the adjacency matrix ...
M <- as_adj(g1, type = "both", sparse = F)
Xm <- the_model$expectation
# to use in fit
fit <- tibble(x = as.vector(as.matrix(Xm)), y = as.vector(as.matrix(M)))
model <- glm( y ~ x, data = fit, family = binomial)
print(summary(model))

plot(fit %>%
       ggplot(aes(x, y)) +
       geom_point(colour = "dodgerblue3", alpha = 0.1) +
       geom_smooth(method = "glm", method.args = list(family = "binomial"), colour = "firebrick3") +
       labs(
         title = "E(data|model)", 
         x = "the_model$expectation",
         y = "Probability of observed link"
       ))

