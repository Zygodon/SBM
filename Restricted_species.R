### libraries #########################
library(tidyverse)
library(igraph)
library(ggraph)
library(tidygraph)
library(RColorBrewer)
library(sbm)

g0 <- readRDS("~/SBM/Qg0.rds")

g1 <- g0 |> activate(nodes) |>
  mutate(n_dyads = local_size(order = 1, mindist = 0))

tbl<-g1|>activate(nodes)|>as_tibble()

tbl |> ggplot(aes(log10(count), n_dyads)) + geom_point() + geom_smooth()

sg_ass <- g1|> activate(nodes)|>
  filter(count > 99)|>
  activate(edges)|> 
  filter(lor >0)

sg_ass <- sg_ass |> activate(nodes) %>%
mutate(group = group_edge_betweenness())

tbl <- sg_ass|>activate(nodes)|>as_tibble()

M <- as_adj(sg_ass, type = "both", sparse = F)
# and covariate matrix if needed
P <- as_adj(sg_ass, attr = "p_obs", type = "both", sparse = F)
# No covariate
the_model <- estimateSimpleSBM(M, 'bernoulli', estimOptions = list(plot = T )) #TRUE))
# With covariate
# the_model <- estimateSimpleSBM(M, 'bernoulli', covariates = list(P), estimOptions = list(plot =  T)) #TRUE))
print(the_model$connectParam)

# Add LC memberships to the node properties
sg_ass <- sg_ass %>% activate("nodes") %>% mutate(block = the_model$memberships)


# Generate block plot...
# Add EDGE latent_community membership, NA for edges between blocks.
# latent_community assigned only to edges between dyads within a block.
sg_ass <- sg_ass %>%
  activate(nodes) %>%
  morph(to_split, block) %>%
  activate(edges) %>%
  # All the edges with at least one end in latent_community
  mutate(edge_block = .N()$block[1]) %>%
  unmorph()

plot(ggraph(
  sg_ass, 'matrix', sort.by = block) +
    scale_edge_colour_brewer(palette = "Accent",na.value="grey") +
    geom_edge_point(aes(colour = as.factor(edge_block)), mirror = TRUE, edge_size = 2, edge_shape=16) +
    scale_y_reverse() +
    coord_fixed() +
    labs(edge_colour = 'block') +
    ggtitle("SBM")) +
  # theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank())

# Obtain the adjacency matrix ...
M <- as_adj(sg_ass, type = "both", sparse = F)
Xm <- the_model$expectation
# to use in fit
fit <- tibble(x = as.vector(as.matrix(Xm)), y = as.vector(as.matrix(M)))
model <- glm( y ~ x, data = fit, family = binomial)
print(summary(model))

plot(fit %>%
       ggplot(aes(x, y)) +
       geom_point(colour = "dodgerblue3", alpha = 0.02) +
       geom_smooth(method = "glm", method.args = list(family = "binomial"), colour = "firebrick3") +
       labs(
         title = "E(data|model)", 
         x = "the_model$expectation",
         y = "Probability of observed link"
       ))

# ROC curve
roc <- ROCit::rocit(score = fit$x, class = fit$y) 
plot(roc)
print(summary(roc))

ciROC_emp95 <- ROCit::ciROC(roc, level = 0.95)
plot(ciROC_emp95, legend = TRUE)

tbl <- sg_ass|>activate(nodes)|>as_tibble()







# tsg_ass|>
#   ggraph(layout = "stress") +
#   scale_edge_colour_manual(values = c("grey80", "firebrick3"), guide = guide_legend("Sign")) +
#   geom_edge_link(aes(colour = sgn),width = 1, alpha = 0.5) +
#   # geom_node_point(aes(size = frequency), pch = 21, fill = 'navajowhite1') +
#   geom_node_point(aes(size = count, fill = as.factor(group)), pch = 21) +
#   # scale_size(name="impact") + #, range = c(5, 15)) +
#   geom_node_text(aes(label = name), colour = 'black', repel = T) +
#   # expand pads the x axis so the labels fit onto the canvas.
#   scale_x_continuous(expand = expansion(mult = 0.2)) +
#   scale_y_continuous(expand = expansion(mult = 0.1)) +
#   ggtitle("sg_ass") +
#   #facet_edges(~sgn) +
#   theme_graph()
# 

