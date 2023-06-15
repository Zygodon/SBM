# libraries #########################
library(tidyverse)
library(igraph)
library(ggraph)
library(tidygraph)
library(ROCit)

# Functions #######################
dbDisconnectAll <- function(){
  ile <- length(dbListConnections(MySQL())  )
  lapply( dbListConnections(MySQL()), function(x) dbDisconnect(x) )
  cat(sprintf("%s connection(s) closed.\n", ile))
}

###  RECOVER the_model #################
the_model <-  read_rds("Q_SBM_cov_P.rds")

# Recover g1
g1 <- read_rds("Qg1.rds")

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
       geom_point(colour = "dodgerblue3", alpha = 0.02) +
       geom_smooth(method = "glm", method.args = list(family = "binomial"), colour = "firebrick3") +
       labs(
         title = "E(data|model)", 
         x = "the_model$expectation",
         y = "Probability of observed link"
       ))

# ROC curve
roc <- rocit(score = fit$x, class = fit$y) 
plot(roc)
print(summary(roc))

ciROC_emp95 <- ciROC(roc, level = 0.95)
plot(ciROC_emp95, legend = TRUE)
