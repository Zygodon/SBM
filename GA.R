### libraries #########################
library(tidyverse)
library(sbm)
library(GA)
library(ROCit)

ObjF <- function(x){
  # print(x)
  g <- sg_ass |> activate(nodes)|>filter(name %in% spp[which(x==1)])
  M <- as_adj(g, type = "both", sparse = F)
  the_model <- estimateSimpleSBM(M, 'bernoulli', 
                                 estimOptions = list(verbosity=0, plot = F, nbBlocksRange=c(2, 6)))
  Xm <- the_model$expectation
  fit <- tibble(x = as.vector(as.matrix(Xm)), y = as.vector(as.matrix(M)))
  roc <- rocit(score = fit$x, class = fit$y)
  return(roc$AUC)
}

g0 <- readRDS("~/SBM/Qg0.rds")

sg_ass <- g0|> activate(nodes)|>
  filter(count > 69)|>
  activate(edges)|> 
  filter(lor > 0)

spp<-sg_ass|>activate(nodes)|>as_tibble()|>pluck(1)
n_spp <- with_graph(sg_ass, graph_order()) #36
chrom<-rbinom(n_spp, 1, 0.7)

# print(ObjF(chrom))

GAmodel <- ga(type = "binary", ObjF, nBits=n_spp)

g <- sg_ass |> activate(nodes)|>filter(name %in% spp[which(GAmodel@solution==1)])
M <- as_adj(g, type = "both", sparse = F)
the_model <- estimateSimpleSBM(M, 'bernoulli',
                               estimOptions = list(verbosity=0, plot = F, nbBlocksRange=c(2, 6)))

########
g <- g %>% activate("nodes") %>% mutate(latent_community = the_model$memberships)

g <- g %>%
  activate(nodes) %>%
  morph(to_split, latent_community) %>%
  activate(edges) %>%
  # All the edges with at least one end in latent_community
  mutate(edge_latent_community = .N()$latent_community[1]) %>%
  unmorph()

plot(ggraph(
  g, 'matrix', sort.by = latent_community) +
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


plot(fit %>%
       ggplot(aes(x, y)) +
       geom_point(colour = "dodgerblue3", alpha = 0.02) +
       geom_smooth(method = "glm", method.args = list(family = "binomial"), colour = "firebrick3") +
       labs(
         title = "E(data|model)", 
         x = "the_model$expectation",
         y = "Probability of observed link"
       ))



