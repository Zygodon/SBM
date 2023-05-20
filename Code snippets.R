######## DB FUNCTIONS #####
dbDisconnectAll <- function(){
  ile <- length(dbListConnections(MySQL())  )
  lapply( dbListConnections(MySQL()), function(x) dbDisconnect(x) )
  cat(sprintf("%s connection(s) closed.\n", ile))
}

# General SQL query
query <- function(q)
{
  # Remote DB with password
  con <- dbConnect(MySQL(), 
                   user  = "guest",
                   password    = "guest",
                   dbname="meadows",
                   port = 3306,
                   host   = "sxouse.ddns.net")
  rs1 = dbSendQuery(con, q)
  return(as_tibble(fetch(rs1, n=-1)))
  dbDisconnectAll()
}



#### FIND IN-GROUP DISSOCIATES
igd <- as_tbl_graph(G0 %>% activate(edges)
                    %>% filter(!is.na(edge_group))
                    %>% filter(sgn == "dissociative"))

isolates <- which(degree(igd)==0)
igd <- as_tbl_graph(delete.vertices(igd, isolates))

igd %>% ggraph(layout = "stress") + 
  geom_edge_link() + 
  geom_node_point(aes(fill = factor(group)),shape = 21, size = 4) +
  geom_node_text(aes(label = species), repel = T, size = 4, family = "serif") +
  ggtitle('In group dissociates') + 
  theme_graph()

igd3 <- igd %>% activate(nodes) %>% filter(group == 3)

igd3 %>% ggraph(layout = "stress") + 
  geom_edge_link() + 
  geom_node_point(aes(fill = factor(group)),shape = 21, size = 4) +
  geom_node_text(aes(label = species), repel = T, size = 4, family = "serif") +
  ggtitle('In group dissociates') + 
  theme_graph()


## SHORT BLOCK MATRIX PLOT
require(tidygraph)
gr <- G1 %>%
  activate(nodes) %>%
  morph(to_split, group) %>%
  activate(edges) %>%
  mutate(edge_group = as.character(.N()$group[1])) %>%
  unmorph()

#> Splitting by nodes

ggraph(
  gr, 'matrix', sort.by = group) +
  geom_edge_point(aes(colour = edge_group), mirror = TRUE, edge_size = 3) +
  scale_y_reverse() +
  coord_fixed() +
  labs(edge_colour = 'group') +
  ggtitle("SBM_Bernouilli")

##### MATRIX PLOT EDGE SIGN
require(tidygraph)
# gr <- G0 %>%
#   morph(to_split, group) %>%
#   activate(edges) %>%
#   mutate(edge_group = as.character(.N()$group[1])) %>%
#   unmorph()
# #> Splitting by nodes

ggraph(
  G0, 'matrix', sort.by = group) +
  scale_edge_colour_manual(values = c("black", "red")) +
  geom_edge_point(aes(colour = sgn), mirror = TRUE, edge_size = 1) +
  scale_y_reverse() +
  coord_fixed() +
  ggtitle("SBM_Bernouilli edge sign")


# MAKE STAR DIAGRAM FOR SINGLE SPECIES AND RETURN THE GRAPH
Star <- function(G0, sp){
  node <- V(G0)[which(V(G0)$species == sp)]
  # get their network neighborhood 
  nbrs <- ego(G0, order=1, nodes = node, mode = "all", mindist = 0)
  # turn the returned list of igraph.vs objects into a graph
  sg <- induced_subgraph(G0,unlist(nbrs))
  edgs <- incident(sg,V(sg)[which(V(sg)$species == sp)])
  sg2 <- delete_edges(sg, E(sg)[which(!E(sg) %in% edgs)])
  isolated <- which(degree(sg2)==0)
  sg3 <-  delete.vertices(sg2, isolated)
  sg4 <- as_tbl_graph(sg3) %>% activate(nodes) %>% mutate(importance = centrality_degree())
  # lo <- create_layout(sg4, layout = 'igraph', algorithm = 'star', center = V(sg4)[which(V(sg4)$species==sp)])
  print(sg4 %>% ggraph(layout = "stress") +
    scale_edge_width_continuous(range = c(0.5, 2.0)) +
    scale_edge_colour_manual(values = c("limegreen", "magenta")) +
    # scale_fill_brewer(palette = "Accent") +
    geom_edge_link0(aes(colour = sgn, width = abs(weight), alpha = weight)) +
    geom_node_point(aes(fill = factor(group)), shape = 21, size = 8) +
    geom_node_text(aes(label = species), repel = TRUE, size = 4, family = "serif") +
    coord_fixed() +
    theme_graph())
  return(sg4)
}

# EXTRACT A CONTINGENCY TABLE
Contingency <- function(edges, A, B){
  ctd <- (edges %>% filter(A == "Rhinanthus_minor", B == "Lathyrus_montanus") 
          %>% select(a,b,c,d))
  return(matrix(unlist(ctd),2,2, byrow = T, dimnames = list(c("0", "1"), c("0", "1"))))
}


### SNIPPET FOR NAMES OF HIGHEST DEGREE NODES
V(G0)$species[which(degree(G0)==max(degree(G0)))]


### PLOT SUBGRAPH ###
nodes_of_interest <- c("Cirsium_arvense")

# select the nodes having these names
sub_nodes <- V(G0)[species %in% nodes_of_interest]
# get their network neighborhood 
sub_nbrs <- ego(G0, order=1, nodes = sub_nodes, mode = "all", mindist = 0)

# turn the returned list of igraph.vs objects into a graph
sub_graph <- induced_subgraph(G0,unlist(sub_nbrs))
plot.igraph(sub_graph, vertex.size=degree(sub_graph), #*5,
            layout=layout_with_fr,
            vertex.label=V(sub_graph)$species,
            edge.width=abs(E(sub_graph)$weight), 
            edge.color=ifelse(E(sub_graph)$weight > 0, "green","red"))


### EXTRACT SPECIES (NODE) DEGREE FROM GRAPH
sp_degree <- tibble(species = V(G0)$species, degree = degree(G0))


#### LARGE SNIPPET FOR ELABORATE BLOCK DIAGRAM

# FUNCTION POSXY needed for rectangles in block diagram
posxy <-  function(hvlines, comms_order)
{
  k <- length(hvlines)
  locs <- c(0, hvlines)
  # ys <- matrix(rep(0, 4*k), 2*k, 2)         # Always 4 corners to a rectangle
  ys <- matrix(rep(0, 2*length(locs)), length(locs), 2)         # Always 4 corners to a rectangle
  # for(i in 1:4) 
  for(i in 1:length(locs)) 
  {
    ys[i,] <- rep(locs[i], 2) # y coords top and bottom hvlines
  }
  y2 <- matrix(rep(0, 4 * k), k, 4)       # matrix, zeros
  # Fill y2
  for (i in 1:k)
  {
    y2[i,] <- c(ys[i,], ys[i + 1,]) # y coords for each group
  }
  x2 <- y2[, c(1,4,3,2)]                  # Swap columns 2, 4
  y3 <- as.vector(t(y2))
  x3 <- as.vector(t(x2))
  
  pos <- data.frame(
    comm = rep(comms_order, each = 4),
    x = x3,
    y = y3)
  return(pos)
}


# # Get membership from the best model
# mmZ <- the_model$memberships[[which.max(the_model$ICL)]]$Z
# sbm_comms <- apply(mmZ, 1, which.max)
# 
# # Add node membership to the graph
# G0 <-(G0 %>% activate(nodes) 
#       %>% mutate(sbm_comm = as.factor(sbm_comms)))
# # Add community membership to edges within each community
# # NOTE: edges BETWEEN communities flagged as NA
# G0 <- (G0 %>% activate(edges)
#        %>% mutate(sbm_comm = ifelse(.N()$sbm_comm[from] == .N()$sbm_comm[to], .N()$sbm_comm[from], NA)))
# 
# # Hairball with communities
# lo <- layout_with_fr(G0)
# G0 %>% activate(edges) %>% ggraph(layout = lo) + 
#   geom_edge_link(colour = "black", alpha = 0.2) +
#   geom_node_point(aes(fill = sbm_comm), size = 4, show.legend = T, shape = 21, alpha = 1) +
#   scale_fill_brewer(palette = "Dark2", na.value = "grey50") +
#   ggtitle('SBM Poisson') +
#   theme_minimal() + th_no_axes( ) +
#   guides(fill = guide_legend(override.aes = list(size=5))) 
# # ggsave("SBM_Poisson_hairball.jpg", width = 10, height = 10, units = "cm")
# 
# # Sort nodes by community size, SBM community and
# # species name (value) prior to looking at block matrix
# no0 <- G0 %>% activate(nodes) %>% as_tibble(.)
# community_size <- no0 %>% group_by(sbm_comm) %>% summarise(size = n())
# G0 <- G0 %>% activate(nodes) %>% left_join(community_size, by = "sbm_comm")
# G0 <- G0 %>% activate(nodes) %>% arrange(desc(size), sbm_comm, species)
# # Re-do no0, sorted
# no0 <- G0 %>% activate(nodes) %>% as_tibble(.)
# # Convenient tibble
# eg0 <- G0 %>% activate(edges) %>% as_tibble(.)
# 
# # Matrix
# # This plot shows edges between communities as NA
# pal <- brewer.pal(model, "Dark2") # model: the selected SBM
# no0 <- no0 %>% mutate(axis_colour = pal[sbm_comm])
# # X, Y coordinates for text label reporting ICL
# text_x <- length(row.names(no0)) - 20
# text_y <-  -3
# # Text to show parameters on plot
# txt <- paste(paste("ICL =", as.integer(max(the_model$ICL)), sep = " "), 
#              paste("p_val <", p_lim, sep = " "),
#              paste("LOR low =", format(lor_low, digits = 2),
#                    "LOR high =", format(lor_high, digits = 2), sep = " "), sep = "\n")
# # Horizontal and vertical lines between communities
# hvlines <- (no0 %>% group_by(sbm_comm)
#             %>% summarise(n = n())
#             %>% arrange(desc(n))
#             %>% mutate(cs = cumsum(n) + 0.5))
# # Rectangles
# rectangles <- posxy(hvlines$cs, hvlines$sbm_comm)
# 
# # You have to run this separately to actually draw the block model
# ggraph(G0, 'matrix', sort.by = NULL) + 
#   geom_polygon(data = rectangles, aes(x = x, y = y, fill = as.factor(comm), group = comm), alpha = 1) +
#   scale_fill_brewer(palette = "Dark2") +
#   guides(fill = guide_legend(title = "community", override.aes = list(alpha = 1))) +
#   geom_edge_point(aes(colour = lor, size = a), edge_alpha = 1, mirror = TRUE) +
#   scale_edge_colour_gradient2(low = "black",
#   mid = "#f7f7f7", high = "#af8dc3", midpoint = 0) +
#   geom_edge_point(edge_shape = 3, edge_size = 0.1, edge_alpha = 1, mirror = TRUE) +
#   guides(edge_size = guide_legend(title = "instances")) +
#   geom_vline(xintercept = c(0, hvlines$cs), alpha = 0.5, colour = "grey") +
#   geom_hline(yintercept = c(0, hvlines$cs), alpha = 0.5, colour = "grey") +
#   ## scale_edge_alpha(trans = 'reverse') + # Reverse the alpha scale for negative LOR
#   scale_y_reverse(breaks = seq(1, length(row.names(no0)), by = 1), labels = no0$species, "from") +
#   scale_x_continuous(breaks = seq(1, length(row.names(no0)), by = 1), labels = no0$species, "to") +
#   coord_fixed() +
#   theme_bw() +
#   theme(axis.text.x = element_text(size = text_size, angle = 90, colour = no0$axis_colour, 
#                                    face = 'bold', hjust = 1)) +
#   theme(axis.text.y = element_text(size = text_size, colour = no0$axis_colour, face = 'bold')) +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), axis.line = element_line(colour = "black")) +
#   theme(legend.box = "horizontal") +
#   ggtitle('SBM Bernoulli positive LOR') +
#   annotate("text", 
#            label = txt, 
#            x = text_x, 
#            y = text_y, 
#            size = 2, 
#            colour = "black")
# 
# # ggsave("SBM_Bernoulli_positive.png", width = 20, height = 20, units = "cm")
# no0 %>% select(species, sbm_comm) %>% arrange(species) %>% write.csv("species_positive.csv")
# 
# # Blockmodel boxplot
# # Identify insignificant communities
# 
# PP <- the_model$model_parameters[model][[1]]$p
# p <-  diag(PP)
# # Set diag(PP) NA so block_p not included in boxplot
# diag(PP) <- NA
# 
# guilds <- stack(as.data.frame(PP))
# colnames(guilds) <- c("out_p", "guild")
# guild_names <- guilds %>% select(guild) %>% group_by(guild) %>% distinct() %>% ungroup()
# block_p <- tibble(guild = guild_names$guild, 
#                   p = p,
#                   guild_int = as.integer(as.factor(guild)))
# # Not plotted - this is just to get bp_stats
# g <- ggplot() + 
#   geom_boxplot(data = guilds, aes(x = guild, y = out_p)) +
#   geom_point(data = block_p, aes(x = guild, y = p), colour = "red") 
# plot(g) 
# 
# bp_stats <- ggplot_build(g)$data[[1]]
# block_p <- (block_p %>% mutate(lower_hinge = bp_stats$lower)
#             %>% mutate(upper_hinge = bp_stats$upper)
#             %>% mutate(strong = (p > upper_hinge | p < lower_hinge)))
# # Plot note log y axis
# g1 <- ggplot() +
#   geom_boxplot(data = guilds, aes(x = guild, y = out_p)) +
#   geom_point(data = block_p, aes(x = guild, y = p), colour = "red") +
#   scale_y_log10()
# plot(g1)

## USE OF EDGE_IS_INCIDENT   
ggrp1 %>%
  activate(edges) %>%
  filter(edge_is_incident(6)) %>%
  as_tibble() %>% select(lor) %>% colSums()
# ggraph(layout = "nicely") +
# geom_edge_link() +
# geom_node_point(size = 10, fill = "white", shape = 21) +
# geom_node_text(aes(label = name)) +
# theme_graph()

### LC expressions by quadrat
quadrat_xp %>% group_by(lc) %>% summarise(max_xp = max(xp))
