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

### DATA WRANGLING #####################
# GET DATA FROM DB
# Remote DB with password
con <- dbConnect(MySQL(), 
                 user  = "guest",
                 password    = "guest",
                 dbname="meadows",
                 port = 3306,
                 host   = "sxouse.ddns.net")

q <- sprintf('select quadrat_id, survey_id, assembly_name, species.species_name from surveys
      join quadrats on quadrats.survey_id = surveys_id
      join visit_dates on quadrats.vd_id = visit_dates.vds_id
      join records on records.quadrat_id = quadrats_id
      join species on species.species_id = records.species_id where species.species_id != 4 and
      major_nvc_community like "MG%%" and quadrat_size = "2x2";')

# NOTE the double %% to escape the % formatting character

rs1 = dbSendQuery(con, q)
d <- as_tibble(fetch(rs1, n=-1))
dbDisconnectAll()
rm(con, q, rs1)

# Count (n) hits for each species (columns) in each quadrat (rows)
d <- (d %>% select(quadrat_id, species_name)
      %>% group_by(quadrat_id, species_name)
      %>% summarise(n=n())
      %>% ungroup()
      %>% pivot_wider(names_from = species_name, values_from = n))

# At this point, d has the number of hits for each survey and species.
# Replace anything numeric with 1, and any NA with 0
d <- (d %>% select(-quadrat_id) %>% replace(., !is.na(.), 1)
      %>% replace(., is.na(.), 0)) # Replace NAs with 0)
n_sites <- d %>% select(1) %>% count() %>% unlist()

# write_csv(d, "hits.csv", col_names = TRUE)

# What follows thanks to Brian Shalloway: 
# https://www.bryanshalloway.com/2020/06/03/tidy-2-way-column-combinations/#fn4
# Manipulating the data into contingency tables for species pairs (dyads)
# and getting the fisher.test statisics using map not for-loops.
df_lists <- d %>%
  summarise_all(list) %>% 
  pivot_longer(cols = everything(), 
               names_to = "var", 
               values_to = "vector")

df_lists_comb <- expand(df_lists,
                        nesting(var, vector),
                        nesting(var2 = var, vector2 = vector))

is_event <- function(...){ # See map2_int below
  sum(..1 & ..2)
}

df_lists_comb_as <- df_lists_comb %>% 
  filter(var != var2) %>% 
  mutate(events = map2_int(.x = vector, .y = vector2, .f = is_event)) %>% 
  filter((events > 0) & events < length(df_lists$vector[[1]])) %>%
  select(-events)
rm(is_event)

# function to make the contingency tables.
xtab <- function(...){ # See map2 below
  t <- table(..1, ..2) 
}
# And map them into a convenient data structure.
df_tables <- df_lists_comb_as %>% 
  mutate(xtable = map2(vector, vector2, xtab))
# Clean up...
rm(df_lists, df_lists_comb, df_lists_comb_as, xtab)

# Make a graph and simplify it before doing the fisher test
df_edges <- df_tables %>% select(var, var2) %>% rename(from = var, to = var2)
g1 <- as_tbl_graph(graph.data.frame(d = df_edges, directed = FALSE))

# Add edge attributes to g1 edges
g1 <- g1 %>% activate("edges") %>%
  mutate(xtable = df_tables$xtable)

# Add Observed dyad probabilities 
g1 <- g1 %>% activate("edges") %>%
  mutate(p_obs = map_dbl(.x = xtable, .f = ~{unlist(.x[4])/sum(unlist(.x))}))

# Simplify the Tidygraph way
g1 <- g1 %>% activate("edges") %>% 
  filter(!edge_is_multiple()) %>%
  filter(!edge_is_loop())
# Clean up ...
# [1,1rm(df_edges, df_tables)

# Important function to SAFELY apply fisher.test, ensuring we get
# some return value for dyad.
sft <- safely(.f = fisher.test, otherwise = NULL, quiet = TRUE)

# Map fisher.test over dyads
g1 <- g1 %>% activate("edges") %>%
  mutate(f_test = map(xtable, sft))
rm(sft)

# Temporary data structures needed to extract fisher.test statistics
# from <hresult> class and transfer them to edges data where they belong.
x <- tibble(pval = rep(0, gsize(g1)))
x <- x %>% mutate(lor = pval)
df_t <- g1 %>% activate("edges") %>% as_tibble()

# Extract p.val and Odds Ratio from fisher.test <hresult>
# Would prefer to do using map.
for(i in 1:gsize(g1)){ 
  x$pval[i] <- (ifelse(is.null(df_t[["f_test"]][[i]][["result"]][["p.value"]]), 100, df_t[["f_test"]][[i]][["result"]][["p.value"]]))
  x$lor[i] <- (ifelse(is.null(df_t[["f_test"]][[i]][["result"]][["p.value"]]), 100, df_t[["f_test"]][[i]][["result"]][["estimate"]]))
}
# Clean up...
rm(i, df_t)

# Actually transfer the stats to the edge attributes
g1 <- g1 %>%
  activate("edges") %>%
  mutate(pval = x$pval) %>% 
  mutate(lor = log(x$lor)) # NOTE log
# Clean up ...  
rm(x)

### KEY STEP IN ANALYISIS. ########
# Select filters on pval and lor
# IMPORTANT NOTE SBM is told nothing about pval or lor. 
# They are used only to select dyads for the SBM to work on.
g1 <- g1 %>%
  activate("edges") %>%
  filter(pval < 0.05) %>% 
  filter(is.finite(lor)) %>%
  mutate(weight = abs(lor)) %>%
  mutate(sgn = ifelse(lor > 0, "associative", "dissociative"))
# Remove isolated nodes
g1 <- g1 %>% activate("nodes") %>% filter(degree(g1) > 0)

# Check on the lor histogram
edges <- g1 %>% activate(edges) %>% as_tibble()
plt1 <- ggplot(edges, aes(lor))  +
  geom_histogram(aes(y = ..density..), binwidth = 0.25, colour = "black") +
  geom_vline(xintercept = 0, colour = "red") +
  xlim(-5, 10) +
  ylim(0.0, 0.4) +
  labs(title ="", x = "log(odds ratio)")
plot(plt1)

# Obtain the adjacency matrix ...
M <- as_adj(g1, type = "both", sparse = F)
# and covariate matrix if needed
P <- as_adj(g1, attr = "p_obs", type = "both", sparse = F)

# And finally, build the model!
# No covariate
# the_model <- estimateSimpleSBM(M, 'bernoulli', estimOptions = list(plot = T )) #TRUE))
# With covariate
the_model <- estimateSimpleSBM(M, 'bernoulli', covariates = list(P), estimOptions = list(plot =  T)) #TRUE))

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
g1 <- g1 %>% activate("nodes") %>% mutate(latent_community = the_model$memberships)

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

# Save g1 here AFTER adding stuff from the_model
write_rds(g1, "Qg1.rds")

## write_rds(the_model, "Q_SBM.rds")

write_rds(the_model, "Q_SBM_cov_P.rds")
