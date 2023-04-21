# 2023-04-20 SBM.R. General Bernouilli SBM. 
# Modify DB extract;
# Modify filter by p_val or lor as required
# Change lor filter to obtain associative clustering, 
# dissociative clustering or both.

# This program makes extensive use of the sbm package:
# Julien Chiquet [aut, cre] (<https://orcid.org/0000-0002-3629-3429>), 
# Sophie Donnet [aut] (<https://orcid.org/0000-0003-4370-7316>), 
# großBM team [ctb], Pierre Barbillon [aut] 
# (<https://orcid.org/0000-0002-7766-7693>)

library("RMySQL")
library(tidyverse)
library(igraph)
library(tidygraph)
library(dplyr)
library(ggraph)
library(sbm)

# Functions
dbDisconnectAll <- function(){
  ile <- length(dbListConnections(MySQL())  )
  lapply( dbListConnections(MySQL()), function(x) dbDisconnect(x) )
  cat(sprintf("%s connection(s) closed.\n", ile))
}

#### MAIN ####
# GET DATA FROM DB
# Remote DB with password
con <- dbConnect(MySQL(), 
                 user  = "guest",
                 password    = "guest",
                 dbname="meadows",
                 port = 3306,
                 host   = "sxouse.ddns.net")

q <- sprintf('select survey_id, assembly_name, quadrat_count, community, quadrat_id, quadrat_size, visit_date, records_id, species.species_id,
    species.species_name from surveys
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

# Count (n) hits for each species (columns) in each survey (rows)
d <- (d %>% select(survey_id, species_name)
      %>% group_by(survey_id, species_name)
      %>% summarise(n=n())
      %>% ungroup()
      %>% pivot_wider(names_from = species_name, values_from = n))

# At this point, d has the number of hits for each survey and species.
# Replace anything numeric with 1, and any NA with 0
d <- (d %>% select(-survey_id) %>% replace(., !is.na(.), 1)
      %>% replace(., is.na(.), 0)) # Replace NAs with 0)

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

df_lists_comb_as <- df_lists_comb %>% 
  filter(var != var2) %>% 
  arrange(var, var2) %>% 
  mutate(vars = paste0(var, ".", var2)) %>% 
  select(contains("var"), everything())

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

# Simplify the Tidygraph way
g1 <- g1 %>% activate("edges") %>% 
  filter(!edge_is_multiple()) %>%
  filter(!edge_is_loop())
# Clean up ...
rm(df_edges, df_tables)

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

##### KEY STEP IN ANALYISIS. #####
# Select filters on pval and lor
# IMPORTANT NOTE SBM is told nothing about pval or lor. 
# They are used only to select dyads for the SBM to work on.
g1 <- g1 %>%
  activate("edges") %>%
  filter(pval < 0.05) %>% 
  filter(is.finite(lor)) %>%
  mutate(weight = abs(lor)) %>%
  mutate(sgn = ifelse(lor > 0, "associative", "dissociative"))

# Check on the lor histogram
df <- g1 %>% activate("edges") %>% as_tibble()
plot(ggplot(df, aes(lor))  +
       geom_histogram(aes(y = ..density..), binwidth = 0.25, colour = "black") +
       stat_function(fun = dnorm, args = list(mean = mean(df$lor), sd = sd(df$lor)), colour = "green") +
       geom_vline(xintercept = 0, colour = "red") +
       xlim(-6, 8) +
       ylim(0.0, 0.6) +
       labs(title ="", x = "log(odds ratio)"))
rm(df)

# Obtain the adjacency matrix ...
M <- as_adj(g1, type = "both", sparse = F)

# And finally, build the model!
the_model <- estimateSimpleSBM(M, 'bernoulli', estimOptions = list(plot = T )) #TRUE))
# NOTE: ICL Integrated Completed Likelihood: figure of merit for the multiple
# the_model explored during estimation
## NOTE to locate the model: model <- which.max(the_model$ICL) # Select the model to use
rm(M) # Clean up...

# Print C matrix. Note that it can be recovered from the_model$connectParam
print(as_tibble(the_model$connectParam))

# Add group memberships to the node properties
g1 <- g1 %>% activate("nodes") %>% mutate(group = the_model$memberships)

# Generate block plot...
# Add EDGE group membership, NA for edges between blocks.
# Group assigned only to edges between diads within a block.
g1 <- g1 %>%
  activate(nodes) %>%
  morph(to_split, group) %>%
  activate(edges) %>%
  mutate(edge_group = as.character(.N()$group[1])) %>%
  unmorph()

# Matrix plot. Points are EDGES. Axes are NODES, i.e plants.
# Edges link the plant represented on the vertical axis to the
# corresponding plant on the horizontal axis.
plot(ggraph(
  g1, 'matrix', sort.by = group) +
    geom_edge_point(aes(colour = edge_group), mirror = TRUE, edge_size = 3) +
    scale_y_reverse() +
    coord_fixed() +
    labs(edge_colour = 'group') +
    ggtitle("SBM"))

##### MATRIX PLOT EDGE SIGN
plot(ggraph(
  g1, 'matrix', sort.by = group) +
    scale_edge_colour_manual(values = c("black", "red")) +
    geom_edge_point(aes(colour = sgn), mirror = TRUE, edge_size = 1) +
    scale_y_reverse() +
    coord_fixed() +
    ggtitle("Block matrix with edge sign"))

##### GROUP MEMBERSHIPS
group_memberships <- g1 %>% activate("nodes") %>% as_tibble()


# TO DO: Goodness of fit