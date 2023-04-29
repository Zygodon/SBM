# Started 2023-04-5
# Code to explore relationship between SBM groups, representative species and surveys.
library("RMySQL")
library(tidyverse)
library(igraph)
library(ggraph)
library(tidygraph)
library(RColorBrewer)
library(sbm)

# Functions
dbDisconnectAll <- function(){
  ile <- length(dbListConnections(MySQL())  )
  lapply( dbListConnections(MySQL()), function(x) dbDisconnect(x) )
  cat(sprintf("%s connection(s) closed.\n", ile))
}
GetSurveyData <-  function()
{
  # GET DATA FROM DB
  # Remote DB with password
  con <- dbConnect(MySQL(), 
                   user  = "guest",
                   password    = "guest",
                   dbname="meadows",
                   port = 3306,
                   host   = "sxouse.ddns.net")
  
  # SQL query to extract survey data and associated species  
  q <- sprintf('SELECT DISTINCT assembly_name, community, species_name
from surveys join quadrats on quadrats.survey_id = surveys.surveys_id
join records on quadrats.quadrats_id = records.quadrat_id
join species on species.species_id = records.species_id
where species.species_id != 4 and major_nvc_community like "MG%%" and quadrat_size = "2x2";') 
  
  # NOTE: this extract includes "MG5", i.e. some MG5 communities where 
  # the team have not decided
  # on a sub-group.
  
  rs1 = dbSendQuery(con, q)
  return(as_tibble(fetch(rs1, n=-1)))
  dbDisconnectAll()
}

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

#### MAIN ####
# GET DATA FROM DB
# Remote DB with password
con <- dbConnect(MySQL(), 
                 user  = "guest",
                 password    = "guest",
                 dbname="meadows",
                 port = 3306,
                 host   = "sxouse.ddns.net")

q <- sprintf('select survey_id, assembly_name, species.species_name from surveys
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
# Remove isolated nodes
g1 <- g1 %>% activate("nodes") %>% filter(degree(g1) > 0)

# Obtain the adjacency matrix ...
M <- as_adj(g1, type = "both", sparse = F)

# And finally, build the model!
the_model <- estimateSimpleSBM(M, 'bernoulli', estimOptions = list(plot = F )) #TRUE))
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


##### GROUP 3 SUMMARY
hits <- gather(d) %>% group_by(key) %>% summarise(count = sum(value)) %>% rename(species = key)
# count: how many meadows the species was found in.
g1 <- g1 %>% activate(nodes) %>% left_join(hits, join_by(name == species))
# Graph for the summary
ggrp3 <- g1 %>% activate(nodes) %>% filter(group == 3)

ggrp3 %>% ggraph(layout = "kk") +
  scale_edge_color_brewer(palette="Dark2") +
  scale_edge_width(range = c(1, 2)) +
  geom_edge_link(aes(colour = sgn, width = weight),alpha = 0.6) + 
  geom_node_point(aes(size = count), pch = 21, fill = 'navajowhite1') +
  scale_size(range = c(5, 15)) +
  geom_node_text(aes(label = name), colour = 'black', repel = T) + 
  # expand pads the x axis so the labels fit onto the canvas.
  scale_x_continuous(expand = expansion(mult = 0.2)) +
  scale_y_continuous(expand = expansion(mult = 0.1)) +
  ggtitle('Group 3') + 
  theme_graph()

# Import data for including sites
survey_data <- GetSurveyData()
survey_data <- rename(survey_data, survey = assembly_name, species = species_name)
group3 <- ggrp3 %>% activate(nodes) %>% as_tibble

# Remove the species that are not represented in the SBM model
# i.e. not group1 species
edge_list <- survey_data %>% 
  filter(species %in% group3$name) %>% 
  filter(!is.na(community)) %>%
  select(-community)

# bp3: bipartite for group 3
bp3 <- graph.data.frame(edge_list, directed = F)
V(bp3)$type <- V(bp3)$name %in% edge_list$species #the second column of edges is TRUE type
bp3 <- as_tbl_graph(bp3)
bp3 <- bp3 %>% activate(nodes) %>% mutate(kind = ifelse(type, "species", "survey"))

# Group 3 membership weight for each survey.
# The sum of the lor for the group1 species that are represented in a survey
# surveys is a place holder for the results
surveys <- bp3 %>% activate(nodes) %>% filter(type == FALSE) %>% as_tibble()
for (i in seq_along(surveys$name)) {
  # get the plants associated with this survey
  survey <- bp3 %>%
    convert(to_local_neighborhood,
            node = which(.N()$name == surveys$name[i]),
            order = 1,
            mode = "all") %>% as_tibble()
  # filter the group1 graph to just these plants'
  # then get the summed lor from the edges
  grp3_weight <- ggrp3 %>% activate(nodes) %>%
    filter(name %in% survey$name[which(survey$type == TRUE)]) %>%
    activate(edges) %>% 
    as_tibble() %>% 
    select(lor) %>% 
    summarise(sum = sum(lor))
  surveys$grp3_weight[i] <- unlist(grp3_weight)
}

surveys <- surveys %>% select(name, grp3_weight)
# Transfer the weights to the bipartite graph nodes.
bp3 <- bp3 %>% activate(nodes) %>% left_join(surveys, join_by(name))

bp3 %>% ggraph(layout = "stress") +
  geom_edge_link(colour = "grey80") +
  scale_colour_brewer(palette = "Dark2") +
  geom_node_point(aes(shape = kind, colour = kind, size = ifelse(kind == "survey", grp3_weight, 2))) +
  # geom_node_text(aes(label = ifelse(kind == "survey", name, "")), colour = 'black', repel = T) +
  ggtitle('Group 3') +
  theme_graph()
