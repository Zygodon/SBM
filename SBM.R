# Started 2023-04-5
# 2023-04-29 Conceptualising to Latent Community (LC)
# Code to explore relationship between SBM latent_communitys, representative species and surveys.
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
  # on a sub-latent_community.
  
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

# Add LC memberships to the node properties
g1 <- g1 %>% activate("nodes") %>% mutate(latent_community = the_model$memberships)

# Generate block plot...
# Add EDGE latent_community membership, NA for edges between blocks.
# latent_community assigned only to edges between diads within a block.
g1 <- g1 %>%
  activate(nodes) %>%
  morph(to_split, latent_community) %>%
  activate(edges) %>%
  mutate(edge_latent_community = as.character(.N()$latent_community[1])) %>%
  unmorph()

# Matrix plot. Points are EDGES. Axes are NODES, i.e plants.
# Edges link the plant represented on the vertical axis to the
# corresponding plant on the horizontal axis.
plot(ggraph(
  g1, 'matrix', sort.by = latent_community) +
    geom_edge_point(aes(colour = edge_latent_community), mirror = TRUE, edge_size = 3) +
    scale_y_reverse() +
    coord_fixed() +
    labs(edge_colour = 'latent_community') +
    ggtitle("SBM"))


########### LATENT COMMUNITY SUMMARY ###############
hits <- gather(d) %>% group_by(key) %>% summarise(count = sum(value)) %>% rename(species = key)
# count: how many sites the species was found in.
g1 <- g1 %>% activate(nodes) %>% left_join(hits, join_by(name == species))

# Import data for including sites
survey_data <- GetSurveyData()
survey_data <- rename(survey_data, survey = assembly_name, species = species_name)

# Somewhere to keep the survey-wise community expressions
survey_expressions <- survey_data %>% distinct(survey)

# Somewhere to keep the latent community max, min and range
lc_stats <- tibble(lc = 1:8, lc_max = 0, lc_min = 0, lc_range = 0)

for (lc in 1:8) {
  # Graph for the summary
  glc1 <- g1 %>% activate(edges) %>% 
    filter(edge_latent_community == lc)
  isolates <- which(degree(glc1)==0)
  glc1 <- as_tbl_graph(delete.vertices(glc1, isolates))
  # Get the stats ...
  # Latent community min, max and range
  associative_degree <- glc1 %>% activate(edges) %>% 
    filter(lor > 0) %>% 
    degree() %>% 
    as_tibble()
  lc_stats$lc_max[lc] <- associative_degree %>% sum()
  
  dissociative_degree <- glc1 %>% activate(edges) %>% 
    filter(lor < 0) %>% 
    degree() %>% 
    as_tibble()
  lc1_min <- dissociative_degree %>% sum()
  lc_stats$lc_min[lc] <- -(lc1_min)
  lc_stats$lc_range[lc] <- lc_stats$lc_max[lc] - lc_stats$lc_min[lc]
  
  # Plot the latent community
  plot(glc1 %>% ggraph(layout = "kk") +
         scale_edge_color_brewer(palette="Dark2") +
         scale_edge_width(range = c(1, 2)) +
         geom_edge_link(aes(colour = sgn, width = weight),alpha = 0.75) + 
         geom_node_point(aes(size = count), pch = 21, fill = 'navajowhite1') +
         scale_size(range = c(5, 15)) +
         geom_node_text(aes(label = name), colour = 'black', repel = T) + 
         # expand pads the x axis so the labels fit onto the canvas.
         scale_x_continuous(expand = expansion(mult = 0.2)) +
         scale_y_continuous(expand = expansion(mult = 0.1)) +
         ggtitle(paste("Latent Community", lc, sep="_")) + 
         theme_graph())
  
  ## Get lc expressed by site
  latent_community <- glc1 %>% activate(nodes) %>% as_tibble
  # Remove the species that are not in latent_community dyads
  edge_list <- survey_data %>% 
    filter(species %in% latent_community$name) %>%  # name is species name
    filter(!is.na(community)) %>% # Community here is assessed NVC
    select(-community)
  # bp1: bipartite for latent_community 1
  bp1 <- graph.data.frame(edge_list, directed = F)
  V(bp1)$type <- V(bp1)$name %in% edge_list$species #the second column of edges is TRUE type
  bp1 <- as_tbl_graph(bp1)
  bp1 <- bp1 %>% activate(nodes) %>% mutate(kind = ifelse(type, "species", "survey"))
  # latent_community expression for each survey.
  # The range of the glc1 subgraph for the community, normalised by
  # the range of lc1
  # surveys is a place holder for the results
  surveys <- bp1 %>% activate(nodes) %>% filter(type == FALSE) %>% as_tibble()
  surveys <- surveys %>% mutate(lc_min = NA) %>%
    mutate(lc_max = NA)
  
  for (i in seq_along(surveys$name)) {
    # get the plants associated with this survey
    survey <- bp1 %>%
      convert(to_local_neighborhood,
              node = which(.N()$name == surveys$name[i]),
              order = 1,
              mode = "all") %>% as_tibble()
    # filter the latent_community graph to just these plants'
    # then get the dissociative and associative degrees
    sg <- glc1 %>% activate(nodes) %>%
      filter(name %in% survey$name[which(survey$type == TRUE)])
    associative_degree <- sg %>% 
      activate(edges) %>% 
      filter(lor > 0) %>% 
      degree() %>% 
      as_tibble()
    surveys$lc_max[i] <- associative_degree %>% sum()
    dissociative_degree <- sg %>% 
      activate(edges) %>% 
      filter(lor < 0) %>% 
      degree() %>% 
      as_tibble()
    surveys$lc_min[i] <- dissociative_degree %>% sum()*-1
  }
  # For each survey and the current latent community, calculate the lc expression
  surveys <- surveys %>% mutate(lc_express = 100*((lc_max - lc_min)/lc_stats$lc_range[lc])) # percent
  surveys <- surveys %>% select(name, lc_min, lc_max, lc_express)
  #Save the survey expressions for this latent community in survey_expressions
  survey_expressions <- survey_expressions %>% 
    left_join(surveys, join_by(survey==name)) %>%
    select(-lc_min, -lc_max)
  # Transfer the latent community expressions to the bipartite graph nodes.
  bp1 <- bp1 %>% activate(nodes) %>% left_join(surveys, join_by(name))
  # Draw the bipartite graph
  plot2 <- bp1 %>% ggraph(layout = "stress") +
    geom_edge_link(colour = "grey80") +
    scale_colour_brewer(palette = "Dark2") +
    geom_node_point(aes(colour = kind, shape = kind,  
                        size = ifelse(kind == "survey", lc_express, 2))) +
    geom_node_text(aes(label = ifelse(kind == "species", name, "")), colour = 'black', repel = T, size=3) + 
    ggtitle(paste("Latent Community", lc, sep = "_")) +
    theme_graph()
  plot(plot2 +
         guides(
           size = guide_legend(title = "Community expression %
                          ", override.aes=list(shape = 17,colour = "#d95f02")),
           shape = guide_legend(title="", override.aes=list(size = 4)),
           colour = guide_legend("")))
} # End for lc in 1:8


##########  POLAR PLOT ###############
survey_expressions <- survey_expressions %>% 
  rename(lc1=2, lc2=3, lc3=4, lc4=5, lc5=6, lc6=7, lc7=8, lc8=9) %>%
  replace(is.na(.), 0) %>%
  arrange(survey) # IMPORTANT

# Polar plot labels based on work by Yan Holz
# https://r-graph-gallery.com/296-add-labels-to-circular-barplot.html?utm_content=cmp-true
survey_columns <- survey_expressions %>% pivot_longer(cols = !survey, names_to = "LC", values_to = "xp")

# Sum of LC expression for each community needed for label y-values
label_y <- survey_columns %>% select(-LC) %>% group_by(survey) %>% summarise(y = sum(xp))
y_max <- ceiling(max(label_y$y))

survey_labels <- survey_columns %>% 
  select(survey) %>% 
  distinct() %>% 
  mutate(id = seq_along(survey)) %>% 
  # Subtract 0.5 because the letter must have the angle of the center of the bars, 
  # not extreme right(1) or extreme left (0)
  mutate(angle =  90 - 360 * (id-0.5) /length(survey)) %>%  
  # calculate the alignment of labels: right or left
  # If I am on the left part of the plot, my labels have currently an angle < -90
  mutate(hjust = ifelse( angle < -90, 1, 0)) %>%
  # Flip angles BY 180 degrees to make them readable
  mutate(angle=ifelse(angle < -90, angle+180, angle))

p <- ggplot(survey_columns) + 
  geom_col(aes(x = survey, y = xp, fill = LC)) +
  scale_fill_brewer(palette = "Accent") +
  coord_polar(start = 0) +
  ylim(-50,y_max) +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") # Adjust the margin so labels are not truncated!
  ) 
# Add the labels, using the label_data dataframe that we have created before
p + geom_text(data = survey_labels, aes(x=id, y=200, label=survey, hjust=hjust), 
              color="black", alpha=0.7, size=3, angle=survey_labels$angle, inherit.aes = FALSE ) +
  guides(fill = guide_legend("Latent Community"))



