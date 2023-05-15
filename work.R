# Started 2023-04-5
# 2023-04-29 Conceptualising to Latent Community (LC)
# Code to explore relationship between SBM latent_communitys, representative species and surveys.

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

NewName <- function(...){paste("lc", lc, sep = "")}


### DATA WRANGLING #####################
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

##### KEY STEP IN ANALYISIS. ########
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

# select which model to use (default is the "best")
# the_model$setModel(6)

# Print C matrix. Note that it can be recovered from the_model$connectParam
print(as_tibble(the_model$connectParam))

# Add LC memberships to the node properties
g1 <- g1 %>% activate("nodes") %>% mutate(latent_community = the_model$memberships)

# Generate block plot...
# Add EDGE latent_community membership, NA for edges between blocks.
# latent_community assigned only to edges between dyads within a block.
g1 <- g1 %>%
  activate(nodes) %>%
  morph(to_split, latent_community) %>%
  activate(edges) %>%
  ## mutate(edge_latent_community = as.character(.N()$latent_community[1])) %>%
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


########### LATENT COMMUNITY SUMMARY ###############
# Needs the_model, g1 and possibly d
# Import data for including sites in the analysis
survey_data <- GetSurveyData() %>%
  rename(site = assembly_name, species = species_name) %>%
  select(-community) # Don't need assessed NVC

n_sites <- survey_data %>% select(site) %>% distinct() %>% count() %>% as.integer()

hits <- gather(d) %>% 
  group_by(key) %>% 
  summarise(count = sum(value)) %>% 
  mutate(frequency = 100*count/n_sites) %>%
  rename(species = key)
# count: how many sites the species was found in.

# In g1, add species name to nodes
g1 <- g1 %>% activate(nodes) %>% left_join(hits, join_by(name == species))
# and to edges from - to
g1 <- g1 %>% activate(edges) %>% mutate(A = .N()$name[from], B = .N()$name[to])

### Make lc_stats with lc_range for calculating lc_expression by site
# lc_stats just has lc and lc_range but could add other stats later
lc_stats <- tibble(lc = seq(1:the_model$nbBlocks))

lc_range <- function(.x) {
  szlc <- g1 %>% activate(edges) %>%
    filter(!is.na(edge_latent_community)) %>%
    filter(edge_latent_community == .x) %>%
    as_tibble() %>%
    count()
  return(data.frame(lc = .x, range = szlc$n))
}
# map_df does an implies mutate
lc_stats <- map_df(.x = lc_stats$lc, 
                   .f = lc_range) # So now lc_stats is ready to be used when I need to calculate site expressions.

### Wrangling to calculate LC expression by site
sv <- rep(survey_data %>% distinct(site), the_model$nbBlocks) %>% unlist() %>% unname(sv) %>% sort()
sv <- tibble(site = sv, lc = rep(seq(1:the_model$nbBlocks), length(sv)/the_model$nbBlocks))

# extract the site and LC pairs from sv as separate vectors
sites <- sv %>% pull(site) %>% as.character
lc <- sv %>% pull(lc)

exp_at_site <- function(.x, .y){
  spp_to_choose_from <- survey_data %>%
        filter(site == .x) %>%
        pull(species)
  
  sg <-  g1 %>% activate(edges) %>% # the sub-graph of g1 with dyads drawn from a site/LC combination
        filter(edge_latent_community == .y) %>%
        filter((A %in% spp_to_choose_from) &  (B %in% spp_to_choose_from))
  rng <-sg %>% activate(edges) %>% as_tibble() %>% summarise(n=n())
  expression_lc_at_site <- 100*rng$n/lc_stats$range[.y]
  return(ifelse(is.na(expression_lc_at_site), 0, expression_lc_at_site))
}

site_xp <- sv %>% mutate(xp = map2_dbl(.x = sites, .y = lc, .f = exp_at_site))
rm(sv, n_sites, lc)

### LC mesoscopic plot ################
meso_plot_list <- map(.x = lc_stats$lc,
                 .f = ~{
                          glc1 <- g1 %>% activate(edges) %>%
                                filter(edge_latent_community == .x)
                          isolates <- which(degree(glc1)==0) # Not Tidygraph
                          glc1 <- as_tbl_graph(delete.vertices(glc1, isolates))
                          plot(glc1 %>% ggraph(layout = "kk") +
                                 scale_edge_colour_brewer(palette="Dark2", guide = guide_legend("Sign")) +
                                 geom_edge_link(aes(colour = sgn),width = 1, alpha = 1) +
                                 geom_node_point(aes(size = frequency), pch = 21, fill = 'navajowhite1') +
                                 scale_size(name="Frequency in data", range = c(5, 15)) +
                                 geom_node_text(aes(label = name), colour = 'black', repel = T) +
                                 # expand pads the x axis so the labels fit onto the canvas.
                                 scale_x_continuous(expand = expansion(mult = 0.2)) +
                                 scale_y_continuous(expand = expansion(mult = 0.1)) +
                                 ggtitle(paste("Latent Community", .x, sep=" ")) +
                                 theme_graph())
                        })

### site-species bipolar plots #################################

# Strategy: make a bipartite graph of all sites and species; get the sub-graph
# for each plot. Start with survey_data

# Thank you Laszlo Gadar for how to make bipartite graph
# https://rpubs.com/lgadar/load-bipartite-graph
bp <- graph.data.frame(survey_data, directed = F)
V(bp)$type <- V(bp)$name %in% survey_data[,2]$species #the second column of edges is TRUE type
bp <- as_tbl_graph(bp)
bp <- bp %>% activate(nodes) %>% mutate(kind = ifelse(type, "species", "site"))

bipolar_plot_list <- map(.x = lc_stats$lc, .f = ~{
    this_lc <- .x
    spp <- g1 %>% activate(nodes) %>% filter(latent_community == .x) %>% select(name) %>% as_tibble()
    sg <- bp %>% activate(nodes) %>% filter(kind == "site" | name %in% spp$name)
    # remove isolated nodes
    isolates <- which(degree(sg)==0) # Not Tidygraph
    sg <- as_tbl_graph(delete.vertices(sg, isolates))
    # Need to get the lc expressions of **this** lc over all sites - to make symbol size
    lcxp <- map_df(.x = this_lc, .f = ~{
      site_xp %>% filter(lc == this_lc) %>% select(site, xp)
    })
    sg <- sg %>% activate(nodes) %>% left_join(lcxp, join_by(name == site))
    sg <- sg %>% activate(nodes) %>% mutate(cb = centrality_betweenness(
        weights = NULL,
        directed = FALSE,
        cutoff = -1,
        normalized = FALSE))

    p2 <- sg %>% ggraph(layout = "stress") +
    geom_edge_link(colour = "grey80") +
    scale_colour_brewer(palette = "Dark2") +
    # geom_node_point(aes(colour = fct_rev(kind), shape = fct_rev(kind), size = ifelse(kind == "site", xp, cb))) +
    geom_node_point(aes(colour = fct_rev(kind), shape = fct_rev(kind), size = ifelse(kind == "site", xp, 3))) +
    # geom_node_text(aes(label = ifelse(kind == "species", name, "")), colour = 'black', repel = T, size=3) +
    ggtitle(paste("Latent Community", .x, sep = " ")) +
    theme_graph()
    plot(p2 +
       guides(
         size = guide_legend(title = "Community expression %", override.aes=list(shape = 17,colour = "#d95f02")),
         shape = guide_legend(title="", override.aes=list(size = 4)),
         colour = guide_legend("")))
})

# 
# ##########  POLAR PLOT ###############

# Polar plot labels based on work by Yan Holz
# https://r-graph-gallery.com/296-add-labels-to-circular-barplot.html?utm_content=cmp-true

# Sum of LC expression for each community needed for label y-values
label_y <- site_xp %>% select(-lc) %>% group_by(site) %>% summarise(y = sum(xp))
y_max <- ceiling(max(label_y$y))

site_labels <- site_xp %>%
  select(site) %>%
  distinct() %>%
  mutate(id = seq_along(site)) %>%
  # Subtract 0.5 because the letter must have the angle of the center of the bars,
  # not extreme right(1) or extreme left (0)
  mutate(angle =  90 - 360 * (id-0.5) /length(site)) %>%
  # calculate the alignment of labels: right or left
  # If I am on the left part of the plot, my labels have currently an angle < -90
  mutate(hjust = ifelse( angle < -90, 1, 0)) %>%
  # Flip angles BY 180 degrees to make them readable
  mutate(angle=ifelse(angle < -90, angle+180, angle))

site_xp <- site_xp %>% arrange(site) # Just to be sure
p <- ggplot(site_xp) +
  geom_col(aes(x = site, y = xp, fill = as.factor(lc))) +
  scale_fill_brewer(palette = "Accent") +
  coord_polar(start = 0) +
  ylim(-50,y_max) +
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_text("Latent community expression") # ?not work
  )
# # Add the site labels.
plot(p + geom_text(data = site_labels, aes(x=id, y=ceiling(0.8*y_max), label=site, hjust=hjust),
                   color="black", alpha=0.6, size=3, angle=site_labels$angle, inherit.aes = FALSE ) +
       guides(fill = guide_legend("Latent Community")) +
       labs(title = "Site expressions of latent communities"))

# Facility to record site_columns
write.csv(site_columns, "site latent communities.csv")

# Extract dyads
nodes <- g1 %>% activate(nodes) %>% as_tibble()

dyads <- g1 %>% activate(edges) %>%
  as_tibble() %>%
  filter(!is.na(edge_latent_community)) %>%
  select(from, to, sgn, edge_latent_community)

dyads <- dyads %>%
  mutate(A = nodes$name[from]) %>%
  mutate(B = nodes$name[to])

dyads <- dyads %>% select(A, B, sgn, edge_latent_community) %>%
  rename(sign = sgn, community = edge_latent_community)

# write.csv(lc_stats, "lc_stats.csv")
# write.csv(dyads, "dyads.csv")


