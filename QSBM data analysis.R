
# Started 2023-05-16
# Code to explore relationship between SBM latent_communities, representative species and quadrats.
# 2023-06-7
# sort out: LC primary & LC secondary dyads; LC prototypes, LC associates and LC contributors.

### libraries #########################
library("RMySQL")
library(tidyverse)
library(igraph)
library(ggraph)
library(tidygraph)
library(RColorBrewer)
library(sbm)

### Functions #######################
dbDisconnectAll <- function(){
  ile <- length(dbListConnections(MySQL())  )
  lapply( dbListConnections(MySQL()), function(x) dbDisconnect(x) )
  cat(sprintf("%s connection(s) closed.\n", ile))
}

GetQuadratData <-  function()
{
  # GET DATA FROM DB
  # Remote DB with password
  con <- dbConnect(MySQL(), 
                   user  = "guest",
                   password    = "guest",
                   dbname="meadows",
                   port = 3306,
                   host   = "sxouse.ddns.net")
 
  # SQL query to extract quadrat data and associated species  
  q <- sprintf('SELECT DISTINCT quadrat_id, species_name
  from surveys join quadrats on quadrats.survey_id = surveys.surveys_id
  join records on quadrats.quadrats_id = records.quadrat_id
  join species on species.species_id = records.species_id
  where species.species_id != 4 and major_nvc_community like "MG%%" and quadrat_size = "2x2";') 
  
  rs1 = dbSendQuery(con, q)
  return(as_tibble(fetch(rs1, n=-1)))
  dbDisconnectAll()
}

### RECOVER the_model, d & g1 #################
# the_model <-  read_rds("Q_SBM.rds")
the_model <-  read_rds("Q_SBM_cov_P.rds")
pm <- the_model$connectParam$mean %>% as_tibble()
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

# Rebuild d
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

### ADD COUNT AND FREQUENCY DATA TO g1 ###############
# Needs the_model & g1
# Import quadrat data
quadrat_data <- GetQuadratData() %>%
  rename(species = species_name)

nq <- quadrat_data %>% select(quadrat_id) %>% distinct() %>% count() %>% as.integer()

hits <- gather(d) %>% 
  group_by(key) %>% 
  summarise(count = sum(value)) %>% 
  mutate(frequency = 100*count/nq) %>%
  rename(species = key)
# count: how many quadrats the species was found in.

# In g1, add count and frequency to nodes
g1 <- g1 %>% activate(nodes) %>% left_join(hits, join_by(name == species))
# and species names to edges from - to
g1 <- g1 %>% activate(edges) %>% mutate(A = .N()$name[from], B = .N()$name[to])

### PROTOTYPES AND ASSOCIATES ###############

# Add LC to the dyad ends
g1 <- g1 |> activate(edges) |> 
          mutate(LC_A = the_model$memberships[from]) |>
          mutate(LC_B = the_model$memberships[to])

prototypes <- g1 |> 
                activate(edges) |>
                filter(LC_A == LC_B) |>
                # The filter removes the edges but not the associated nodes
                activate(nodes) |>
                filter(!node_is_isolated())

associates <- g1 |> 
  activate(edges) |>
  filter(LC_A != LC_B) # associates is connected; no isolated nodes

items <- seq(1:the_model$nbBlocks)
lc_prototypes_list <- map(.x = items, .f = ~{
  prototypes |> activate(nodes) |> filter(latent_community == .x)
}) 

lc_associates_list <- map(.x = items, .f = ~{
  associates |> activate(nodes) |> filter(latent_community != .x) 
})

### PROTOTYPE SPECIES LISTS ##################
prototypes_list <- map(.x = items, .f = ~{
  return(data.frame(lc_prototypes_list[[.x]] %>% activate(nodes) |> select(name)))
})    

### LC MESOSCOPIC PLOTS ################
meso_plot_list <- map(.x = items, .f = ~{
  lc <- lc_prototypes_list[[.x]] %>% 
      activate(nodes) %>% 
      mutate(links = degree(lc_prototypes_list[[.x]]))
  if(is.connected(lc)){
    plot(lc %>% ggraph(layout = "centrality", cent = frequency) +
           scale_edge_colour_manual(values = c("grey80", "firebrick3"), guide = guide_legend("Sign")) +
           geom_edge_link(aes(colour = sgn),width = 1, alpha = 1) +
           # geom_node_point(aes(size = frequency), pch = 21, fill = 'navajowhite1') +
           geom_node_point(aes(size = frequency, fill = links), pch = 21) +
           scale_size(name="frequency", range = c(5, 15)) +
           geom_node_text(aes(label = name), colour = 'black', repel = T) +
           # expand pads the x axis so the labels fit onto the canvas.
           scale_x_continuous(expand = expansion(mult = 0.2)) +
           scale_y_continuous(expand = expansion(mult = 0.1)) +
           ggtitle(paste("Latent Community", .x, sep=" ")) +
           # facet_edges(~sgn) +
           theme_graph())
  }else{
    plot(lc %>% ggraph(layout = "stress") +
           scale_edge_colour_manual(values = c("grey80", "firebrick3"), guide = guide_legend("Sign")) +
           geom_edge_link(aes(colour = sgn),width = 1, alpha = 1) +
           geom_node_point(aes(size = frequency, fill = links), pch = 21) +
           scale_size(name="frequency", range = c(5, 15)) +
           geom_node_text(aes(label = name), colour = 'black', repel = T) +
           # expand pads the x axis so the labels fit onto the canvas.
           scale_x_continuous(expand = expansion(mult = 0.2)) +
           scale_y_continuous(expand = expansion(mult = 0.1)) +
           ggtitle(paste("Latent Community", .x, sep=" ")) +
           # facet_edges(~sgn) +
           theme_graph())
  }
})
  
### LC EXPRESSION BY QUADRAT #############
# Required for bipartite plots
# Make lc_stats with lc_range for calculating lc_expression by quadrat
# lc_stats just has lc and lc_range but could add other stats later
lc_stats <- tibble(lc = items)

# lc_range: count the number of  edges in prototypes graph
lc_range <- function(.x) {
  return(data.frame(lc = .x, range = gsize(lc_prototypes_list[[.x]])))
}
# map_df does an implied mutate
lc_stats <- map_df(.x = lc_stats$lc,
                   .f = lc_range) # So now lc_stats is ready to be used when I need to calculate site expressions.

### Wrangling to calculate LC expression by quadrat
qx <- rep(quadrat_data %>% distinct(quadrat_id), the_model$nbBlocks) %>% unlist() %>% unname(qx) %>% sort()
qx <- tibble(quadrat = qx, lc = rep(seq(1:the_model$nbBlocks), length(qx)/the_model$nbBlocks))

# extract the site and LC pairs from sv as separate vectors
quadrats <- qx %>% pull(quadrat)
lc <- qx %>% pull(lc)

exp_at_quadrat <- function(.x, .y){
  spp_to_choose_from <- quadrat_data %>%
    filter(quadrat_id == .x) %>%
    pull(species)

  sg <- g1 %>% activate(edges) %>% # the sub-graph of g1 with dyads drawn from a quadrat/LC combination
    filter(edge_latent_community == .y) %>%
    filter((A %in% spp_to_choose_from) &  (B %in% spp_to_choose_from))
  rng <-sg %>% activate(edges) %>% as_tibble() %>% summarise(n=n())
  expression_lc_at_quadrat <- 100*rng$n/lc_stats$range[.y]
  return(ifelse(is.na(expression_lc_at_quadrat), 0, expression_lc_at_quadrat))
}

quadrat_xp <- qx %>% mutate(xp = map2_dbl(.x = quadrats, .y = lc, .f = exp_at_quadrat))
rm(qx, nq, lc)

### QUADRAT-SPECIES BIPARTITE PLOTS #################################
# Strategy: make a bipartite graph of all quadrats and species; get the sub-graph
# for each plot. Start with quadrat_data

# Thank you Laszlo Gadar for how to make bipartite graph
# https://rpubs.com/lgadar/load-bipartite-graph
bp <- graph.data.frame(quadrat_data, directed = F)
V(bp)$type <- V(bp)$name %in% quadrat_data[,2]$species #the second column of edges is TRUE type
bp <- as_tbl_graph(bp)
bp <- bp %>% activate(nodes) %>% mutate(kind = ifelse(type, "species", "quadrat"))

bipartite_plot_list <- map(.x = lc_stats$lc, .f = ~{
  this_lc <- .x
  spp <- lc_prototypes_list[[.x]] %>% activate(nodes) %>% select(name) %>% as_tibble()
  sg <- bp %>% 
    activate(nodes) %>% 
    filter(kind == "quadrat" | name %in% spp$name) |>
    filter(!node_is_isolated())
  # Need to get the lc expressions of **this** lc over all quadrats - to make symbol size
  lcxp <- map_df(.x = this_lc, .f = ~{
    quadrat_xp %>% filter(lc == this_lc) %>% select(quadrat, xp)
  })
  lcxp <- lcxp %>% mutate(q = as.character(quadrat))
  sg <- sg %>% activate(nodes) %>% left_join(lcxp, join_by(name == q))
  sg <- sg %>% activate(nodes) %>% mutate(cb = centrality_betweenness(
    weights = NULL,
    directed = FALSE,
    cutoff = -1,
    normalized = FALSE))
  
  plot(p2 <- sg %>% ggraph(layout = "stress") +
         geom_edge_link(colour = "grey80") +
         scale_colour_brewer(palette = "Dark2") +
         # geom_node_point(aes(colour = fct_rev(kind), shape = fct_rev(kind), size = ifelse(kind == "quadrat", xp, cb))) +
         geom_node_point(aes(colour = fct_rev(kind), shape = fct_rev(kind), size = ifelse(kind == "quadrat", xp, 3))) +
         geom_node_text(aes(label = ifelse(kind == "species", name, "")), colour = 'black', repel = T, size=3) +
         ggtitle(paste("Latent Community", .x, sep = " ")) +
         guides(size = guide_legend(title = "Community expression %", override.aes=list(shape = 17,colour = "#d95f02")),
                shape = guide_legend(title="", override.aes=list(size = 4)),
                colour = guide_legend("")) +
         theme_graph())
})

### GENERAL QUADRAT LC-EXPRESSION POLAR PLOT ###############
quadrat_xp <- quadrat_xp %>% arrange(quadrat) # Just to be sure
# plot(p <- ggplot(quadrat_xp %>% filter(lc < 7)) +
plot(p <- ggplot(quadrat_xp) +
       geom_col(aes(x = quadrat, y = xp, fill = as.factor(lc))) +
       scale_fill_brewer(palette = "Accent") +
       coord_polar(start = 0) +
       ylim(-50,100) +
       theme(
         axis.text.x = element_blank(),
         axis.title.x = element_blank(),
         axis.title.y = element_text("Latent community expression"), # ?not work
         plot.margin = unit(rep(1,4), "cm") # Adjust the margin to make sure labels are not truncated!
       ))

### GENERAL SITE LC-EXPRESSION POLAR PLOT ###############
quadrat_xp <- quadrat_xp %>% arrange(quadrat) # Just to be sure
# Get site names with quadrats at site
con <- dbConnect(MySQL(), 
                 user  = "guest",
                 password    = "guest",
                 dbname="meadows",
                 port = 3306,
                 host   = "sxouse.ddns.net")

# SQL query to extract quadrat data and associated species  
q <- sprintf('select quadrats_id, assembly_name from quadrats join surveys on survey_id = surveys_id') 
rs1 = dbSendQuery(con, q)
sites <- as_tibble(fetch(rs1, n=-1))
dbDisconnectAll()
sites <- sites |> rename(quadrat = quadrats_id, site = assembly_name)
# df <- quadrat_data |> select(quadrat, site) |> left_join(sites)
df <- quadrat_xp|> left_join(sites)
rm(sites)
df <- df |> 
  select(-quadrat) |> 
  group_by(site, lc) |> 
  summarise(mean_xp = mean(xp)) |> 
  ungroup()

# Make the site labels.
# Sum of LC expression for each community needed for label y-values
label_y <- df %>% select(-lc) %>% group_by(site) %>% summarise(y = sum(mean_xp))
y_max <- ceiling(max(label_y$y)) #75

site_labels <- df %>%
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

polar_plot <- ggplot(df) +
       geom_col(aes(x = site, y = mean_xp, fill = as.factor(lc))) +
       scale_fill_brewer(palette = "Accent") +
       coord_polar(start = 0) +
       ylim(-50,100) +
       geom_text(data = site_labels, aes(x=id, y=ceiling(0.8*75), label=site, hjust=hjust),
            color="black", alpha=0.6, size=2, angle=site_labels$angle, inherit.aes = FALSE ) +
       guides(fill = guide_legend("Latent Community")) +
       labs(title = "Site expressions of latent communities") +
       theme(
         axis.text.x = element_blank(),
         axis.title.x = element_blank(),
         axis.title.y = element_text("Latent community expression"), # ?not work
         plot.margin = unit(rep(1,4), "cm") # Adjust the margin to make sure labels are not truncated!
       )

plot(polar_plot)

### {SITE} EXPRESSIONS OF LC POLAR PLOTS - NOT IMPLEMENTED FOR QUADRATS ################ 
# polar_plot_list <- map(.x = lc_stats$lc, .f = ~{
#   data <- site_xp %>% filter(lc == .x)
#   data_labels <- data %>%
#     select(site) %>%
#     mutate(id = seq_along(site)) %>%
#     # Subtract 0.5 because the letter must have the angle of the center of the bars,
#     # not extreme right(1) or extreme left (0)
#     mutate(angle =  90 - 360 * (id-0.5) /length(site)) %>%
#     # calculate the alignment of labels: right or left
#     # If I am on the left part of the plot, my labels have currently an angle < -90
#     mutate(hjust = ifelse( angle < -90, 1, 0)) %>%
#     # Flip angles BY 180 degrees to make them readable
#     mutate(angle=ifelse(angle < -90, angle+180, angle))
#   y_max <- ceiling(max(data$xp))
# 
#   p <- ggplot(data) +
#     geom_col(aes(x = site, y = xp), fill="steelblue3") +
#     coord_polar(start = 0) +
#     ylim(-ceiling(y_max/3),y_max) +
#     ylab(label="latent community expression, %") +
#     ggtitle(paste("Site expression of latent community", .x, sep = " ")) +
#     theme(
#       axis.text.x = element_blank(),
#       axis.title.x = element_blank())
#   # Add the survey labels.
#   plot(p + geom_text(data = data_labels, aes(x=id, y=ceiling(0.8*y_max), label=site, hjust=hjust),
#                 color="black", alpha=0.7, size=3, angle=data_labels$angle, inherit.aes = FALSE ))
# })

### SAVE DATA STRUCTURES (LISTS) #########
write_rds(lc_prototypes_list, "lc_prototype_species.rds")
write_rds(lc_associates_list, "lc_associate_species.rds")
write_rds(meso_plot_list, "mesoscopic_plots.rds")
write_rds(bipartite_plot_list, "bipartite_plots.rds")
write_rds(polar_plot, "polar_plot.rds")

### MISCELLANEOUS ############
# ggraph(g1a, layout = 'kk') +
#   geom_edge_fan(aes(colour = as.factor(edge_latent_community)), alpha = 0.2) +
#   # geom_edge_fan(alpha = 0.2) +
#   geom_node_point(aes(colour = as.factor(latent_community))) +
#   labs(title = 'g1a')

# ggraph(prototypes, layout = 'kk') +
#   # geom_edge_fan(aes(colour = as.factor(edge_latent_community))) +
#   geom_edge_fan() +
#   geom_node_point(aes(colour = as.factor(latent_community))) +
#   labs(title = 'Prototypes')

# ggraph(lc_prototypes_list[[1]], layout = 'kk') +
#   geom_edge_fan() +
#   geom_node_point(aes(colour = as.factor(latent_community))) +
#   labs(title = 'LC Prototypes')

# ggraph(associates, layout = 'kk') +
#   geom_edge_fan(alpha = 0.2) +
#   geom_node_point(aes(colour = as.factor(latent_community))) +
#   labs(title = 'Associates')

# ggraph(lc_associates_list[[7]], layout = 'kk') +
#   geom_edge_fan(alpha = 0.2) +
#   geom_node_point(aes(colour = as.factor(latent_community))) +
#   labs(title = 'LC Associates')
