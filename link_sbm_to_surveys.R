# Started 2023-04-5
# Code to explore relationship between SBM groups, representative species and surveys.
# Run SBm_Bernouilli_positive first
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

# Load the database
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
  q <- sprintf('SELECT DISTINCT assembly_name, community, grid_ref, species_name
from surveys join quadrats on quadrats.survey_id = surveys.surveys_id
join records on quadrats.quadrats_id = records.quadrat_id
join species on species.species_id = records.species_id;') 
  
  # NOTE: this extract includes "MG5", i.e. some MG5 communities where 
  # the team have not decided
  # on a sub-group.
  
  rs1 = dbSendQuery(con, q)
  return(as_tibble(fetch(rs1, n=-1)))
  dbDisconnectAll()
}

### MAIN ###
survey_data <- GetSurveyData()
survey_data <- rename(survey_data, survey = assembly_name, species = species_name)
# Remove the species that are not represented in the SBM model
survey_data <- survey_data %>% filter(species %in% group_memberships$species)
# Add the group number
survey_data <- survey_data %>% left_join(group_memberships, by = "species")
# With survey_data, get a count of the number of species in each group represented in each survey
species_grp_counts <- survey_data %>% select(survey, grp) %>% group_by(survey, grp) %>% summarise(species_count= n())
# Count the number of groups represented by some species in each survey
survey_grp_counts <- species_grp_counts %>% select(survey, grp) %>% group_by(survey) %>% summarise(group_count = n())

# Show distribution of group counts in surveys
# ... species are grouped in the data, not neccesarily in the field - surveys can have
# as many as 4 groups represented in them.
bars1 <- ggplot (survey_grp_counts) + 
  geom_bar(mapping = aes(x = group_count)) +
  ggtitle("Survey group counts")
plot(bars1)

# If a group is represented at a site, then by how many species?
# First, count how many species in each group?
group_sizes <- group_memberships %>% group_by(grp) %>% summarise(spp_in_group = n())



