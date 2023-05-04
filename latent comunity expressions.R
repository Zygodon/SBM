library(tidyverse)
library(igraph)
library(ggraph)
library(tidygraph)
library(RColorBrewer)

PolarPlot <- function(lc){
  data <- survey_columns %>% filter(lc_id==lc)
  data_labels <- data %>% 
    select(survey) %>% 
    mutate(id = seq_along(survey)) %>% 
    # Subtract 0.5 because the letter must have the angle of the center of the bars, 
    # not extreme right(1) or extreme left (0)
    mutate(angle =  90 - 360 * (id-0.5) /length(survey)) %>%  
    # calculate the alignment of labels: right or left
    # If I am on the left part of the plot, my labels have currently an angle < -90
    mutate(hjust = ifelse( angle < -90, 1, 0)) %>%
    # Flip angles BY 180 degrees to make them readable
    mutate(angle=ifelse(angle < -90, angle+180, angle))
  y_max <- ceiling(max(data$xp))
  
  p <- ggplot(data) + 
    geom_col(aes(x = survey, y = xp), fill="steelblue3") +
    coord_polar(start = 0) +
    ylim(-20,y_max) +
    ggtitle(paste("Site expression of latent community", lc, sep = " ")) +
    theme(
      axis.text.x = element_blank(),
      axis.title.x = element_blank())
  # Add the survey labels.
  # p + geom_text(data = data_labels, aes(x=id, y=y_max-20, label=survey, hjust=hjust), 
  p + geom_text(data = data_labels, aes(x=id, y=0.8*y_max, label=survey, hjust=hjust), 
                              color="black", alpha=0.7, size=3, angle=data_labels$angle, inherit.aes = FALSE ) +
    guides(fill = guide_legend("Latent Community"))
}


survey_columns <- read.csv("site latent communities.csv") %>% select(-1)
survey_columns <- survey_columns %>% mutate(lc_id = as.numeric(substring(survey_columns$LC, 3,3)))

lc_count <- survey_columns %>% select(lc_id) %>% max()

for(lc in 1:lc_count) {plot(PolarPlot(lc))}
