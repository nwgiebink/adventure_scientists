#Occurence data from Adventure Scientists
#Noah Giebink
#giebink@email.arizona.edu
#2019-07-19

#data
adventure_scientists_data_raw <-
  read.csv("data/iNaturalist_AdventureScientists.csv")
adventure_scientists_data_raw_expanded <-
  read.csv("data/iNaturalist_AdventureScientists_taxonInfoExpanded.csv")

#packages
library(tidyverse)
library(ggplot2)
library(corrplot)

#DATA EXPLORATION

#check for synonymous names
#correlation plot: common name~species name
names <- select(adventure_scientists_data_raw_expanded, 
                common_name,
                scientific_name
                )

names_corr <- corrplot(names, method = "number")
  #doesn't work yet 

#identify good candidate species in each region
#heatmap: state~species 

speciesHeatmap <- ggplot(adventure_scientists_data_raw_expanded,
                         aes(x = scientific_name,
                             y = place_state_name)) +
  geom_tile() +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 1))

#heatmap:state~genus

genusHeatmap <- ggplot(adventure_scientists_data_raw_expanded,
                         aes(x = taxon_genus_name,
                             y = place_state_name)) +
  geom_tile() +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 1))

#heatmap: state~family
familyHeatmap <- ggplot(adventure_scientists_data_raw_expanded,
                       aes(x = taxon_family_name,
                           y = place_state_name)) +
  geom_tile() +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 1))
  #by far the easiest to read, but less informative


#heatmaps currently only show presence/absence...
#add some metric of relative abundance within states
#and map to aes() with fill = 

