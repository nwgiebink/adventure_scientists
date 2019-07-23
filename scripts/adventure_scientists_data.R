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

#Checking to make sure each common name doesn't have multiple species names
adventure_scientists_data_raw_expanded %>%
  group_by(common_name) %>%
  summarize(unique_spec = n_distinct(scientific_name)) %>%
  print(n = 149)

#Checking out species that don't have a common name
the_naughty_list = adventure_scientists_data_raw_expanded %>%
  filter(common_name == "") %>%
  group_by(scientific_name) %>%
  summarize(n = n())

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

#tidyverse candidate species
top_5 = adventure_scientists_data_raw_expanded %>%
  filter(place_state_name != "") %>%
  filter(place_state_name %in% c("Washington", "Utah", 
                                 "Montana", "California", 
                                 "Arizona")) %>%
  group_by(place_state_name, scientific_name) %>%
  summarize(num_records = n()) %>%
  ungroup() %>%
  group_by(place_state_name) %>%
  top_n(n = 5, wt = num_records) %>%
  arrange((place_state_name), desc(num_records)) %>%
  print(n = 46)
