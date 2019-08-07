#Occurence data from Adventure Scientists
#Noah Giebink
#giebink@email.arizona.edu
#2019-07-19

#packages
library(tidyverse)
library(ggplot2)

#data
adventure_scientists_data_raw <-
  read.csv("data/iNaturalist_AdventureScientists.csv")
adventure_scientists_data_raw_expanded <-
  read.csv("data/iNaturalist_AdventureScientists_taxonInfoExpanded.csv")
iNat_montana <- 
  read.csv("data/iNat_montana.csv")
iNat_california <-
  read.csv("data/iNat_california.csv")
iNat_washington <-
  read.csv("data/iNat_washington.csv")
iNat_utah <-
  read.csv("data/iNat_utah.csv")
iNat_arizona <-
  read.csv("data/iNat_arizona.csv")
iNat_as <-
  read.csv("data/iNat_as.csv") 
#iNat_as is updated Adventure Scientists data as of 2019-8-6
#retrieved after iNat_[state] data (also 2019-8-6)
#to exclude AS data from iNat

#iNat states compiled
iNat_full <-
  rbind(iNat_arizona, iNat_california, iNat_montana, iNat_utah, iNat_washington) 
  
#remove Adventure Scientists observations from iNat
as_observations <- select(iNat_as, id)
iNat_only <- filter(iNat_full, !(iNat_full$id %in% as_observations$id))

#DATA EXPLORATION

#Checking to make sure each common name doesn't have multiple species names
adventure_scientists_data_raw_expanded %>%
  group_by(common_name) %>%
  summarize(unique_spec = n_distinct(scientific_name)) %>%
  print(n = 149)

iNat_only_duplicates <- iNat_only %>%
  group_by(common_name) %>%
  summarize(unique_spec = n_distinct(scientific_name)) %>%
  filter(unique_spec != 1)
  #123 species with no common name
  #3 common names with duplicate (2) species names:
  #Arrowhead Blue, Large Marble, Pacific Dotted-Blue

#Checking out species that don't have a common name
the_naughty_list = adventure_scientists_data_raw_expanded %>%
  filter(common_name == "") %>%
  group_by(scientific_name) %>%
  summarize(n = n())

#Candidate species: top 5 by number of records in each state:
#Adventure Scientists
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
#iNaturalist only--not including AS data
top_5_iNat <- iNat_only %>%
  group_by(place_state_name, scientific_name) %>%
  summarize(num_records = n()) %>%
  ungroup() %>%
  group_by(place_state_name) %>%
  top_n(n = 5, wt = num_records) %>%
  arrange((place_state_name), desc(num_records)) %>%
  print(n = 46)

#What do iNat and AS have in common?
top_shared <- top_5 %>%
  ungroup() %>%
  mutate(place_state_name = as.character(place_state_name), 
         scientific_name = as.character(scientific_name)) %>%
  inner_join(top_5_iNat %>%
               ungroup() %>%
               mutate(place_state_name = as.character(place_state_name),
                      scientific_name = as.character(scientific_name)), by = "scientific_name")
             
#Visualize candidate species in each region
#heatmap: state~species 

speciesHeatmap <- ggplot(top_5,
                         aes(x = scientific_name,
                             y = place_state_name)) +
  geom_tile(aes(fill = num_records)) +
  coord_flip() +
  geom_text(aes(label = num_records)) +
  scale_fill_gradient(low = "white",
                      high = "red") +
theme(axis.text.x = element_text(angle = 45,
                                 hjust = 1))

speciesHeatmap_iNat_only <- ggplot(top_5_iNat,
                         aes(x = scientific_name,
                             y = place_state_name)) +
  geom_tile(aes(fill = num_records)) +
  coord_flip() +
  geom_text(aes(label = num_records)) +
  scale_fill_gradient(low = "white",
                      high = "red") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))


