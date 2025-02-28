#Occurence data from Adventure Scientists
#Noah Giebink
#giebink@email.arizona.edu
#2019-07-19

#packages
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(ggmap)
library(mapr)

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
xerces_red_list <- 
  read.csv("data/endangered_leps.csv")
usfws_endangered_threatened <- 
  read.csv("data/endangered_leps_usfws.csv")
west_as <- read.csv("data/west_as.csv")
west_iNat <- read.csv("data/west_iNat.csv")

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

no_name <- iNat_only %>%
  filter(common_name == "")

inner_join(no_name %>% mutate(scientific_name = as.character(scientific_name)), top_5_iNat %>%
             ungroup() %>%
             mutate(place_state_name = as.character(place_state_name),
                    scientific_name = as.character(scientific_name)), by = "scientific_name")
  #Result: none of the top 5 lack a common name (i.e. no duplicate scientific names)

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
  arrange((place_state_name), desc(num_records))

#iNaturalist only--not including AS data
top_5_iNat <- iNat_only %>%
  group_by(place_state_name, scientific_name) %>%
  summarize(num_records = n()) %>%
  ungroup() %>%
  group_by(place_state_name) %>%
  top_n(n = 5, wt = num_records) %>%
  arrange((place_state_name), desc(num_records))

#All species with at least 20 observations in iNat alone, by state
iNat_coverage <- iNat_only %>%
  group_by(place_state_name, scientific_name) %>%
  summarize(num_records = n()) %>%
  ungroup() %>%
  group_by(place_state_name) %>%
  filter(num_records > 19) %>%
  arrange((place_state_name), desc(num_records))

#What do iNat and AS have in common?
top_shared <- top_5 %>%
  ungroup() %>%
  mutate(place_state_name = as.character(place_state_name), 
         scientific_name = as.character(scientific_name)) %>%
  inner_join(top_5_iNat %>%
               ungroup() %>%
               mutate(place_state_name = as.character(place_state_name),
                      scientific_name = as.character(scientific_name)), by = "scientific_name") %>%
  select(scientific_name, iNat_data_state = place_state_name.y, num_records_iNat_data = num_records.y, 
         as_data_state = place_state_name.x, num_records_as_data = num_records.x) %>%
  filter(iNat_data_state == as_data_state) %>%
  select("Species" = scientific_name, 
         "State" = iNat_data_state, 
         "iNat_Records" = num_records_iNat_data,
         "AS_Records" = num_records_as_data)

shared_coverage <- top_5 %>%
  ungroup() %>%
  mutate(place_state_name = as.character(place_state_name), 
         scientific_name = as.character(scientific_name)) %>%
  inner_join(iNat_coverage %>%
               ungroup() %>%
               mutate(place_state_name = as.character(place_state_name),
                      scientific_name = as.character(scientific_name)), by = "scientific_name") %>%
  select(scientific_name, iNat_data_state = place_state_name.y, num_records_iNat_data = num_records.y, 
         as_data_state = place_state_name.x, num_records_as_data = num_records.x) %>%
  filter(iNat_data_state == as_data_state) %>%
  select("Species" = scientific_name, 
         "State" = iNat_data_state, 
         "iNat_Records" = num_records_iNat_data,
         "AS_Records" = num_records_as_data)

#Visualize candidate species in each region
#heatmap: state~species 

speciesHeatmap <- ggplot(top_5,
                         aes(x = scientific_name,
                             y = place_state_name)) +
  geom_tile(aes(fill = num_records)) +
  coord_flip() +
  geom_text(aes(label = num_records)) +
  scale_fill_gradient(low = "white",
                      high = "red", name = "Number of records") +
  xlab("Scientific name") +
  ylab("State") +
  theme_bw() +
  theme(axis.text.y = element_text(face = "italic"),
        axis.text.x = element_text(angle = 45, 
                                   hjust = 1)) +
  ggtitle("Adventure Scientists")

speciesHeatmap_iNat_only <- ggplot(top_5_iNat,
                         aes(x = scientific_name,
                             y = place_state_name)) +
  geom_tile(aes(fill = num_records)) +
  coord_flip() +
  geom_text(aes(label = num_records)) +
  scale_fill_gradient(low = "white",
                      high = "red") +
  xlab("Scientific name") +
  ylab("State") +
  theme_bw() +
  theme(axis.text.y = element_text(face = "italic"),
        axis.text.x = element_text(angle = 45, 
                                   hjust = 1)) +
  ggtitle("iNat only")

both <- ggarrange(speciesHeatmap, speciesHeatmap_iNat_only, common.legend = TRUE)

#Map candidate species
key <- read.table("/home/noah/Documents/r-stats/ggmap_API_key")
register_google(key = key[1,1])

L_carinenta_AS <- filter(iNat_as, scientific_name == "Libytheana carinenta") %>% 
  mutate(source = "adventure_scientists") 
  L_carinenta_iNat <- filter(iNat_only, scientific_name == "Libytheana carinenta") %>%
  mutate(source = "iNat_only")
L_carinenta <- rbind(L_carinenta_iNat, L_carinenta_AS) %>% rename(name = scientific_name)
  #map_ggmap   requires a variable called "name" in addition to lat and lon

L_carinenta %>%
  map_ggmap() +
  geom_point(aes(fill = source))
  
#look at example in monarch_ml to integrate ggmap with ggplot
#species by color facet by source? Or some combination of color and faceting

# look for endangered species in AS data
AS_records <- adventure_scientists_data_raw_expanded %>%
  filter(place_state_name != "") %>%
  filter(place_state_name %in% c("Washington", "Utah", 
                                 "Montana", "California", 
                                 "Arizona")) %>%
  group_by(place_state_name, scientific_name) %>%
  summarize(num_records = n()) %>%
  ungroup() %>%
  arrange(scientific_name, desc(num_records))


AS_red_list <- inner_join(AS_records, xerces_red_list,
                            by = "scientific_name")
  #none are Xerces red-listed. BUT, does NOT account for subsp. name
AS_endangered_threatened  <- inner_join(usfws_endangered_threatened, 
                                        AS_records, 
                                        by = "scientific_name")
  #Figure out how to search "starts with" any value in a column of strings
  #the answer is likely in:
  #https://stackoverflow.com/questions/38724690/r-filter-rows-that-contain-a-string-from-a-vector/38726850
  #or 
  #https://github.com/rstudio/cheatsheets/blob/master/strings.pdf (stringr cheat sheet)



#REPEAT LISTS & HEATMAPS FOR "WEST" DATASETS

#Candidate species: top 5 by number of records in each state:
#Adventure Scientists
top_5_west = west_as %>%
  filter(place_state_name != "") %>%
  filter(place_state_name %in% c("Washington", "Utah", 
                                 "Montana", "California", 
                                 "Arizona")) %>%
  group_by(place_state_name, scientific_name) %>%
  summarize(num_records = n()) %>%
  ungroup() %>%
  group_by(place_state_name) %>%
  top_n(n = 5, wt = num_records) %>%
  arrange((place_state_name), desc(num_records))

#iNaturalist only--not including AS data
top_5_iNat_west <-  west_iNat_only %>%
  filter(place_state_name != "") %>%
  filter(place_state_name %in% c("Washington", "Utah", 
                                 "Montana", "California", 
                                 "Arizona")) %>%
  group_by(place_state_name, scientific_name) %>%
  summarize(num_records = n()) %>%
  ungroup() %>%
  group_by(place_state_name) %>%
  top_n(n = 5, wt = num_records) %>%
  arrange((place_state_name), desc(num_records))  


#All species with at least 20 observations in iNat alone, by state
iNat_coverage_west <- west_iNat_only %>%
  filter(place_state_name != "") %>%
  group_by(place_state_name, scientific_name) %>%
  summarize(num_records = n()) %>%
  ungroup() %>%
  group_by(place_state_name) %>%
  filter(num_records > 19) %>%
  arrange((place_state_name), desc(num_records))

#What do iNat and AS have in common?
shared_coverage_west <- top_5_west %>%
  ungroup() %>%
  mutate(place_state_name = as.character(place_state_name), 
         scientific_name = as.character(scientific_name)) %>%
  inner_join(iNat_coverage_west %>%
               ungroup() %>%
               mutate(place_state_name = as.character(place_state_name),
                      scientific_name = as.character(scientific_name)), by = "scientific_name") %>%
  select(scientific_name, iNat_data_state = place_state_name.y, num_records_iNat_data = num_records.y, 
         as_data_state = place_state_name.x, num_records_as_data = num_records.x) %>%
  filter(iNat_data_state == as_data_state) %>%
  select("Species" = scientific_name, 
         "State" = iNat_data_state, 
         "iNat_Records" = num_records_iNat_data,
         "AS_Records" = num_records_as_data)

#Visualize candidate species in each region
#heatmap: state~species 

speciesHeatmap_west <- ggplot(top_5_west,
                         aes(x = scientific_name,
                             y = place_state_name)) +
  geom_tile(aes(fill = num_records)) +
  coord_flip() +
  geom_text(aes(label = num_records)) +
  scale_fill_gradient(low = "white",
                      high = "red", name = "Number of records") +
  xlab("Scientific name") +
  ylab("State") +
  theme_bw() +
  theme(axis.text.y = element_text(face = "italic"),
        axis.text.x = element_text(angle = 45, 
                                   hjust = 1)) +
  ggtitle("Adventure Scientists")

speciesHeatmap_west_iNat_only <- ggplot(top_5_iNat,
                                   aes(x = scientific_name,
                                       y = place_state_name)) +
  geom_tile(aes(fill = num_records)) +
  coord_flip() +
  geom_text(aes(label = num_records)) +
  scale_fill_gradient(low = "white",
                      high = "red") +
  xlab("Scientific name") +
  ylab("State") +
  theme_bw() +
  theme(axis.text.y = element_text(face = "italic"),
        axis.text.x = element_text(angle = 45, 
                                   hjust = 1)) +
  ggtitle("iNat only")

both_west <- ggarrange(speciesHeatmap_west, speciesHeatmap_west_iNat_only, common.legend = TRUE)










