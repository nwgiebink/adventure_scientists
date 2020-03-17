# Occurrence data for all western US
# updated from adventure_scientists_data.R
# 2020-3-3
# Noah Giebink


#packages
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(ggmap)
library(mapr)


# Data
west_as <- read.csv("data/west_as.csv")
west_iNat <- read.csv("data/west_iNat.csv")


# Remove adventure scientist observations from west_iNat
west_iNat_only <- filter(west_iNat, !(west_iNat$id %in% west_as$id))

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

both_west <- ggarrange(speciesHeatmap_west_iNat_only, speciesHeatmap_west, common.legend = TRUE)
both_west