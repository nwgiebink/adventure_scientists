# Maps and Tables
# Noah Giebink
# Noah Giebink
# giebink@email.arizona.edu
# 2019/11/2


# Packages
library(tidyverse)
library(ggpubr)
library(ggmap)
library(raster)
library(rgeos)
library(tmap)
library(tmaptools)
library(shinyjs)
library(formattable)
library(knitr)
library(kableExtra)
library(magick)
library(webshot)

# which observations are AS vs. iNat?
as_labs <- mutate(west_as_cand, "source" = "as")
iNat_labs <- mutate(west_iNat_only_cand, "source" = "iNat")
full_labs <- rbind(as_labs, iNat_labs)


# number of AS and iNat observations for each candidate species

candidates <- c('Gyrocheilus patrobas', 'Eurema mexicana', 'Libytheana carinenta', 
                'Celastrina echo', 'Vanessa cardui')
df <- filter(full_labs, full_labs$scientific_name %in% candidates) %>%
  group_by(scientific_name, source) %>%
  summarise(n = n()) %>%
  pivot_wider(names_from = source, values_from = n) %>%
  arrange((iNat)) %>%
  select('Species' = 'scientific_name', 'AS' = 'as', 'iNat')
df 


sample <- kable(df) %>%
  kable_styling() %>%
  row_spec(1:3, bold = T)

as_image(sample, file = 'sample.png', width = 10)



# Background map layers ----
 
# Hillshade layer
 # for Mexico
el_mex <- getData('alt', country = 'MEX')
  # for USA
el_usa <-  getData('alt', country = 'USA')
el_usa <- el_usa[[1]]
  # USA + Mexico
el_list <- list(el_mex, el_usa)
el <- do.call(merge, el_list)
  # make hillShade USA + Mexico
slope <- terrain(el, opt = 'slope')
aspect <- terrain(el, opt = ' aspect')
hill <- hillShade(slope, aspect, 40, 270)

# Political boundaries layer
pol_usa <- getData(country = 'USA', level=1) 
pol_mex <- getData(country = 'MEX', level=1) 

# crop rasters to SDM extent
  # Hillshadow
hill_gpa <- crop(hill, extent(pred_Gpa_iNat))
  # political boundaries
pol_usa_gpa <- crop(pol_usa, extent(pred_Gpa_iNat))
pol_mex_gpa <- crop(pol_mex, extent(pred_Gpa_iNat))

# plots ----

hill_map <- function(pred_raster, elev_raster, boundaries, boundaries2 = NULL){
  # crop rasters to SDM extent
  # Hillshadow
  elev_raster <- crop(elev_raster, extent(pred_raster))
  # political boundaries
  boundaries <- crop(boundaries, extent(pred_raster))
  boundaries2 <- crop(boundaries2, extent(pred_raster))
  
  map <- tm_shape(elev_raster) +
    tm_raster(palette = gray(0:100/100), n=100, legend.show = FALSE)+
    tm_shape(boundaries) +
    tm_borders(col = 'white') +
    tm_shape(boundaries2) +
    tm_borders(col = 'white') +
    tm_shape(pred_raster) +
    tm_raster(alpha = 2/5, style = 'cont', 
              palette = 'viridis') +
    tm_legend(legend.position = c('right', 'top')) +
    tm_scale_bar(position = c('left', 'bottom'),
                 color.dark = 'gray60') +
    tm_layout(legend.outside = TRUE,
              legend.title.size = 1,
              legend.outside.position = c('right', 'top'))
  return(map)
}

# plots ----

hill_map2 <- function(pred_raster, elev_raster, boundaries, boundaries2 = NULL){
  # crop rasters to SDM extent
  # Hillshadow
  elev_raster <- crop(elev_raster, extent(pred_raster))
  # political boundaries
  boundaries <- crop(boundaries, extent(pred_raster))
  boundaries2 <- crop(boundaries2, extent(pred_raster))
  
  map <- tm_shape(elev_raster) +
    tm_raster(palette = gray(0:100/100), n=100, legend.show = FALSE)+
    tm_shape(boundaries) +
    tm_borders(col = 'white') +
    tm_shape(boundaries2) +
    tm_borders(col = 'white') +
    tm_shape(pred_raster) +
    tm_raster(alpha = 2/5, style = 'cont', 
              palette = 'viridis') +
    tm_legend(legend.position = c('right', 'top')) +
    tm_scale_bar(position = c('left', 'bottom'),
                 color.dark = 'gray60') +
    tm_layout(legend.show = FALSE)
  return(map)
}

# G patrobas iNat + AS
map_gpa <- hill_map2(pred_Gpa_iNat, elev_raster = hill, 
                    boundaries = pol_usa, boundaries2 = pol_mex)
# G patrobas iNat only
map_noas_gpa <- hill_map2(pred_Gpa_iNat_only, elev_raster = hill, 
                         boundaries = pol_usa, boundaries2 = pol_mex)
# L carinenta iNat + AS
map_lca <- hill_map2(pred_lca_iNat, elev_raster = hill, 
                    boundaries = pol_usa, boundaries2 = pol_mex)
# L carinenta iNat only
map_noas_lca <- hill_map2(pred_lca_iNat_only, elev_raster = hill,
                         boundaries = pol_usa, boundaries2 = pol_mex)
# E mexicana iNat + AS
map_eme <- hill_map2(pred_eme_iNat, elev_raster = hill, 
                    boundaries = pol_usa, boundaries2 = pol_mex)
# E mexicana iNat only
map_noas_eme <- hill_map2(pred_eme_iNat_only, elev_raster = hill, 
                         boundaries = pol_usa, boundaries2 = pol_mex)
save_tmap(map_gpa, 'maps/map_gpa.png')
save_tmap(map_noas_gpa, 'maps/map_noas_gpa.png')
save_tmap(map_eme, 'maps/map_eme.png')
save_tmap(map_noas_eme, 'maps/map_noas_eme.png')
save_tmap(map_noas_lca, 'maps/map_noas_lca')
save_tmap(map_lca, 'maps/map_lca.png')


  # made before hill_map function ----
gpa_map <- tm_shape(hill_gpa) +
  tm_raster(palette = gray(0:100/100), n=100, legend.show = FALSE)+
tm_shape(pol_mex_gpa)+
  tm_borders(col = 'white')+
tm_shape(pol_usa_gpa)+
  tm_borders(col = 'white') +
tm_shape(pred_Gpa_iNat)+
  tm_raster(alpha = 1/3, style = 'cont', 
            palette = 'viridis')+
  tm_legend(legend.position = c('right', 'top'))+
tm_scale_bar(position = c('left', 'bottom'),
             color.dark = 'gray60')+
  tm_layout(legend.show = FALSE)
gpa_map

save_tmap(gpa_map, 'maps/gpa_map.png')


# Benchmark-like test for G. patrobas model performance comparison ----

# convert elevation raster to data frame
el2 <- as(el, "SpatialPixelsDataFrame")
elev <- as.data.frame(el2)
  # way too big to realistically work with
# convert cropped to data frame
elev2 <- crop(el2, extent(pred_Gpa_iNat))
elev2 <- as.data.frame(elev2)

elev3 <- crop(el2, extent(pred_Gpa_iNat_only2))
elev3 <- as.data.frame(elev3)

# check elevation looks right
elplot <- ggplot()+
  geom_tile(data = elev2, aes(x, y, fill = layer))
  #looks right

# merge elevation and prediction data frames
pred_Gpa_iNat2 <- as(pred_Gpa_iNat, 
                    "SpatialPixelsDataFrame")
pred_Gpa_iNat2 <- as.data.frame(pred_Gpa_iNat) %>%
  rename("value" = "layer")

# iNat only
pred_Gpa_iNat_only2 <- as(pred_Gpa_iNat_only, 
                          "SpatialPixelsDataFrame")
pred_Gpa_iNat_only2 <- as.data.frame(pred_Gpa_iNat_only2) %>%
  rename("value" = "layer")

gpa_iNat_el <- inner_join(pred_Gpa_iNat2, elev2, by = c('x','y')) %>%
  select('lon' = x, 'lat' = y, 'probability_of_occurrence' = value, 'elevation' = layer) %>%
  mutate(source = "iNat + AS")
gpa_iNat_only_el <- inner_join(pred_Gpa_iNat_only2, elev3, by = c('x','y')) %>%
  select('lon' = x, 'lat' = y, 'probability_of_occurrence' = value, 'elevation' = layer) %>%
  mutate(source = "iNat Only")

gpa_el <- rbind(gpa_iNat_only_el, gpa_iNat_el)


# elevation occurrence data frames
# get occ pts
gpa_iNat_occ <- G_patrobas_iNat2 %>%
  select(Species, latitude, longitude) %>%
  filter(Species == 1) %>%
  select(longitude, latitude)

gpa_iNat_only_occ <- Gpa_iNat_only2 %>%
  select(Species, latitude, longitude) %>%
  filter(Species == 1) %>%
  select(longitude, latitude)

# iNat + AS
elev_ras <- raster(crop(el2, extent(pred_Gpa_iNat)))
el_occ <- raster::extract(x = elev_ras, y = gpa_iNat_occ)
elev_df <- cbind(el_occ, gpa_iNat_occ) %>%
  mutate(source = "iNat + AS")

# iNat only
elev_ras2 <- raster(crop(el2, extent(pred_Gpa_iNat_only2)))
el_occ2 <- raster::extract(x = elev_ras2, y = gpa_iNat_only_occ)
elev_df2 <- cbind(el_occ2, gpa_iNat_only_occ) %>%
  mutate(source = "iNat Only") %>%
  rename(el_occ = el_occ2)

elev_df_full <- rbind(elev_df2, elev_df)


# visualize the distribution of P(occ) values across elevation range
# 
  # Scott's 'The Butterflies of North America' limits G. patrobas
  # habitat from upper Sonoran to transition open woodland (ponderosa)
  # based on Merriam's life zones scheme, this range is about 1200-2750m

gpa_el_plot_rug <- ggplot(gpa_el, aes(elevation, probability_of_occurrence, fill = source))+
  geom_area()+ # shows bar for ymax across x values
  geom_vline(xintercept = c(1200, 2750), 
             linetype = 'dashed')+
  coord_cartesian(ylim = c(0,1))+
  facet_wrap(~source)+
  xlab('Elevation (m)')+
  ylab('Max Probability of Occurrence')+
  theme(legend.position = 'none')+
  theme(panel.background = element_blank(),
        axis.line = element_line(color = 'black'),
        axis.text = element_text(color = "black"))+
  geom_rug(data = elev_df_full, mapping = aes(el_occ),
           inherit.aes = F, alpha = 1/6)
gpa_el_plot
gpa_el_plot_rug

  # regression
gpa_iNat_glm <- glm(gpa_iNat_el$probability_of_occurrence~gpa_iNat_el$elevation)
summary(gpa_iNat_glm)

gpa_iNat_only_glm <- glm(gpa_iNat_only_el$probability_of_occurrence~gpa_iNat_only_el$elevation)
summary(gpa_iNat_only_glm)
# Not particularly informative...both have *** signif. codes (p~=0)

# Where the models predict out of expected elevation range,
# is it due to out-of-range occurrences or extrapolation?

# any observations outside of elevation range (Scott)?
elev_df %>%
  mutate(elevation = if_else(el_occ < 1200, 'below expected',
                     if_else(el_occ > 2750, 'above expected', 
                                    'expected'))) %>%
  filter(elevation != 'expected')
elev_df2 %>%
  mutate(elevation = if_else(el_occ2 < 1200, 'below expected',
                             if_else(el_occ2 > 2750, 'above expected', 
                                    'expected'))) %>%
  filter(elevation != 'expected')
  # result: no observations outside of range in either data set

ggplot(elev_df, aes(el_occ))+
  geom_histogram()
  
