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


# which observations are AS vs. iNat?
as_labs <- mutate(west_as_cand, "source" = "as")
iNat_labs <- mutate(west_iNat_only_cand, "source" = "iNat")
full_labs <- rbind(as_labs, iNat_labs)


# number of AS and iNat observations for each candidate species

candidates <- c("Libytheana carinenta","Celastrina echo","Gyrocheilus patrobas",
                "Eurema mexicana", "Vanessa cardui")
df <- filter(full_labs, full_labs$scientific_name %in% candidates) %>%
  group_by(scientific_name, source) %>%
  summarise(n = n()) %>%
  pivot_wider(names_from = source, values_from = n) %>%
  arrange(desc(iNat))
df 

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
hill_gpa_noas <- crop(hill, extent(pred))
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
    tm_shape(boundaries)+
    tm_borders(col = 'white')+
    tm_shape(boundaries2)+
    tm_borders(col = 'white') +
    tm_shape(pred_raster)+
    tm_raster(alpha = 1/3, style = 'cont', 
              palette = 'viridis')+
    tm_legend(legend.position = c('right', 'top'))+
    tm_scale_bar(position = c('left', 'bottom'),
                 color.dark = 'gray60')
  return(map)
}

# G patrobas iNat + AS
map_gpa <- hill_map(pred_Gpa_iNat, elev_raster = hill, 
                    boundaries = pol_usa, boundaries2 = pol_mex)
# G patrobas iNat only
map_noas_gpa <- hill_map(pred_Gpa_iNat_only, elev_raster = hill, 
                         boundaries = pol_usa, boundaries2 = pol_mex)
# L carinenta iNat + AS
map_lca <- hill_map(pred_lca_iNat, elev_raster = hill, 
                    boundaries = pol_usa, boundaries2 = pol_mex)
# L carinenta iNat only
map_noas_lca <- hill_map(pred_lca_iNat_only, elev_raster = hill,
                         boundaries = pol_usa, boundaries2 = pol_mex)
# E mexicana iNat + AS
map_eme <- hill_map(pred_eme_iNat, elev_raster = hill, 
                    boundaries = pol_usa, boundaries2 = pol_mex)
# E mexicana iNat only
map_noas_eme <- hill_map(pred_eme_iNat_only, elev_raster = hill, 
                         boundaries = pol_usa, boundaries2 = pol_mex)

tmap_arrange(map_gpa, map_noas_gpa, 
             map_lca, map_noas_lca, 
             map_eme, map_noas_eme,
             nrow = 3)
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
tm_layout(title = "Gyrocheilus patrobas")
gpa_map

