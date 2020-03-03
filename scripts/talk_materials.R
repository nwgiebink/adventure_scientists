# Talk materials
# Noah Giebink
# giebink@email.arizona.edu
# 2019-11-17

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
library(rgdal)
library(maps)


# AZ map for SDMs & sampling bias demo ----

# Google API Key
key <- read.table("/home/noah/Documents/r-stats/ggmap_API_key")
register_google(key = key[1,1])

az<- get_map(c(left = -115, right = -108.7, bottom = 31, top = 37.3)) %>%
ggmap()+
  theme_void()




