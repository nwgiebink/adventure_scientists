#SDMs for butteflies of Western U.S. 
#and Northern Mexico
#Noah Giebink
#giebink@email.arizona.edu
#2019-9-17

#Packages
library(tidyverse)

#Data
  #All research-grade AS and iNat butterfly (Papilionoidea) 
  #records within swlat 28.0 swlng -125.0 nelat 49.0 nelng -100.0
west_as <- read.csv("data/west_as.csv")
west_iNat <- read.csv("data/west_iNat.csv")

  #remove Adventure Scientists observations from iNat
west_as_observations <- select(west_as, id)
west_iNat_only <- filter(west_iNat, !(west_iNat$id %in% as_observations$id))

