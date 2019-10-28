#SDMs for butteflies of Western U.S. 
#and Northern Mexico
#Noah Giebink
#giebink@email.arizona.edu
#2019-9-17

#Packages ----------------------------------------------------------------
library(tidyverse)

#Data --------------------------------------------------------------------
  
# Data from later outputs
G_patrobas_iNat <- readRDS('data/G_patrobas_iNat.rds')
G_patrobas_iNat_blocked <- readRDS("data/G_patrobas_iNat_blocked.rds")
G_patrobas_iNat2 <- read.csv("data/G_patrobas_iNat2.csv")
G_patrobas_iNat2_split <- readRDS('data/G_patrobas_iNat2_split.rds')
G_patrobas_iNat_model <- readRDS("data/G_patrobas_inat_model.rds")
G_patrobas_iNat_best <- readRDS("data/G_patrobas_iNat_best.rds")
G_patrobas_iNat_eval <- readRDS("data/G_patrobas_iNat_eval.rds")
G_patrobas_iNat_full <- readRDS("data/G_patrobas_iNat_full.rds")

# Data from fresh start
  #All research-grade AS and iNat butterfly (Papilionoidea) 
  #records within swlat 28.0 swlng -125.0 nelat 49.0 nelng -100.0
west_as <- read.csv("data/west_as.csv")
west_iNat <- read.csv("data/west_iNat.csv")

  #remove Adventure Scientists observations from iNat
west_as_observations <- select(west_as, id)
west_iNat_only <- filter(west_iNat, !(west_iNat$id %in% as_observations$id))

  # Candidates all data (iNat + AS)
west_iNat_cand <- filter(west_iNat, scientific_name == "Libytheana carinenta"|
                           scientific_name == "Celastrina echo"|
                           scientific_name == "Gyrocheilus patrobas"|
                           scientific_name == "Eurema mexicana"|
                           scientific_name == "Vanessa cardui")
  # candidates iNat data only
west_iNat_only_cand <- filter(west_iNat_only, scientific_name == "Libytheana carinenta"|
                                scientific_name == "Celastrina echo"|
                                scientific_name == "Gyrocheilus patrobas"|
                                scientific_name == "Eurema mexicana"|
                                scientific_name == "Vanessa cardui")
  # candidates AS data only
west_as_cand <- filter(west_as, scientific_name == "Libytheana carinenta"|
                         scientific_name == "Celastrina echo"|
                         scientific_name == "Gyrocheilus patrobas"|
                         scientific_name == "Eurema mexicana"|
                         scientific_name == "Vanessa cardui")

# Google API Key ----
key <- read.table("/home/noah/Documents/r-stats/ggmap_API_key")
register_google(key = key[1,1])


# Quick maps ------------------


  # Map candidate species west_iNat_cand
map_cand <- get_map(c(left = -125, right = -100, bottom = 28, top = 49)) %>%
  ggmap() +
  geom_point(data = west_iNat_cand,
             aes(x = longitude, y = latitude, color = scientific_name)) +
  facet_wrap(~scientific_name)

map_cand_AZ <- get_map(c(left = -115, right = -105, bottom = 28, top = 38)) %>%
  ggmap() +
  geom_point(data = west_iNat_cand,
             aes(x = longitude, y = latitude, color = scientific_name)) +
  facet_wrap(~scientific_name)


  # Map west_iNat_only_cand
map_cand_iNat <- get_map(c(left = -125, right = -100, bottom = 28, top = 49)) %>%
  ggmap() +
  geom_point(data = west_iNat_only_cand,
             aes(x = longitude, y = latitude, color = scientific_name)) +
  facet_wrap(~scientific_name)

  # Map west_as_cand
map_cand_as <- get_map(c(left = -125, right = -100, bottom = 28, top = 49)) %>%
  ggmap() +
  geom_point(data = west_as_cand,
             aes(x = longitude, y = latitude, color = scientific_name)) +
  facet_wrap(~scientific_name)

# pre-prep and species_sep for compatibility with Keaton's functions ----

#' Function: prep data for SDM-building function (below)
#' Split observed_on into date, year
#' change scientific_name to true_name
#' @param data_in The df to be modified

pre_prep <- function(data_in){
  mod = data_in %>%
  separate(col ="observed_on", into = c("year", "month", "day"), 
           sep = "-") %>%
  unite(col = "date", "month", "day", sep = "-") %>%
  rename("true_name" = "scientific_name", 
         "Species" = "taxon_species_name",
         "ID" = "id")
  return(mod)
}

#' Function species_sep splits a dataframe into a list with separate dfs for each species
#' into a list of dataframes for each individual species
#' @param data_in The df to be manipulated
#' @param tag String to 'tag' onto the end of each auto-generated name in the list  


species_sep <- function(data_in, tag){
  data_in$true_name <- droplevels(data_in$true_name)
  data_in <- mutate(data_in, list_name = true_name)
  data_in$list_name <- str_replace(data_in$list_name, " ", "_")
  data_in <- split(data_in, data_in$list_name)
  new_names <- paste0(names(data_in), tag)
  glimpse(data_in)
  names(data_in) <- new_names
  return(data_in)
}


# run pre_prep and species_sep on all data ----------
  # AS data
prepped_as <- pre_prep(west_as_cand)
prepped_as <- species_sep(prepped_as, "_as")
  # iNat data
prepped_iNat <- pre_prep(west_iNat_cand)
prepped_iNat <- species_sep(prepped_iNat, "_iNat")
  #iNat only data
prepped_iNat_only <- pre_prep(west_iNat_only_cand)
prepped_iNat_only <- species_sep(prepped_iNat_only, "_iNat_only")

# access one df within the list e.g. prepped_iNat$Vanessa_cardui_iNat


# Function for building butterfly SDMs -----------------------------------
# Based on https://github.com/keatonwilson/butterfly-species-declines/blob/master/sdm_function.R
# Keaton Wilson
# keatonwilson@me.com
# 2019-09-20


# packages ----------------------------------------------------------------

library(dismo)
library(raster)
library(tidyverse)
library(blockCV)
library(tidyverse)
library(maxnet)
library(ENMeval)
if (Sys.getenv("JAVA_HOME")!="")
  Sys.setenv(JAVA_HOME="")
library(rJava)
library(ggmap)

# Importing big bioclim data ----------------------------------------------
#' @examples
#' bv_t1 = readRDS("./data/bioclim_t1.rds")
#' bv_t2 = readRDS("./data/bioclim_t2.rds")

#Bioclim data for U.S. west of 100th meridian
bv_as <- readRDS("./data/as_terraclim.rds")

bv_as_t1 <- bv_as[[1]]
bv_as_t2 <- bv_as[[2]]

# Prepping Occurrence Data ------------------------------------------------

#' Initial prepping of occurence data to create SDMs
#'
#' @param data A dataframe outputted from the butt_obs script. Generated from 
#' \code{\link[spocc]{occ}}
#' @param year_split The year to split the data by. Non inclusive (e.g. 2000 will
#'  split the  data into everything through 1999, and 2000-everything after).
#'  Default is the year 2000. 
#'
#' @return A list four elements long: the first two are the occurence data split
#'  by the year_split argument with 10k background points added. Additionally, 
#'  they are converted to a spatial points dataframe. 
#'  The second two items are the env rasters cropped 
#'  to the area of the occurences for each subset split by year_split.
#'
#' @examples

prep_data = function(data, year_split = 2000, env_raster_t1, env_raster_t2) {
  
  # selecting the pieces we want and separating by time
  small_data = data %>%
    dplyr::select(name = true_name, longitude, latitude, date, year) %>%
    mutate(time_frame = ifelse(year < year_split, "t1", "t2"))
    #2019/10/15 added dplyr::select to resolve error: "unable to find an
    #inherited method for function 'select' for signature '"data.frame"'
  
  # calculating extent of occurences
  max_lat = ceiling(max(small_data$latitude))
  min_lat = floor(min(small_data$latitude))
  max_lon = ceiling(max(small_data$longitude))
  min_lon = floor(min(small_data$longitude))
  
  # added a 1º buffer in every direction
  geographic_extent <- extent(x = c(min_lon-1, max_lon+1, min_lat-1, max_lat+1))
  
  # Crop bioclim data to geographic extent of species
  bv_t1_cropped <- crop(x = env_raster_t1, y = geographic_extent)
  bv_t2_cropped <- crop(x = env_raster_t2, y = geographic_extent)
  
  # Split by time period into two data frames
  df_t1 = small_data %>% 
    filter(time_frame == "t1")
  df_t2 = small_data %>%
    filter(time_frame == "t2")
  
  # print each to make sure it looks ok
  print(glimpse(df_t1))
  print(glimpse(df_t2))
  
  # Generate 10k background points for each one. 
  bg_t1 = dismo::randomPoints(bv_t1_cropped, 10000)
  colnames(bg_t1) = c("longitude", "latitude")
  
  bg_t2 = randomPoints(bv_t2_cropped, 10000)
  colnames(bg_t2) = c("longitude", "latitude")
  
  # Merging background data and occurence data
  df_comb_t1 = data.frame(df_t1) %>%
    mutate(pb = 1) %>%
    dplyr::select(pb, longitude, latitude) %>%
    bind_rows(data.frame(bg_t1) %>% 
                mutate(pb = 0))  %>%
    mutate(Species = as.integer(pb)) %>%
    dplyr::select(-pb)
  
  df_comb_t2 = data.frame(df_t2) %>%
    mutate(pb = 1) %>%
    dplyr::select(pb, longitude, latitude) %>%
    bind_rows(data.frame(bg_t2) %>% 
                mutate(pb = 0)) %>%
    mutate(Species = as.integer(pb)) %>%
    dplyr::select(-pb)
  
  # Changing to a spatial points data frame
  df_sp_t1 = SpatialPointsDataFrame(df_comb_t1[,c("longitude","latitude")], 
                                    df_comb_t1, 
                                    proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")) 
  df_sp_t1$time_frame = "t1"
  df_sp_t2 = SpatialPointsDataFrame(df_comb_t2[,c("longitude","latitude")], 
                                    df_comb_t2, 
                                    proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  df_sp_t2$time_frame = "t2"
  
  #Converting to a list with the two dataframes
  prepared_data_list = list(data = list(t1 = df_sp_t1, t2 = df_sp_t2),
                            env_data = list(bv_t1_cropped, bv_t2_cropped))
  #Names
  bio_names = c()
  for(i in 1:19){
    bio_names[i] = paste0("Bio", i)
  }
  
  names(prepared_data_list[[2]][[1]]) = bio_names
  names(prepared_data_list[[2]][[2]]) = bio_names
  
  return(prepared_data_list)
}

#' Run prep_data function with iNat data ---------------------------------

G_patrobas_iNat <- prep_data(data = prepped_iNat$Gyrocheilus_patrobas_iNat, 
          year_split = 2000, 
          env_raster_t1 = bv_as_t1, env_raster_t2 = bv_as_t2)

saveRDS(G_patrobas_iNat, 'data/G_patrobas_iNat.rds')

# CHECKPOINT
# get a map of the data to see range size 
# and determine optimal block size for Block CV function

# Map G_patrobas
  # all iNaturalist data
map_G_patrobas_iNat <- get_map(c(left = -125, right = -100, bottom = 28, top = 49)) %>%
  ggmap() +
  geom_point(data = prepped_iNat$Gyrocheilus_patrobas_iNat,
             aes(x = longitude, y = latitude))
map_G_patrobas_iNat_AZ <- get_map(c(left = -115, right = -105, bottom = 28, top = 38)) %>%
  ggmap() +
  geom_point(data = prepped_iNat$Gyrocheilus_patrobas_iNat,
             aes(x = longitude, y = latitude))
  # AS data
map_G_patrobas_as_AZ <- get_map(c(left = -115, right = -105, bottom = 28, top = 38)) %>%
  ggmap() +
  geom_point(data = prepped_as$Gyrocheilus_patrobas_as,
             aes(x = longitude, y = latitude))

# Map other candidates
map_C_echo_iNat_AZ <- get_map(c(left = -115, right = -105, bottom = 28, top = 38)) %>%
  ggmap() +
  geom_point(data = prepped_iNat$Celastrina_echo_iNat,
             aes(x = longitude, y = latitude))
map_C_echo_as_AZ <- get_map(c(left = -115, right = -105, bottom = 28, top = 38)) %>%
  ggmap() +
  geom_point(data = prepped_as$Celastrina_echo_as,
             aes(x = longitude, y = latitude))

map_L_carinenta_iNat_AZ <- get_map(c(left = -115, right = -105, bottom = 28, top = 38)) %>%
  ggmap() +
  geom_point(data = prepped_iNat$Libytheana_carinenta_iNat,
             aes(x = longitude, y = latitude))
map_L_carinenta_as_AZ <- get_map(c(left = -115, right = -105, bottom = 28, top = 38)) %>%
  ggmap() +
  geom_point(data = prepped_as$Libytheana_carinenta_as,
             aes(x = longitude, y = latitude))

map_V_cardui_iNat_AZ <- get_map(c(left = -115, right = -105, bottom = 28, top = 38)) %>%
  ggmap() +
  geom_point(data = prepped_iNat$Vanessa_cardui_iNat,
             aes(x = longitude, y = latitude))
map_V_cardui_as_AZ <- get_map(c(left = -115, right = -105, bottom = 28, top = 38)) %>%
  ggmap() +
  geom_point(data = prepped_as$Vanessa_cardui_as,
             aes(x = longitude, y = latitude))

# Block CV ----------------------------------------------------------------
#' Running blockCV with a preset config for this project
#'
#' @param prepped_data The prepped spatial points dataframe created by 
#' \code{link{prep_data}}
#' @param bv_raster the cropped raster associated with the same time period 
#' as the prepped_data above. 
#' @param block_size block size in m^2. Default = 400000
#' @return a blockCV object that we will use in later analysis
#'
#' @examples
run_block_cv = function(prepped_data, bv_raster, block_size = 400000){
  
  blocked = spatialBlock(speciesData = prepped_data,
                         species = "Species",
                         rasterLayer = bv_raster,
                         theRange = block_size,
                         k = 5, 
                         selection = "random", 
                         iteration = 250, 
                         biomod2Format = TRUE, 
                         xOffset = 0, 
                         yOffset = 0, 
                         progress = T, 
                         showBlocks = F
  )
  return(blocked)
}

# Run run_block_cv iNat -----------------------------------
  # recommended range
spatialAutoRange(rasterLayer = G_patrobas_iNat$env_data[[2]],
                 sampleNumber = 5000,
                 doParallel = TRUE,
                 showPlots = TRUE)
  #recommeded range based on rangeExplorer
rangeExplorer(rasterLayer = G_patrobas_iNat$env_data[[2]],
              speciesData = G_patrobas_iNat$data[[2]],
              species = "Species",
              minRange = 1000,
              maxRange = 200000)
  # run the function
G_patrobas_iNat_blocked <- run_block_cv(prepped_data = G_patrobas_iNat$data[[2]], 
             bv_raster = G_patrobas_iNat$env_data[[2]],
             block_size = 25000) #determined by decreasing until all blocks nonzero

saveRDS(G_patrobas_iNat_blocked, "data/G_patrobas_iNat_blocked.rds")

# Preparing data 2 ----------------------------------------------------------
#' More data preparation prior to SDM building
#'
#' @param data Prepped spatial points datframe created by \code{link{prep_data}}
#' NOTE: must index an individual item in the list e.g. data$data[[2]], data$env_data[[2]]
#' @param env_raster the cropped raster associated with the same time period 
#' as the prepped_data above.
#'
#' @return a dataframe with extracted environmental variables along with presence 
#' for all of the occurence and background data
#'
#' @examples
prep_data_2 = function(data, env_raster){
  extra_prepped = raster::extract(env_raster, data, df = TRUE) %>%
    bind_cols(as.data.frame(data)) %>%
    drop_na() %>%
    dplyr::select(-ID, Species, longitude, latitude, Bio1:Bio19)
  return(extra_prepped)
}

#' Run prep_data2 function with iNat data --------------------------------

G_patrobas_iNat2 <- prep_data_2(G_patrobas_iNat$data[[2]], G_patrobas_iNat$env_data[[2]])
write.csv(G_patrobas_iNat2, "data/G_patrobas_iNat2.csv")

# Train and test split ---------------------------------------

train_test_split = function(extra_prepped_data, blocked_obj){
  
  extract_index = function(list_of_folds = NULL) {
    for(k in 1:length(list_of_folds)){
      train_index <- unlist(list_of_folds[[k]][1]) # extract the training set indices
      test_index <- unlist(list_of_folds[[k]][2])# extract the test set indices
    }
    mini_list = list(train_index, test_index)
    return(mini_list)
  }
  
  indices = extract_index(blocked_obj$folds)
  print(length(indices[[1]]))
  print(length(indices[[2]]))
  
  #applying indexes and splitting data
  training_data = extra_prepped_data[indices[[1]],]
  test_data = extra_prepped_data[-indices[[2]],]
  
  return(list(training_data = training_data, 
              test_data = test_data))
}

#' Run train_test_split function with iNat data -----------------------
G_patrobas_iNat2_split <- train_test_split(G_patrobas_iNat2, G_patrobas_iNat_blocked)

saveRDS(G_patrobas_iNat2_split, "data/G_patrobas_iNat2_split.rds")

# Modeling ----------------------------------------------------------------

model_func = function(data = NULL, env_raster, num_cores) {
  data_occ = data %>%  #Generating occurence lat long
    filter(Species == 1) %>%
    dplyr::select(longitude, latitude) %>%
    drop_na()
  
  data_bg = data %>% #Generating background lat long
    filter(Species == 0) %>%
    dplyr::select(longitude, latitude) %>%
    drop_na()
  
  #Running the model
  eval = ENMevaluate(occ = data_occ, 
                     bg.coords = data_bg,
                     env = env_raster,
                     method = 'randomkfold', 
                     kfolds = 5, 
                     parallel = TRUE,
                     numCores = num_cores,
                     algorithm = 'maxent.jar')
  return(eval)
}

#' Run model_func function with iNat data -----------------------
G_patrobas_iNat_model <- 
  model_func(G_patrobas_iNat2_split$training_data, G_patrobas_iNat$env_data[[2]], 6)

  # save model as .rds
saveRDS(G_patrobas_iNat_model, file = "data/G_patrobas_inat_model.rds")

G_patrobas_iNat_model <- readRDS("data/G_patrobas_inat_model.rds")

  # Evaluation plots ---------------------------------------------------------

eval_plots = function(eval_object = NULL) {
  par(mfrow=c(2,3))
  eval.plot(eval_object@results)
  eval.plot(eval_object@results, 'avg.test.AUC', legend = F)
  eval.plot(eval_object@results, 'avg.diff.AUC', legend = F)
  eval.plot(eval_object@results, 'avg.test.or10pct', legend = F)
  eval.plot(eval_object@results, 'avg.test.orMTP', legend = F)
  plot(eval_object@results$avg.test.AUC, eval_object@results$delta.AICc, bg=eval_object@results$features, pch=21, cex= eval_object@results$rm/2, xlab = "avg.test.AUC", ylab = 'delta.AICc', cex.lab = 1.5)
  legend("topright", legend=unique(eval_object@results$features), pt.bg=eval_object@results$features, pch=21)
  mtext("Circle size proportional to regularization multiplier value", cex = 0.6)
  
}

# Run eval_plots with G_patrobas
G_patrobas_iNat_eval <- eval_plots(G_patrobas_iNat_model)


# Model Selection ---------------------------------------------------------

best_mod = function(model_obj){
  best_index = as.numeric(row.names(model_obj@results[which(model_obj@results$avg.test.AUC== max(model_obj@results$avg.test.AUC)),]))[1]
  
  best_mod = model_obj@models[[best_index]]
  return(list(best_mod, best_index))
}

# Run best_mod() on G_patrobas

G_patrobas_iNat_best <- best_mod(G_patrobas_iNat_model)
saveRDS(G_patrobas_iNat_best, "data/G_patrobas_iNat_best.rds")

# Evaluating on test data -------------------------------------------------

evaluate_models = function(test_data, model, env_raster) {
  test_data_occ = test_data %>%
    filter(Species == 1) %>%
    dplyr::select(longitude, latitude)
  
  bg_data = test_data %>%
    filter(Species == 0) %>%
    dplyr::select(longitude, latitude)
  
  ev = evaluate(test_data_occ, a = bg_data, model = model, x = env_raster)
  return(ev)
}

# run evaluate_models ----

G_patrobas_iNat_eval <- evaluate_models(test_data = G_patrobas_iNat2_split$test_data, 
                                        model = G_patrobas_iNat_model@models[[14]], 
                                        env_raster = G_patrobas_iNat$env_data[[2]]) 
                                        #index only t2
saveRDS(G_patrobas_iNat_eval, "data/G_patrobas_iNat_eval.rds")

  # Building full models on all data ----------------------------------------

full_model = function(models_obj, best_model_index, full_data = NULL, env_raster) {
  auc_mod = models_obj@results[best_model_index,]
  FC_best = as.character(auc_mod$features[1])
  rm_best = auc_mod$rm
  
  
  maxent.args = ENMeval::make.args(RMvalues = rm_best, fc = FC_best)
  
  # re calculating environmental raster
  
  
  full_mod = maxent(env_raster, as.matrix(full_data[,1:2]), args = maxent.args[[1]])
  return(full_mod)
}


# run full_mod iNat -----------

G_patrobas_iNat_full <- full_model(models_obj = G_patrobas_iNat_model, 
                                   best_model_index = G_patrobas_iNat_best[[2]], 
                                   full_data = G_patrobas_iNat_model@occ.pts, 
                                   env_raster = G_patrobas_iNat$env_data[[2]]) #index t2
saveRDS(G_patrobas_iNat_full, "data/G_patrobas_iNat_full.rds")

# troubleshooting full_mod ----

auc_mod <- G_patrobas_iNat_model@results[14,]
FC_best = as.character(auc_mod$features[1])
rm_best = auc_mod$rm


maxent.args = ENMeval::make.args(RMvalues = rm_best, fc = FC_best)

full_mod = maxent(G_patrobas_iNat$env_data[[2]], 
                  as.matrix(G_patrobas_iNat_model@occ.pts[,1:2]),
                  args = maxent.args[[1]])


# Model Plots --------------------------------------------------
# variable contribution plot
G_patrobas_iNat_vars <- plot(G_patrobas_iNat_full)
  # comes out as a vector for some reason

# make predictions using model
# predict()
pred_Gpa_iNat <- predict(object = G_patrobas_iNat_full,
                         x = G_patrobas_iNat$env_data[[2]],
                         ext = G_patrobas_iNat$env_data[[2]]@extent,
                         args = 'outputformat=cloglog')
                          # cloglog

# make a spatial pixels dataframe from predictions
pred_Gpa_iNat <- as(pred_Gpa_iNat, 
                    "SpatialPixelsDataFrame")
pred_Gpa_iNat <- as.data.frame(pred_Gpa_iNat) %>%
  rename("value" = "layer")

# use geom_tile to plot predictions

# basic ggmap
map_Gpa_iNat <- get_map(c(left = -113, right = -107, bottom = 28, top = 36)) %>%
  ggmap() +
  geom_tile(data = pred_Gpa_iNat, 
            aes(x, y, fill=value, alpha = value)) +
  scale_fill_viridis_c(name = "Probability of Occurence") +
  guides(alpha = FALSE)
map_Gpa_iNat
  # To do: 
  # change underlying map to black-and-white
  # get_map(... color = "bw") has not worked
  # Threshold map
  