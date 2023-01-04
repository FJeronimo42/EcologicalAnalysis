######################################
### landscape metrics calculations ###
######################################
rm(list = ls())

#### work directory ####
setwd("X:/RDirectory/Tests/LandscapeEcology")
getwd()

### creating directory folders
dir.create("Maps")
dir.create("MetricsList")
dir.create("MetricsRaster")
dir.create("MetricsTables")
dir.create("Models")
dir.create("Rasters")
dir.create("Vectors")
dir.create("Data")

#### load packages ####
pacman::p_load(landscapemetrics, landscapetools, raster, rgdal, sf, tidyverse, 
               tmap)

#### load raster ####
landsc.raster <- raster("Rasters/Mapbiomas-Parana-2020-UTMSIRGAS22S.tif") # load raster

check_landscape(landsc.raster) #check raster quality

str(landsc.raster) # check raster properties

landsc.raster # check raster properties


#### load sample unitis points ####
sample.points <- read.csv("Data/data.landscape.csv", sep="") %>% # load and cleaning data frame
  select("SU", "X", "Y") %>% 
  rename("Lat" = Y,
         "Lon" = X) %>%
  mutate(SU = str_remove(SU, "^SU")) %>%
  mutate_if(is.character, as.numeric) %>%
  as_tibble()

glimpse(sample.points) # check data frame

coordinates(sample.points) <- c("Lon", "Lat") # set coordinates 

proj4string(sample.points) <- CRS("+proj=longlat +datum=WGS84") # set CRS

sample.poiutm <- spTransform(sample.points, 
                             CRS("+proj=utm +zone=22 +south +datum=WGS84 
                                 +units=m +no_defs")) # reprojecting and transform points

sample.poiutm # check spatial points data frame

sample.unipoi <- st_as_sf(x = sample.poiutm, coords = c("x", "y"), 
                          crs = sample.poiutm) # converting into sf object

sample.unipoi # check sf points 



#### creating buffers ####
buffer <- st_buffer(x = sample.unipoi, dist = 2000)
buffer



#### geoprocessing ####
landsc.sample <- list() # create empity raster list

# crop raster with buffer mask
for(i in 1:15){
    print(paste0("Clipping Landscape ", i)) # information
    buffer_pa <- buffer %>% 
    filter(SU == i) # filter
    landsc.sample[[i]] <- landsc.raster %>% # crop e mask
    crop(buffer_pa) %>% 
    mask(buffer_pa)
}

landsc.sample # check landscape list

names(landsc.sample) <- c(paste0("Landscape_", 1:14), "Landscape_15") # rename landscape samples

names(landsc.sample) # check landscape names

# export landscapes
for(i in 1:15){
  print(paste0("Exporting ", names(landsc.sample)[i])) # information
  writeRaster(x = landsc.sample[[i]], # exportation
                      filename = paste0("./Rasters/", names(landsc.sample)[i]),
                      format = "GTiff",
                      options = c("COMPRESS=DEFLATE" , "TFW=TRUE"),
                      overwrite = TRUE)
}



#### landscape metrics computation ####
check_landscape(landsc.sample) # check clips quality

# class level metrics
lsm_class <- calculate_lsm(landscape = landsc.sample,
                           level = c("class"), 
                           edge_depth = 0, # depth edge cells
                           neighbourhood = 8, # neighbourhood cells
                           full_name = TRUE,
                           verbose = TRUE, 
                           progress = TRUE)

data.clas.2000m <- lsm_class %>% 
  rename("Radius"= id) %>%
  mutate(Radius = as.factor(Radius)) %>% 
  replace_na(list(Radius = '2000m'))

data.clas.2000m

write_csv(data.clas.2000m, "./MetricsTables/ClassMetrcis2000m.csv")

# landscape level metrics
lsm_land <- calculate_lsm(landscape = landsc.sample,
                          level = "land", 
                          edge_depth = 0, # depth edge cells
                          neighbourhood = 8, # neighbourhood cells
                          full_name = TRUE,
                          verbose = TRUE, 
                          progress = TRUE)

data.land.2000m <- lsm_class %>% 
  rename("Radius"= id) %>%
  mutate(Radius = as.factor(Radius)) %>% 
  replace_na(list(Radius = '2000m'))

data.land.2000m

write_csv(data.land.2000m, "./MetricsTables/LandMetrcis2000m.csv")





#### visualizing landscapes ####
# mapbiomas palette
mapbiomas <- c("1"  = "#129912", "3"  = "#006400", "4"  = "#00FF00", 
               "5"  = "#687537", "49" = "#6b9932", "10" = "#BBFCAC",
               "11" = "#45C2A5", "12" = "#B8AF4F", "32" = "#968C46",
               "29" = "#665A3A", "13" = "#F1C232", "14" = "#FFFFB2",
               "15" = "#FFD966", "18" = "#E974ED", "19" = "#D5A6BD",
               "39" = "#E075AD", "20" = "#C27BA0", "40" = "#982C9E",
               "41" = "#E787F8", "36" = "#F3B4F1", "46" = "#CCA0D4",
               "47" = "#D082DE", "48" = "#CD49E4", "9"  = "#AD4413", 
               "21" = "#FFF3BF", "22" = "#EA9999", "23" = "#DD7E6B",
               "24" = "#AA0000", "30" = "#AF2A2A", "25" = "#FF3D3D", 
               "26" = "#0000FF", "33" = "#0000FF", "31" = "#02106F",
               "27" = "#D5D5E5")

# landscape plots
land1 <- show_landscape(landsc.sample$Landscape_1, discrete = TRUE) +
  scale_fill_manual(values = mapbiomas) +
  labs(title = "Landscape SU01") +
  theme(legend.position = "none",
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12, angle = 90, hjust = .5))
land1

land2 <- show_landscape(landsc.sample$Landscape_2, discrete = TRUE) +
  scale_fill_manual(values = mapbiomas) +
  labs(title = "Landscape SU02") +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 5),
        axis.text.y = element_text(size = 5, angle = 90, hjust = .5))
land2

land3 <- show_landscape(landsc.sample$Landscape_3, discrete = TRUE) +
  scale_fill_manual(values = mapbiomas) +
  labs(title = "Landscape SU03") +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 5),
        axis.text.y = element_text(size = 5, angle = 90, hjust = .5))
land3

land4 <- show_landscape(landsc.sample$Landscape_4, discrete = TRUE) +
  scale_fill_manual(values = mapbiomas) +
  labs(title = "Landscape SU04") +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 5),
        axis.text.y = element_text(size = 5, angle = 90, hjust = .5))
land4

land5 <- show_landscape(landsc.sample$Landscape_5, discrete = TRUE) +
  scale_fill_manual(values = mapbiomas) +
  labs(title = "Landscape SU05") +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 5),
        axis.text.y = element_text(size = 5, angle = 90, hjust = .5))
land5

land6 <- show_landscape(landsc.sample$Landscape_6, discrete = TRUE) +
  scale_fill_manual(values = mapbiomas) +
  labs(title = "Landscape SU06") +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 5),
        axis.text.y = element_text(size = 5, angle = 90, hjust = .5))
land6

land7 <- show_landscape(landsc.sample$Landscape_7, discrete = TRUE) +
  scale_fill_manual(values = mapbiomas) +
  labs(title = "Landscape SU07") +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 5),
        axis.text.y = element_text(size = 5, angle = 90, hjust = .5))
land7

land8 <- show_landscape(landsc.sample$Landscape_8, discrete = TRUE) +
  scale_fill_manual(values = mapbiomas) +
  labs(title = "Landscape SU08") +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 5),
        axis.text.y = element_text(size = 5, angle = 90, hjust = .5))
land8

land9 <- show_landscape(landsc.sample$Landscape_9, discrete = TRUE) +
  scale_fill_manual(values = mapbiomas) +
  labs(title = "Landscape SU09") +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 5),
        axis.text.y = element_text(size = 5, angle = 90, hjust = .5))
land9

land10 <- show_landscape(landsc.sample$Landscape_10, discrete = TRUE) +
  scale_fill_manual(values = mapbiomas) +
  labs(title = "Landscape SU010") +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 5),
        axis.text.y = element_text(size = 5, angle = 90, hjust = .5))
land10

land11 <- show_landscape(landsc.sample$Landscape_11, discrete = TRUE) +
  scale_fill_manual(values = mapbiomas) +
  labs(title = "Landscape SU011") +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 5),
        axis.text.y = element_text(size = 5, angle = 90, hjust = .5))
land11

land12 <- show_landscape(landsc.sample$Landscape_12, discrete = TRUE) +
  scale_fill_manual(values = mapbiomas) +
  labs(title = "Landscape SU012") +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 5),
        axis.text.y = element_text(size = 5, angle = 90, hjust = .5))
land12

land13 <- show_landscape(landsc.sample$Landscape_13, discrete = TRUE) +
  scale_fill_manual(values = mapbiomas) +
  labs(title = "Landscape SU013") +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 5),
        axis.text.y = element_text(size = 5, angle = 90, hjust = .5))
land13

land14 <- show_landscape(landsc.sample$Landscape_14, discrete = TRUE) +
  scale_fill_manual(values = mapbiomas) +
  labs(title = "Landscape SU014") +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 5),
        axis.text.y = element_text(size = 5, angle = 90, hjust = .5))
land14

land15 <- show_landscape(landsc.sample$Landscape_15, discrete = TRUE) +
  scale_fill_manual(values = mapbiomas) +
  labs(title = "Landscape SU015") +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 5),
        axis.text.y = element_text(size = 5, angle = 90, hjust = .5))
land15