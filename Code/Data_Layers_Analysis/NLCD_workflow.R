# Ryan Ruggiero

rm(list=ls(all=T)) # clear global env.
gc()

####################### Goal of Code #######################

# workflow for getting percent land covers from NLCD
# the NLCD is also resampled down to 8 classes from a potential of 20
# note that the order of operaitons on watershed shapefile prior to clipping is important!!

####################### funcitons and variables needed below #######################

# Reassign NLCD legend to less classes

legend<-pal_nlcd()%>%
  mutate(ID2=c(1,2,3,3,3,3,2,4,4,4,4,4,5,5,2,2,6,7,8,8))

legend_2<-data.frame(ID2 = unique(legend$ID2))%>%
  mutate(Class2 = c("Open Water", "Other", "Developed", "Forest", "Grassland", "Pasture/Hay", "Cultivated Crops", "Wetlands"))

legend<-left_join(legend, legend_2, by = 'ID2')

####################### Load packages #######################

library(dataRetrieval)
library(elevatr)
library(FedData)
# library(EGRET)
library(sf)
library(sp)
library(mapview)
library(fuzzyjoin)
library(maps)
# library(lfstat)
library(CropScapeR)
library(raster)
library(tidyverse)
library(velox)
library(rnaturalearth)
library(rgeos)
library(stars)
library(terra)
library(rgdal)

####################### Workflow to process NLCD data #######################

############ read in data ############

load("C:/PhD/Research/TP_Regression_data/TP_sites_with_SS_attributes.Rdata")

df<-TP_sites_with_SS_attributes

rm(TP_sites_with_SS_attributes)

# set crs (this is the crs of the watershed forrm streamstats) and set as sf
df<-st_as_sf(df,sf_column_name = 'geometry', crs = 4326)

############ create variables from dataframe ############

i<-7

geom<-st_as_sf(df$geometry[i])

site<-df$Site[i]

geom<-st_cast(geom, 'POLYGON')

############ Get NLCD and clip to watershed boundary ############

lc<-get_nlcd(template = geom,label = site ,year = 2016)

geom<-st_transform(geom, crs(lc))

geom<-as_Spatial(geom)

lc <- crop(lc, extent(geom))
lc <- mask(lc, geom)

#### convert to dataframe, merge with reclassified legend, and calcualte %s ####

lc_df<-as.data.frame(lc, xy = TRUE)%>%
  drop_na()%>%
  rename(ID = NLCD.Land.Cover.Class)%>%
  left_join(.,legend[,c(1,2,5,6)], by = 'ID')%>%
  group_by(Class2)%>%
  summarise(n = n())%>%
  mutate(pland = round(n / sum(n), 4))





