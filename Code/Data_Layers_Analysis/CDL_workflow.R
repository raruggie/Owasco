# Ryan Ruggiero

rm(list=ls(all=T)) # clear global env.
gc()

####################### Goal of Code #######################

# template workflow for processing CDL for drainage area

# the goal is to get a cdl layer for each year there is TP data

####################### Load packages #######################

library(sf)
library(sp)
library(CropScapeR)
library(raster)
library(tidyverse)
library(terra)
library(rgdal)

####################### Workflow: guts of for loop  #######################

############ read in data to test workflow with ############

# load raw data (site + date): no watershed geometry.
# also want to add a year column for matching up later
# also want to add columns for manure crop types

load("C:/PhD/Research/TP_Regression_data/dfTP.Rdata")

dfTP$Year<-format(as.Date(dfTP$Date, format = '%Y-%m-%d'), '%Y')

types<-c('Corn', 'Soybeans', 'Other_Hay/Non_Alfalfa', 'Alfalfa')

dfTP[,types]<-NA

# load in post NLCD step (just site): has watersehd geometry

load("C:/PhD/Research/TP_Regression_data/df.Rdata")

# create a non-sf dataframe for easy viewing

plain_df<-st_set_geometry(df, NULL) # just to look at df without having to wait for geometry column to load

############ create variables from dataframe ############

i<-13

geom<-st_as_sf(df$geometry[i])

site<-df$Site[i]

geom<-st_cast(geom, 'POLYGON')

# also want the dates (i.e. years) of TP data associated with this site

years<-dfTP%>%
  filter(Site == site)

years<-unique(years$Year)

############ Get CDL and clip to watershed boundary ############

j<-3

for (j in seq(1:length(years))){
  
  # download a single year CDL
  
  CDL <- GetCDLData(aoi = geom, year = 2006, type = "b", tol_time = 100 )
  
  # mask out values not in drainage area
  # first change the CRS first to that of the raster data
  
  geom_CDL<-st_transform(geom, st_crs(CDL))
  
  CDL <- geom_CDL %>%
    #--- mask the values outside the sf (turn them into NA) ---#
    raster::mask(CDL, .) 
  
  # convert CDL data to dataframe, find shares, and filter based on
  # a set of assumed manured crop types inputted above
  
  crop_freq <- freq(CDL)%>% 
    data.frame(.) %>% 
    mutate(share = round(count/sum(count), 4))%>%
    dplyr::left_join(., linkdata, by = c('value' = 'MasterCat'))%>%
    filter(grepl(paste(types, collapse="|"), Crop))
  
  # append to dataframe
  
  for (k in seq(1,dim(dfTP)[1])){
    if (dfTP$Site[k] == site & dfTP$Year[k] == years[j]){
      dfTP$Corn[k]<-crop_freq$share[1]
      dfTP$Soybeans[k]<-crop_freq$share[2]
      dfTP$`Other_Hay/Non_Alfalfa`[k]<-crop_freq$share[3]
      dfTP$Alfalfa[k]<-crop_freq$share[4]
    }
  }
}







