# Ryan Ruggiero

rm(list=ls(all=T)) # clear global env.
gc()

####################### Goal of Code #######################

# Several data layers have been identified as potentially useful for us
# to explain variability in nutrient concentrations. These include:
# 1) Crop Data Layer (CDL)
# 2) National Land Cover Database (NLCD)
# 3) Digital Elevation Models (DEM)
# 4) Soil Survey Geographic Database (SSURGO)

# This code explores downloading, clipping, and summarizing these data layers
# in R. The goal is to use R packages that allow API access to the data layers,
# and R geospatial packages to perform clipping and spatial summary statistics.

# Owasco is used as a testing area since I have been working with the water 
# quality data there most recently

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

####################### CDL: CropScapeR #######################

# see: https://tmieno2.github.io/R-as-GIS-for-Economists/CropScapeR.html

# read in shapefile for AOI. Here I use the Dutch Hollow Watershed in Owasco Lake Watershed

DH_aoi<-st_as_sf(st_read("C:/PhD/Owasco/Owasco/Owasco_WQ_data/Shapefiles/OWLA-166_DA_shapefile_SS/globalwatershed.shp"))

# map

tmap::qtm(DH_aoi)

################### Download RAW CDL data raster #######################

# download the CDL layer. Note the function does not clip, rather only has bounding box capabilities 

DH_CDL_2010 <- GetCDLData(aoi = DH_aoi, year = "2010", type = "b", tol_time = 100 )
DH_CDL_2020 <- GetCDLData(aoi = DH_aoi, year = "2020", type = "b", tol_time = 100 )

# Note that the CDL data uses the Albers equal-area conic projection.

terra::crs(DH_CDL_2010) 

# If you do not want to have values outside of the sf object, you can use raster::mask() to turn them into NA as follows:
  
DH_CDL_2010 <- DH_aoi %>% 
  #--- change the CRS first to that of the raster data ---#
  st_transform(., projection(DH_CDL_2010)) %>%
  #--- mask the values outside the sf (turn them into NA) ---#
  raster::mask(DH_CDL_2010, .) 

DH_CDL_2020 <- DH_aoi %>% 
  #--- change the CRS first to that of the raster data ---#
  st_transform(., projection(DH_CDL_2020)) %>%
  #--- mask the values outside the sf (turn them into NA) ---#
  raster::mask(DH_CDL_2020, .)

# take a look at CDL data

tmap::qtm(DH_CDL_2010)
plot(DH_CDL_2010)

# use raster::freq() to get the frequency (the number of raster cells) of each land use type.

crop_freq_2010 <- freq(DH_CDL_2010)

# get shares column and transform to data frame

crop_data_2010 <-crop_freq_2010 %>% 
    #--- matrix to data.frame ---#
    data.frame(.) %>% 
    #--- find share ---#
    mutate(share = count/sum(count))

# can get the reference table using data(linkdata) from the CropScapeR package then merge

crop_data_2010 <- dplyr::left_join(crop_data_2010, linkdata, by = c('value' = 'MasterCat'))

# These points with NoData can be removed by using the filter function
# (not shown)

################### Download summarized CDL data #######################

# Instead of downloading the raw CDL data, CropScape provides an 
# option to download summarized CDL data of changes in crops
# between two years of interest

# E.g. How much land changed from corn to cotton between 2010 and 2020?

# the function is called GetCDLComp but it only works with a url
# of the compressed shapefile

# a workaround is to download the two raster files and use the manualrotate
# function to perform the change summary

# there is also a GetCDLStat function that estimates acres. But this is just
# to save time in performing the calculation yourself (as is the purpose of 
# GetCDLComp as well)

############### Cropping changes between two dates #######################

########### Using GetCDLComp #######################

# # GetCDLComp(): request data on land use changes
# # The GetCDLComp function allows users to request data on changes in land 
# # cover over time from the CDL. Specifically, this function returns acres
# # changed from one crop category to another crop category between two 
# # years for a user-defined AOI.
# 
# # test it out
# 
# data_change <- GetCDLComp(aoi = DH_aoi, year1 = 2016, year2 = 2018, type = 'b', crs =  "+init=epsg:4326")
# data_change <- GetCDLComp(aoi = as.vector(st_bbox(DH_aoi)), year1 = 2016, year2 = 2018, type = 'b', crs =  "+init=epsg:4326")
# 
# # does not work
# 
# # using a bigger aoi, all OWasco watershed
# 
# OW_aoi<-st_as_sf(st_read("C:/PhD/Owasco/Owasco/Owasco_WQ_data/Shapefiles/Owasco_Lake_DA_SS/globalwatershed.shp"))
# 
# data_change <- GetCDLComp(aoi =OW_aoi, year1 = 2016, year2 = 2018, type = 'b', crs =  "+init=epsg:4326")
# 
# # still does not work
# 
# # trying cayuga county
# 
# data <- GetCDLComp(aoi = '36011', year1 = 2017, year2 = 2018, type = 'f')

########### Using manualrotate #######################

comp<-manualrotate(DH_CDL_2010, DH_CDL_2020)

################### Summarizing CDL data #######################

# the manualrotate function appears to work well for determining 
# the changes of one crop to another over a period (has not been validated in any way yet however)

# the next step is to make sense of this. 
# one idea isto look at the entires with corn in the 'to' column 
# but obviously omitting the corn to corn (not sure why that is in there. but after looking at Bownes example code, it appears it is normal for it to be in the result)

# another idea is we can look at crops with no change
# the question is what are we trying to do? What are we trying to explain?

####################### National Elevation Dataset (NED): FedData #######################

# Get the NED (USA ONLY)
# Returns a raster

NED <- get_ned(
  template = DH_aoi,
  label = "DH"
)
# Plot with raster::plot
raster::plot(NED)

####################### Daymet: FedData #######################

# Get the DAYMET (North America only)
# Returns a raster

DAYMET <- get_daymet(
  template = FedData::meve,
  label = "DH",
  elements = c("prcp", "tmax"),
  years = 1980:1985
)

# Plot with raster::plot
raster::plot(DAYMET$tmax$X1985.10.23)

# this doesnt work
# the same error is given when trying the example from the FedData
# github tutorial

####################### GHCN precip: FedData #######################

# Get the daily GHCN data (GLOBAL)
# Returns a list: the first element is the spatial locations of stations,
# and the second is a list of the stations and their daily data

GHCN.prcp <- get_ghcn_daily(
  template = DH_aoi,
  label = "DH",
  elements = c("prcp")
)

# Plot the NED again

raster::plot(NED)

# Plot the spatial locations

sp::plot(GHCN.prcp$spatial,
         pch = 1,
         add = TRUE
)

# Add legend

legend("bottomleft",
       pch = 1,
       legend = "GHCN Precipitation Records",
       trace = T,
       cex = .5
)

####################### GHCN temperature: FedData #######################

# Elements for which you require the same data
# (i.e., minimum and maximum temperature for the same days)
# can be standardized using standardize==T

GHCN.temp <- get_ghcn_daily(
  template = DH_aoi,
  label = "DH",
  elements = c("tmin", "tmax"),
  years = 1980:1985,
  standardize = TRUE
)

# does not work
# same error is given when using example (i.e. FedData::meve) code

# Plot the NED again

raster::plot(NED)

# Plot the spatial locations

sp::plot(GHCN.temp$spatial,
         add = TRUE,
         pch = 1
)

# Add legend

legend("bottomleft",
       pch = 1,
       legend = "GHCN Precipitation Records",
       trace = T,
       cex = .5
)

####################### National Hydrography Dataset (NHD): FedData #######################

# Get the NHD (USA ONLY)

# DH_for_NHD<-st_as_sf(DH_aoi[,2])
class(DH_aoi) = c("sf", "tbl", "tbl_df", "data.frame")

get_nhd(
  template = DH_aoi,
  label = "DH_aoi"
) %>%
  plot_nhd(template = DH_aoi)

# this not working even though the class is the same 
# as FedData::meve

####################### SSURGO: FedData #######################

# Get the NRCS SSURGO data (USA ONLY)

SSURGO.DH <- get_ssurgo(
  template = DH_aoi,
  label = "DH"
)

# takes a long time

# Plot the NED again

raster::plot(NED)

# Plot the SSURGO mapunit polygons

plot(SSURGO.DH$spatial$geom,
     lwd = 0.1,
     add = TRUE
)

# save Rdata file

setwd("C:/PhD/Owasco/Owasco/Code/Data_Layers_Analysis")
save(list = ls(.GlobalEnv), file = "Data_layers.Rdata")

# load R data file

setwd("C:/PhD/Owasco/Owasco/Code/Data_Layers_Analysis")
load("Data_layers.Rdata")

#### Some shit ####

state = ne_states(iso_a2 = "US", returnclass = "sf") %>%    # pull admin. bounds. for US
  filter(iso_3166_2 == "US-MA") %>%                         # select Massachusetts
  st_transform(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80
+towgs84=0,0,0,0,0,0,0 +units=m +no_defs") # nlcd crs

pts = st_sample(state, size = 10, type = "regular")         # sample 10 points in polygon

# Plot!
plot(st_transform(pts, 4326), col="red", pch=20)
maps::map("state", add=T)

nlcd = get_nlcd(        # fn for pulling NLCD data
  template = state,     # polygon template for download
  label = "4pland",     # this is just a character string label
  year = 2011,          # specify the year (2016 should be available soon)
  force.redo = F
) 

# Plot!
plot(nlcd)
plot(pts, col="black", pch=20, cex=1.5, add=T)

nlcd.vx = velox(stack(nlcd))                                  # raster for velox
sp.buff = gBuffer(as(pts, 'Spatial'), width=1000, byid=TRUE)  # spatial buffer, radius in meters
buff.df = SpatialPolygonsDataFrame(
  sp.buff,                                         
  data.frame(id=1:length(sp.buff)),                 # set ids
  FALSE)                                            # df of buffers
ex.mat.vx = nlcd.vx$extract(buff.df, df=T)                    # extract buffers from velox raster
rm(nlcd.vx) # removing the velox raster can free up space

prop.lc = ex.mat.vx %>%
  setNames(c("ID", "lc_type")) %>%        # rename for ease
  group_by(ID, lc_type) %>%               # group by point (ID) and lc class 
  summarise(n = n()) %>%                  # count the number of occurences of each class
  mutate(pland = n / sum(n)) %>%          # calculate percentage
  ungroup() %>%                           # convert back to original form
  dplyr::select(ID, lc_type, pland) %>%   # keep only these vars
  complete(ID, nesting(lc_type), 
           fill = list(pland = 0)) %>%             # fill in implicit landcover 0s
  spread(lc_type, pland)   

nlcd_cover_df = as.data.frame(nlcd@data@attributes[[1]]) %>%      # reference the name attributes
  subset(NLCD.2011.Land.Cover.Class != "") %>%                    # only those that are named
  dplyr::select(ID, NLCD.2011.Land.Cover.Class)                   # keep only the ID and the lc class name
lc.col.nms = data.frame(ID = as.numeric(colnames(prop.lc[-1])))   # select landcover classes
matcher = merge.data.frame(x = lc.col.nms,                        # analogous to VLOOKUP in Excel
                           y = nlcd_cover_df,
                           by.x = "ID",
                           all.x = T)               
colnames(prop.lc) = c("ID", as.character(matcher[,2]))


