# Ryan Ruggiero

rm(list=ls(all=T)) # clear global env.
gc()

####################### Goal of Code #######################

# layout workflow for extracting predictor variables for regression that are
# derived from the DEM

# Average slope,
# Topographic complexity 
# (sd of elevation within watershed), 
# Watershed slope/relief ratio (m/km) (watershed elevation change/watershed length from outlet to highest point on perimeter), 
# Mean elevation (m). 
# I also want to add Topographic wetness index but that is a per-cell metric that I don't know what is the best way to aggregate to a watershed metric. 
# Therfore, the DEMs of each watershed will also be saved to a list.

####################### Load packages #######################

library(tidyverse)
library(sf)
library(sp)
# library(sfheaders)
# library(streamstats)
library(terra)
# library(ggmap)
library(FedData)
library(raster)
# library(landscapemetrics)
library(whitebox)
# library(tmap)

####################### funcitons and variables needed below #######################

# Reassign NLCD legend to less classes

legend<-pal_nlcd()%>%
  mutate(ID2=c(1,2,3,3,3,3,2,4,4,4,4,4,5,5,2,2,6,7,8,8))

legend_2<-data.frame(ID2 = unique(legend$ID2))%>%
  mutate(Class2 = c("Open Water", "Other", "Developed", "Forest", "Grassland", "Pasture/Hay", "Cultivated Crops", "Wetlands"))

legend<-left_join(legend, legend_2, by = 'ID2')

cbind.fill<-function(...){
  nm <- list(...) 
  nm<-lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow)) 
  do.call(cbind, lapply(nm, function (x) 
    rbind(x, matrix(, n-nrow(x), ncol(x))))) 
}

# create a relcassificaiton matrix for applying the reclassification to the raster (and not just df!)

reclass_m <- as.matrix(legend[,c(1,5)])

# create a reclassification matrix for getting the reclassifed names

t<-legend[,c(5,6)]%>%distinct(ID2, .keep_all = T)

reclass_m_names<-as.matrix(legend[,c(5,6)])

####################### Workflow to process DEM data #######################

############ read in data ############

load("C:/PhD/Research/TP_Regression_data/df.Rdata")

load("C:/PhD/Research/TP_Regression_data/NLCD_list.Rdata")

############ workflow for for loop ############

i<-17

plain_df<-st_set_geometry(df, NULL) # just to look at df without having to wait for geometry column to load

# add columns to df

plain_df<-plain_df%>%
  mutate(Avg_slope = NA, Topo_complex = NA, slope_relief_ratio = NA, Mean_elev = NA)

DEM_list<-list()

#### assign df values to loop variable ####

geom<-st_cast(st_as_sf(df$geometry[i]), "POLYGON")

site<-df$Site[i]

lc<-NLCD_list[[i]]

#### Get and process DEM ####

# download DEM

DEM <- get_ned(template = geom, label = site)

# change crs/projection to match NLCD (see why later)

DEM<-projectRaster(from = DEM, to = lc)

# clip to watershed boundary

geom<-as_Spatial(st_transform(geom, crs(DEM)))

DEM <- crop(DEM, extent(geom))
DEM <- mask(DEM, geom)

# save DEC to list for future use

DEM_list[[i]]<-DEM

# create a vector of the elevation data in the DEM for future use

el<-as.data.frame(DEM)%>%drop_na()%>%.$layer

#### Slope: Calculate average slope ####

slope = terrain(DEM, v = "slope")

avg_slope<-round(mean(as.data.frame(slope, xy = TRUE)%>%drop_na()%>%.$slope), 4)

#### Topographic complexity (sd of elevation) #### 

topo_complex<-round(sd(el),4)

#### Watershed relief ratio ####

# (watershed elevation change/watershed length from outlet to highest point on perimeter)

# Save DEM to local file for whitebox to work

DEM %>% rast %>% writeRaster('C:/PhD/Research/TP_Regression_data/my_rast.tif', overwrite = TRUE)

# fill and breach DEM

wbt_fill_single_cell_pits( dem = 'C:/PhD/Research/TP_Regression_data/my_rast.tif', output = 'C:/PhD/Research/TP_Regression_data/my_rast_filled.tif')

wbt_breach_depressions_least_cost(dem = 'C:/PhD/Research/TP_Regression_data/my_rast_filled.tif', output = 'C:/PhD/Research/TP_Regression_data/my_rast_filled_breached.tif', dist = 5)

# D8 flow accumulation

wbt_d8_flow_accumulation(input = 'C:/PhD/Research/TP_Regression_data/my_rast_filled_breached.tif', output = 'C:/PhD/Research/TP_Regression_data/my_rast_d8fa.tif', out_type = 'cells')

# now, we can import the flow accumulation raster and plot it. Due to massive variation in values (some cells have 10-100 cells draining into them, others at the bottom of the landscape have 100,000-200,000), this is hard to visualise. Rather, we plot log-transformed values:

# rast('C:/PhD/Research/TP_Regression_data/my_rast_d8fa.tif') %>% {. ->> my_rast_d8fa}

# tm_shape(my_rast_d8fa %>% log10)+
#   tm_raster(palette = 'viridis')

# extract streams

wbt_extract_streams(flow_accum = 'C:/PhD/Research/TP_Regression_data/my_rast_d8fa.tif', output = 'C:/PhD/Research/TP_Regression_data/raster_streams.tif', threshold = 500)

# import and inspect streams raster by plotting with DEM

rast('C:/PhD/Research/TP_Regression_data/raster_streams.tif') %>% {. ->> my_rast_streams}

# tm_shape(DEM)+ tm_raster(style = 'cont', palette = 'viridis')+ tm_shape(my_rast_streams)+ tm_raster(palette = c('white'), legend.show = FALSE)

# however, all this and we still dont have longest flow path. To get this, I am trying the d8 pointer>downslope flowpath length workflow

wbt_d8_pointer(dem = 'C:/PhD/Research/TP_Regression_data/my_rast.tif', output = 'C:/PhD/Research/TP_Regression_data/my_rast_d8_pointer.tif')

wbt_downslope_flowpath_length(d8_pntr = 'C:/PhD/Research/TP_Regression_data/my_rast_d8_pointer.tif', output = 'C:/PhD/Research/TP_Regression_data/my_rast_downslope_flpth_len.tif')

# read in newly created raster and plot with stream network

rast('C:/PhD/Research/TP_Regression_data/my_rast_downslope_flpth_len.tif') %>% {. ->> my_rast_downslope}
# 
# tm_shape(my_rast_downslope)+ tm_raster(style = 'cont', palette = 'viridis')+ tm_shape(my_rast_streams)+ tm_raster(palette = c('white'), legend.show = FALSE)

# Now I can find the max downslope stream length from raster values

max_stream_len_Ryan<-round(max(my_rast_downslope%>%as.data.frame(.)%>%drop_na()%>%.$my_rast_downslope_flpth_len),2)

# or I can use the main channel length that streamstats give me (and convert to meters from miles!!

max_stream_len_SS<-round(plain_df$LENGTH[i]*1609.34, 2)

# finally calculate the relief ratio by hand

relief_ratio<-round((max(el)-min(el))/max_stream_len_SS,4)

# # another option for stream length is as follows:
# 
# # vectorize the raster stream network 
# 
# wbt_raster_streams_to_vector(streams = 'C:/PhD/Research/TP_Regression_data/raster_streams.tif',
#                              d8_pntr = 'C:/PhD/Research/TP_Regression_data/my_rast_d8_pointer.tif',
#                              output = 'C:/PhD/Research/TP_Regression_data/vector_streams.shp')
# 
# # read in shapefile using sf. note that the projection is needed to be assigned
# # before using riverdist importing
# 
# t<-read_sf(dsn = 'C:/PhD/Research/TP_Regression_data', layer = "vector_streams")
# 
# # set the crs
# 
# st_crs(t)<-st_crs(lc)
# 
# # dissolve 
# 
# t<-st_union(t)
# 
# # export to shapefile that has crs!
# 
# st_write(t, "C:/PhD/Research/TP_Regression_data/vector_streams_projected.shp", append = FALSE)
# 
# # read in vector shapefile using riverdist package
# 
# MyRivernetwork <- line2network(path='C:/PhD/Research/TP_Regression_data', layer="vector_streams_projected")

# this package doesnt work...

#### mean elevation ####

mean_elev<-round(mean(el), 2)

#### zonal statistics ####

# I can calculate metrics on a per-landuse basis using zonal statistics

# not including in the workflow now because not in Jabbar and Grote

# however, here is an example of calculat\ting the average slope
# per land use using zonal statistics

# first need to reclassify the NLCD raster
  
# z_rast<-reclassify(lc, reclass_m)

# then just run terra::zonal !!

# zstats = zonal(x = slope, z = z_rast, fun = "mean")

#### append to df ####

plain_df$Avg_slope[i]<-avg_slope

plain_df$Topo_complex[i]<-topo_complex

plain_df$slope_relief_ratio[i]<-relief_ratio

plain_df$Mean_elev[i]<-mean_elev




