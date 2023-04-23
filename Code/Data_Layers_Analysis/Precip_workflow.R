# Ryan Ruggiero

rm(list=ls(all=T)) # clear global env.
gc()

####################### Goal of Code #######################

# template workflow for getting precipitation data for a drainage area

####################### Load packages #######################

library(tidyverse)
library(sf)
library(sp)
library(rnoaa)

############ read in data to test workflow with ############

# load raw data (site + date): no watershed geometry. I am at the step
# where I processed the CDL site-data data into the dfTP dataframe

load("C:/PhD/Research/TP_Regression_data/dfTP_CDL.Rdata") # the dataframe is called dfTP!!

dfTP$Date<-as.Date(dfTP$Date, format = '%Y-%m-%d')

dfTP$Year<-format(dfTP$Date, '%Y')

dfTP$two_week_rain<-NA

load("C:/PhD/Research/TP_Regression_data/df.Rdata")

############ guts of the workflow for loop ############

i<-1

#### Step 1 - assign df values to loop variable ####

geom<-st_cast(st_as_sf(df$geometry[i]), "POLYGON") # this isn't working in the rnoaa workflow for some reason (including the additional st commands like centroid. not sure why)

geom_2<-st_as_sf(df$geometry[i]) # this one works in st_intersect 

geom_centroid<-geom_2%>%st_centroid() # use this later

site<-df$Site[i]

## find the dates associated with the site

dates<-dfTP%>%filter(Site==site)%>%.$Date

years<-as.numeric(unique(dfTP%>%filter(Site==site)%>%.$Year))

#### Step 2 - find closest ghcnd stations ####

# create a dataframe of watershed centroid

test <- geom_centroid %>%
  mutate(longitude = unlist(map(geom_centroid$x,1)),
         latitude = unlist(map(geom_centroid$x,2)),
         id = site)%>%
  st_set_geometry(., NULL)%>%
  dplyr::select(id, latitude, longitude)

# use rnoaa function to find 10 closest stations

mon_near<- meteo_nearby_stations(
  lat_lon_df = test,
  year_min = min(years),
  year_max = max(years),
  var = "PRCP",
  limit = 10
  )

# make a map

mon_near[[1]]%>%
  as.data.frame(row.names = 1:nrow(.))%>%
  st_as_sf(.,coords=c('longitude','latitude'), crs = 4326)%>%
  mapview::mapview(., zcol = 'name', na.color = NA)

plain_df%>%
  st_as_sf(.,coords=c('Long','Lat'), crs = 4326)%>%
  mapview::mapview(., zcol = 'Site', na.color = NA)


#### Step 3 - pull climate data at these sites ####

# using the site closest to the centroid:
# for each site-date combination, pull the precip data in the two weeks prior

j<-1

for (j in seq(1:length(dates))){
  
  # get precip data for two weeks prior
  
  prcp_date_data <- meteo_pull_monitors(
    monitors = mon_near[[1]]$id[1],
    date_min = as.character(dates[j]-14),
    date_max = as.character(dates[j]),
    var = "PRCP"
  ) # units of 10th of mm
  
  # calcualte two week prior rainfall in mm
  
  two_week_prcp<-sum(prcp_date_data$prcp)/10 # units of mm
  
  # append to dataframe
  
  for (k in seq(1,dim(dfTP)[1])){
    if (dfTP$Site[k] == site & dfTP$Date[k] == dates[j]){
      dfTP$two_week_rain[k]<-two_week_prcp
    }
  }
}










