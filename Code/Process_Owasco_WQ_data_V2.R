# Ryan Ruggiero

rm(list=ls(all=T)) # clear global env.
gc()

####################### Goal of code #######################

# Total P:

# Only the datasets approved for DEC analysis are imported here.
# The datasets are cleaned, merged, and exported in a single dataframe.
# Duplicate samples and site locations are determined and removed. 

####################### Load packages #######################

library(tidyverse)
library(sf)
library(mapview)
library(fuzzyjoin)
library(maps)
library(lfstat)
library(reshape2)

####################### DEC data #######################

DEC<-read.csv("C:/PhD/Owasco/Owasco/Owasco_WQ_data/DEC/Final_Advanced_MP_External_102221.csv")

DEC<-DEC[,c(1,2,4,22)]

names(DEC)<-c('Site', "Date", "Value", "Consit_name")

# there are 'O' instead of zeros in some of the site names. need to replace them
# approach: extract the first two characters from the string and replace 'O' with '0'

for (i in seq(1:dim(DEC)[1])){
  string<-DEC$Site[i]
  first_char<-substr(string, start = 1, stop = 1)
  string_end<-substr(string, start = 2, stop = nchar(string))
  if(first_char == 'O'){
    DEC$Site[i]<-paste0('0', string_end)
  }
}

sites<-read.csv("C:/PhD/Owasco/Owasco/Owasco_WQ_data/DEC/Final_Advanced_MP_External_102221_site_names.csv")

sites<-sites[,c(1,7,8)]%>%distinct(Location.ID, .keep_all = T)

DEC<-left_join(DEC,sites, by = c("Site" = "Location.ID"))%>%
  filter(grepl('phos', Consit_name, ignore.case = T))%>%
  filter(grepl('total', Consit_name, ignore.case = T))%>%
  drop_na()%>%
  sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

# import Owasco watershed boundary

OWS<-st_as_sf(st_read("C:/PhD/Owasco/Owasco/Owasco_WQ_data/Shapefiles/Owasco_Lake_DA_SS/globalwatershed.shp"))

# clip sites to Owasco watershed

DEC<-st_intersection(DEC, OWS)%>%
  st_set_geometry(., NULL)%>%
  select(!c(Name, Consit_name))%>%
  left_join(.,sites, by = c("Site" = "Location.ID"))%>%
  mutate(Database = 'DEC')

####################### Dana Hall Data: 2018, 2021, 2022 #######################

setwd("C:/PhD/Owasco/Owasco/Owasco_WQ_data/Dana_Hall")

# site info

DHD_sites<-read.csv('Site_names_and_coords_2022.csv')%>%
  drop_na(Lat)%>%
  select(c(1,4,6))%>%
  rename(Site = 1, Latitude = 2, Longitude = 3)

# WQ data

DHD_2018<-read.csv("C:/PhD/Owasco/Owasco/Owasco_WQ_data/Dana_Hall/Dana_Hall_Data_2018.csv")%>%select(c(1,3,8,4,5))%>%mutate(TP = as.numeric(TP)/1000, Client.ID = paste("OWLA", as.character(Client.ID)))%>%`colnames<-`(c("Site", "Date", "Value", "Latitude", "Longitude"))

DHD_2021<-read.csv('Dana_Hall_Data_2021.csv')[-1,]%>%filter(Station != 'Field Dup')%>%filter(Station != 'Field Blank')%>%filter(Station != 'Field Duplicate')%>%select(c(2,6,11))%>%mutate(TP = as.numeric(TP)/1000)%>%`colnames<-`(c("Site", "Date", "Value"))%>%left_join(.,DHD_sites, by = 'Site')

DHD_2022<-read.csv('Dana_Hall_Data_2022.csv')[-1,]%>%filter(Station != 'Field Dup')%>%filter(Station != 'Field Blank')%>%filter(Station != 'Field Duplicate')%>%select(c(2,6,11))%>%mutate(TP = as.numeric(TP)/1000)%>%`colnames<-`(c("Site", "Date", "Value"))%>%left_join(.,DHD_sites, by = 'Site')

# merge

DHD<-rbind(DHD_2018, DHD_2021, DHD_2022)%>%
  mutate(Database = 'Dana')

####################### DOW Data #######################

DOW<-read.csv("C:/PhD/Owasco/Owasco/Owasco_WQ_data/DOW/DOW_Owasco_P_data.csv")%>%select(c(1,3,5,8,9))%>%mutate(sample_date = gsub("(.*),.*", "\\1", sample_date))%>%`colnames<-`(c("Site", "Date", "Value", "Latitude", "Longitude"))%>%mutate(Database = 'DOW')

####################### Merge All Owasco Data #######################

Owasco_TP<-rbind(DEC, DHD, DOW)

####################### Duplicate Checks #######################

# a simple method for detemrining duplicate data is to combine date and 
# TP values. 

Owasco_TP<-Owasco_TP%>%mutate(ID = paste(Date, Value))

dx<-Owasco_TP

p<-dx[dx$ID %in% dx$ID[duplicated(dx$ID)],]

# clearly, these sampling dates are duplicated across the database. 

# Remove them

Owasco_TP<-Owasco_TP%>%distinct(ID, .keep_all = T)%>%mutate(Date = as.Date(Date, format = "%m/%d/%Y"))%>%select(-ID)

# remove any NAs

Owasco_TP<-Owasco_TP%>%drop_na()

#### write to csv ####

write.csv(Owasco_TP, "C:/PhD/Owasco/Owasco/Owasco_WQ_data/Tribs_final_dataframes/Owasco_TP_ALL_V2.csv", row.names = F)



