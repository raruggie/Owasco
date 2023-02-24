# Ryan Ruggiero

rm(list=ls(all=T)) # clear global env.
gc()

####################### Goal of code #######################

# Two datasets are processed here:
#   
# Final_Advanced_MP_External_102221.csv is the data from the DEC and Final_Advanced_MP_External_102221_site_names.csv is the site information table in the DEC report
# 
# Dana_Hall.csv is the data Charley shared

####################### Load packages #######################

# library(dataRetrieval)
# library(EGRET)
library(tidyverse)
library(sf)
library(mapview)
library(fuzzyjoin)
library(maps)
library(lfstat)

####################### Functions #######################

# setwd("C:/PhD/Research")
# source('Ryan_Functions.R')

####################### DEC data #######################

# set wd to DEC data location

setwd("C:/PhD/Owasco/Owasco_WQ_data/DEC")

# read in site names data

site_names<-read.csv("Final_Advanced_MP_External_102221_site_names.csv")

# filter site names data for Owasco sites

site_names<-site_names%>%filter(Group == 'Owasco')

# plot Owasco sites

site_names%>%
  as.data.frame(row.names = 1:nrow(.))%>%
  st_as_sf(.,coords=c('Longitude','Latitude'), crs = 4326)%>%
  mapview(., zcol = 'Location.ID', na.color = NA)

# 07-OWLI-3.0 matches up with USGS gauge


# read in raw data

df<-read.csv("Final_Advanced_MP_External_102221.csv")

# check to see if Owasco Inlet site is in here

temp<-filter(df, CHS_EVENT_SMAS_HISTORY_ID == '07-OWLI-0.1')

# there are 'O' instead of zeros in some of the site names. need to replace them
# approach: extract the first two characters from the string and replace 'O' with '0'

for (i in seq(1:dim(df)[1])){
  string<-df$CHS_EVENT_SMAS_HISTORY_ID[i]
  first_char<-substr(string, start = 1, stop = 1)
  string_end<-substr(string, start = 2, stop = nchar(string))
  if(first_char == 'O'){
    df$CHS_EVENT_SMAS_HISTORY_ID[i]<-paste0('0', string_end)
  }
}

# filter Owasco data by Owasco sites

Ow<-df%>%filter(CHS_EVENT_SMAS_HISTORY_ID %in% site_names$Location.ID)

# note: there are 13 Owasco sites in the FL_adv_monitoring word doc table but only 9 were also in the raw data file
# confirmed looking out putput of following code:

# unique(site_names$Location.ID)
# unique(df$CHS_EVENT_SMAS_HISTORY_ID)
# unique(df$CHS_EVENT_SMAS_HISTORY_ID) %in% unique(site_names$Location.ID)
# unique(site_names$Location.ID) %in% unique(df$CHS_EVENT_SMAS_HISTORY_ID)

# add site names to Owasco raw data
# there exist a duplicate of 07-OWAL_T9-0.1 in site names which will mess with the left join

site_names_reduced<-site_names[-11,]%>%select(Location.ID, Stream)

Ow_names<-Ow%>%rename(Location.ID = CHS_EVENT_SMAS_HISTORY_ID)%>%left_join(.,site_names_reduced, by = 'Location.ID')%>%relocate(Stream, .after = Location.ID)

# first want to reduce the number of columns in the orginal dataframe

Ow_reduced<-Ow_names[,c(1,2,3,5,23,24,29)]

# format date column

Ow_reduced$CHS_EVENT_SMAS_SAMPLE_DATE<-as.Date(Ow_reduced$CHS_EVENT_SMAS_SAMPLE_DATE, format = '%m/%d/%Y')

# make sure all units are mg/L for EGRET

# sum(Ow_reduced$CHEM_PARAMETER_UNIT != 'mg/L') # good

####################### Dana Hall Data #######################

# set wd

setwd("C:/PhD/Owasco/Owasco_WQ_data/Dana_Hall")

# actual data
# need to adjust column names because there were two  rows for column headers in UFI data files

DHD_2021<-read.csv('Dana_Hall_Data_2021.csv')
colnames(DHD_2021) <- paste(sep = '_', colnames(DHD_2021), as.character(unlist(DHD_2021[1,])))
DHD_2021<-DHD_2021[-1,]

DHD_2022<-read.csv('Dana_Hall_Data_2022.csv')
colnames(DHD_2022) <- paste(sep = '_', colnames(DHD_2022), as.character(unlist(DHD_2022[1,])))
DHD_2022<-DHD_2022[-1,]

# clean up data for field dups and blanks
# add NA columns for tNH3 in 2021

DHD_2021<-DHD_2021%>%mutate(`tNH3_ (µgN/L)` = NA, `flags.4_(tNH3)` = NA, .after = `flags.3_(NOx)`)%>%filter(Station_Name != 'Field Dup')%>%filter(Station_Name != 'Field Blank')%>%filter(Station_Name != 'Field Duplicate')
names(DHD_2021)<-names(DHD_2022)

DHD_2021<-DHD_2021[,c(2,5,6,11,13,15,18,20,22,24)]

DHD_2022<-DHD_2022[,c(2,5,6,11,13,15,18,20,22,24)]%>%filter(Station_Name != 'Field Dup')%>%filter(Station_Name != 'Field Blank')%>%filter(Station_Name != 'Field Duplicate')

# sampling dates in DHD 2022 data have two digit years for the last part of the 2022 data

for (i in seq(1:dim(DHD_2022)[1])){
  month<-sapply(strsplit(DHD_2022$Sampling_Date, "/"), "[[", 1)[i]
  day<-sapply(strsplit(DHD_2022$Sampling_Date, "/"), "[[", 2)[i]
  year<-sapply(strsplit(DHD_2022$Sampling_Date, "/"), "[[", 3)[i]
  if (nchar(year)<4){
    DHD_2022$Sampling_Date[i]<-paste0(month,'/',day,'/', '2022')
  }
}

# merge them together

DHD_all<-DHD_2021%>%bind_rows(.,DHD_2022)

# site info

DHD_sites<-read.csv('Site_names_and_coords_2022.csv')

# for some reason this csv copied weird from the report table, so need to clean it up

DHD_sites<-DHD_sites[,-c(2,5,9)]%>%drop_na(Lat)

####################### Plot DEC and Dana Hall Data #######################

# plot Dana hall data and DEC data to see which sites have the most data

# format site names dataframe from DEC data section

site_names_1<-site_names%>%mutate(DataBase = 'DEC', description = paste(Stream,Description))%>%select(DataBase, Location.ID, description, Latitude, Longitude)

# format DHD site dataframe and rbind with DEC info

all_sites<-DHD_sites%>%mutate(DataBase = 'Dana_Hall', description = Site.Name)%>%rename(Latitude = Lat, Longitude = Long, Location.ID = OWLA.Site.Number)%>%select(DataBase, Location.ID, description, Latitude, Longitude)%>%bind_rows(.,site_names_1)

# make a map

all_sites%>%
  as.data.frame(row.names = 1:nrow(.))%>%
  st_as_sf(.,coords=c('Longitude','Latitude'), crs = 4326)%>%
  mapview(., zcol = 'DataBase', na.color = NA)

####################### Exporting the WQ Data #######################

# First paired DEC and Dana Hall site is 07-SCKR-0.1 and OWLA-101
# going to call this single WQ site Sucker Brook or SB
# and first going to start with TP
# NEED TO CONVERT DANA HALL DATA FROM ug/L TO mg/L!!!!!!!
# EGRET Sample dataframe format for external WQ data: Date, Remarks, value

DEC<-Ow_reduced%>%filter(Location.ID == '07-SCKR-0.1' & CHEM_PARAMETER_NAME == "PHOSPHORUS, TOTAL (AS P)")%>%mutate(Remarks = NA, Date = CHS_EVENT_SMAS_SAMPLE_DATE, Value = CHR_RESULT_VALUE)%>%select(c(9,8,10))

DHD<-DHD_all[,c(1:4)]%>%filter(Client.ID_ == "OWLA 101")%>%filter(Station_Name != 'Long Point')%>%mutate(Value = as.numeric(`TP_(µgP/L)`)*0.001, Remarks = NA, Date = as.Date(Sampling_Date, format = '%m/%d/%Y'))%>%select(c(7,6,5))

# bind them together

SB_TP<-bind_rows(DEC,DHD)

# write out to csv

setwd("C:/PhD/Owasco/Owasco_WQ_data/Dana_Hall")
write.csv(SB_TP, "SB_TP.csv", row.names = F)











# format dataframe for EGRET, split up by consitutent

Consit_list<-Owasco_WQ%>%
  select(CHEM_PARAMETER_NAME, CHS_EVENT_SMAS_SAMPLE_DATE, CHR_RESULT_VALUE)%>%
  mutate(Remarks = NA, .after = CHS_EVENT_SMAS_SAMPLE_DATE)%>%
  split(., f = .$CHEM_PARAMETER_NAME)%>%
  lapply(., function(x) { x["CHEM_PARAMETER_NAME"] <- NULL; x })

# write out to individual csv files

setwd("C:/PhD/Owasco/Owasco_inlet_WQ_data/DEC/07-OWLI-0.1")
sapply(names(Consit_list),function (x) write.csv(Consit_list[[x]], file=paste(x, "csv", sep="."), row.names = FALSE))



# break up data in list of dataframes based on unique sites


Sites_list<- split(Ow_reduced, f = Ow$Location.ID)












