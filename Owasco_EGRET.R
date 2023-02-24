# Ryan Ruggiero

rm(list=ls(all=T)) # clear global env.
gc()

####################### Goal of Code #######################

# USGS Gauge at Owasoc inlet is scaled using drainage area method to Sucker Brook
# Sucker Brook Water quality Data comes from DEC (2017,2018) and Dana Hall (2021, 2022)
# DEC WQ data site # for sucker brook: 07-SCKR-0.1
# Dana Hall WQ data site number for sucker brook: OWLA-101
# WQ data was combined into EGRET ready dataframe in another code and output saved to csv (for EGRET requirments)

####################### Load packages #######################

library(dataRetrieval)
library(EGRET)
library(tidyverse)
library(sf)
library(mapview)
library(fuzzyjoin)
library(maps)
library(lfstat)

####################### Functions #######################

# setwd("C:/PhD/Research")
# source('Ryan_Functions.R')

####################### A summary of EGRET #######################

# The focus of EGRET can best be described under the general 
# heading of exploratory data analysis (Tukey, 1977) as opposed 
# to statistical inference or hypothesis testing. Within that 
# overall framework, EGRET carries out three types of tasks.

# It presents annual and seasonal summaries of the behavior of
# concentration and flux over time and flow-normalized estimates 
# of concentration and flux that are designed to remove the influence
# of year-to-year variations in discharge and thus provide more
# insight on underlying changes in the behavior of the watershed.
# This component also provides a wide range of specialized graphics
# aimed at identifying potential problems of bias in flux estimates
# (Hirsch, 2014) and describing the nature of the changes that have
# taken place.

####################### Import Flow Data #######################

# using EGRET sample code, get Owasco Inlet mean daily discharge data into EGRET format
# Owasco Inlet does not have any data from 1969-2009, hence the start date used so reduce the size of the dataframe

siteNumber <- "04235299"
QParameterCd <- "00060"
StartDate <- "2009-04-22"
EndDate <- ""
Daily <- readNWISDaily(siteNumber, QParameterCd, StartDate, EndDate)

# summary(Daily)

# Note that the discharge values in the summary (and in Daily!) are automatically converted to cubic meter per second (m3/s)

# plot(Daily$DecYear,Daily$Q,log="y",type="l")

####################### Import WQ Data #######################

# import just one consituent for now at Sample dataframe

Sample<-readUserSample("C:/PhD/Owasco/Owasco_WQ_data/Dana_Hall", "SB_TP.csv", hasHeader = TRUE, separator = ",",verbose = TRUE, interactive = NULL)

####################### Import metadata #######################

INFO <- readNWISInfo(siteNumber="04235299",parameterCd = "", interactive = FALSE)

####################### Map flow and WQ sites #######################

# flow site lat long is in INFO

map_Q<-c('USGS_gauge_04235299', INFO$dec_lat_va[1], INFO$dec_long_va[1])

# WQ lat long is in DEC report table, which I copied to a CSV

map_WQ<-read.csv("C:/PhD/Owasco/Owasco_WQ_data/DEC/Final_Advanced_MP_External_102221_site_names.csv")%>%filter(Location.ID == '07-SCKR-0.1')
map_WQ<-map_WQ[,c(1,7,8)]

# combine dataframes and plot

rbind(map_WQ, map_Q)%>%
  as.data.frame(row.names = 1:nrow(.))%>%
  st_as_sf(.,coords=c('Longitude','Latitude'), crs = 4326)%>%
  mapview(., zcol = 'Location.ID', na.color = NA)

####################### Moving Discharge Data from the Daily Data Frame to the Sample #######################

# eList <- mergeReport(INFO,Daily,Sample) # not sure why we use this still

####################### Scale flow to WQ site #######################

# drainage area of sucker brook at OWLA-101 is 9.85 sqmi (source:stream stats)
# drainage area of USGS_gauge_04235299 is in INFO
# come up with a scaling factor for flow:

Q_sf<-9.85/INFO$drain_area_va[1]

# scale Daily dataframe

Daily_scaled<-Daily%>%mutate(Q=Q*Q_sf, LogQ = log(Q*Q_sf), Q7 = Q7*Q_sf, Q30 = Q30*Q_sf)

####################### Flow History Analysis #######################

# create elist object

eList<-as.egret(INFO,Daily_scaled,Sample)

# set the PA

# eList <- setPA(eList, paStart = 3, paLong = 6, window = 5) # call for PA that covers the months December through February and a half window width of 15 years

# Plotting the Results for a Single Discharge Statistic by Using plotFlowSingle

plotFlowSingle(eList, istat = 5, qUnit = 2) # The function call for annual 7-day minimum discharge, in units of m3/s

# the output from plotFlowSingle can be displayed by using the function printSeries.

Owasco_Series <- printSeries(eList, istat = 8, runoff = FALSE) # This command 1) creates a data frame, in this case called SpokaneSeries, that contains the tabular information, and 2) prints the information to the console

# In addition, the results of plotFlowSingle can also be expressed in terms of the amount of change estimated to have taken place between any two years in the smoothed time series. This is done with the function tableFlowChange. The function describes these changes between selected pairs of years in four different ways:
# 1. a change between the first and last year of the pair, expressed in the flow units selected,
# 2. a change between the first and last year of the pair, expressed as a percentage of the value in the first year
# 3. a slope between the first and last year of the pair, expressed in terms of the flow units per year
# 4. a slope between the first and last year of the pair, expressed as a percentage change per year (a percentage based on the value in the first year)

# example useage of tableFlowchange with yearppoints argument defined

yearPoints <- c(2010, 2020)
tableFlowChange(eList, istat = 8, qUnit = 2, yearPoints=NA) # csant use ablve year points because I guess the smoothing doesn't work so close the the start of the POR?

# indicate if changes are taking place in the overall variability of the daily discharge record, without regard to the question of whether the central tendency is changing over time. The function produces a graphic of the running standard deviation of the log of daily mean discharge
# Note that the "window" argument to plotSDLogQ specifies a nonweighted rectangular window over which the standard deviation is computed, in contrast to the "window" argument to setPA, which specifies a weighted smoothing window for computing other statistics
# this graph can be useful and simple way of providing empirical evidence for hypotheses exploring the idea that increasing urbanization or increasing greenhouse gas concentrations in the atmosphere are bringing about changes in hydrologic variability

plotSDLogQ(eList, window = 5) # need to use window argument because POR is too short for default of 15: Error in plotSDLogQ(eList) :  Adjust 'window' argument to a value less than: 13

# Creating Graphics for Plotting the Discharge Record by Using plotQTimeDaily
# plot only those periods in which discharge is higher than some threshold value.

plotQTimeDaily(eList, lwd = 1, qUnit = 1)
plotQTimeDaily(eList, qLower = 1000, lwd = 1)

# Creating Multipanel Graphics for Flow History

plotFour(eList, qUnit = 2, window = 21) # doesn;t plot the last pane because of issue demonstrated above with plotSDLogQ

plot15(eList, yearStart=2010,yearEnd=2022)

####################### Summarizing Water-Quality Data (without Using WRTDS) #######################

# The existence of long gaps (more than about two years) can be important to
# the interpretation of WRTDS results, and a specific function (blankTime) 
# is provided to deal with the issues of long data gaps. In cases where 
# there is little or no sampling during certain seasons (most commonly the 
# winter season), the user may want to restrict the WRTDS results that are
# presented by setting the PA to the sampled months (for example, by confining
# the results to April through November by using paStart = 4, paLong = 8)
# 
# The types of plots presented in this section may be highly useful for 
# confirming various findings in WRTDS results [due to simplicty]. Confirmation
# of findings by using much simpler graphical means can provide useful 
# confirmation of the WRTDS inferences and suggest further analysis that 
# should be considered.


















