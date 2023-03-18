# Ryan Ruggiero

rm(list=ls(all=T)) # clear global env.
gc()

####################### Goal of Code #######################

# USGS Gauge at Owasoc inlet is scaled using drainage area method to Sucker Brook
# Sucker Brook Water quality Data comes from DEC (2017,2018), Dana Hall (2021, 2022), DOW ()
# DEC/DOW WQ data site # for Sucker Brook: 07-DUCH-0.3
# Dana Hall WQ data site number for Sucker Brook: OWLA-101
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

Sample<-readUserSample("C:/PhD/Owasco/Owasco/Owasco_WQ_data/Tribs_final_dataframes", "SB_TP.csv", hasHeader = TRUE, separator = ",",verbose = TRUE, interactive = NULL)

####################### Import metadata #######################

INFO <- readNWISInfo(siteNumber="04235299",parameterCd = "", interactive = F)
INFO$param.units = 'mg/L'
INFO$paramShortName = 'TP'

####################### Map flow and WQ sites #######################

# flow site lat long is in INFO

map_Q<-c('USGS_gauge_04235299', INFO$dec_lat_va[1], INFO$dec_long_va[1])

# WQ lat long is in DEC report table, which I copied to a CSV

map_WQ<-read.csv("C:/PhD/Owasco/Owasco/Owasco_WQ_data/DEC/Final_Advanced_MP_External_102221_site_names.csv")%>%filter(Location.ID == '07-SCKR-0.1')
map_WQ<-map_WQ[,c(1,7,8)]

# load watershed shapefiles

setwd("C:/PhD/Owasco/Owasco/Owasco_WQ_data/Shapefiles/")
USGS_DA<-st_read("USGS_gauge_04235299_DA_shapefile_SS/globalwatershed.shp")
USGS_DA$Name[1]<-'USGS_04235299'
WQ_DA<-st_read("OWLA-101_DA_shapefile_SS/globalwatershed.shp")
WQ_DA$Name[1]<-'OWLA-101'

# combine dataframes and plot

rbind(map_WQ, map_Q)%>%
  as.data.frame(row.names = 1:nrow(.))%>%
  st_as_sf(.,coords=c('Longitude','Latitude'), crs = 4326)%>%
  mapview(., zcol = 'Location.ID', na.color = NA)+mapview(USGS_DA)+mapview(WQ_DA)

####################### Scale flow to WQ site #######################

# drainage area of sucker brook at OWLA-101 is:

DA<-as.numeric(st_area(WQ_DA)*(1/(2.59*(10^6))))

# sqmi

# drainage area of USGS_gauge_04235299 is in INFO

# come up with a scaling factor for flow:

Q_sf<-DA/INFO$drain_area_va[1]

# scale Daily dataframe

Daily_scaled<-Daily%>%mutate(Q=Q*Q_sf, LogQ = log(Q*Q_sf), Q7 = Q7*Q_sf, Q30 = Q30*Q_sf)

# change INFO

INFO$station_nm<-"Sucker Brook (Flow Scaled)"
INFO$shortName<-"Sucker Brook (Flow Scaled)"
INFO$drain_area_va[1]<-DA

####################### Moving Discharge Data from the Daily Data Frame to the Sample #######################

eList <- mergeReport(INFO,Daily_scaled,Sample) # not sure why we use this still

####################### Flow History Analysis #######################

# create elist object

# eList<-as.egret(INFO,Daily_scaled,Sample)

# using as.egret does not add Q to the sample dataframe which is messing with the plotting funcitons in the next section. Thus, sticking with mergeReport elist for now

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
plotQTimeDaily(eList, qLower = 100, lwd = 1)

# Creating Multipanel Graphics for Flow History

plotFour(eList, qUnit = 2, window = 5)

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

# produces a time series graph of the constituent concentration values as a function of time

graphics.off()

plotConcTime(eList)

# for somereason this is not working. It works if eList is created using merge report

# range of discharge variables that are common and those that are extreme, for the record as a whole or for a particular part of the year.

flowDuration(eList, qUnit = 1)

# relation between discharge and concentration

plotConcQ(eList, logScale=TRUE)

# relation between discharge and flux

plotFluxQ(eList, fluxUnit=4)

# multi-plot function: plotConcQ, plotConcTime, boxConcMonth, and boxQTwice

multiPlotDataOverview(eList)

####################### Load Estimation from C-Q curve #######################

# Get Sample data with Q

samp1<-eList[[3]]

C<-samp1$ConcLow
Q<-samp1$Q

################### Power Function Regression #######################

# linear model in log space

CQ<-lm(log(C)~log(Q))

# plot model and assess viability

plot(Q, C)

plot(log(Q), log(C))

abline(CQ)

par(mfrow=c(2,2))
plot(CQ)

graphics.off()

# Get cefficents and Bias correction factor for model

CQ_sum<-summary(CQ)

Beta_0<-CQ_sum$coef[1,1]

Beta_1<-CQ_sum$coef[2,1]

BCF<-exp((sigma(CQ)^2)/2)

# Apply equation to a daily time series of concentration

df<-Daily_scaled[,c(1,2,8,11)]

df$ConcHAT<-BCF*exp(Beta_0+(Beta_1*df$LogQ))

# use daily conc and flow to get daily load
# flow units are m3/s, conc are mg/L
# 86.4 converts mg * m3 / L * s to kg/day

df$loadHAT<-df$Q*df$ConcHAT*86.4

# Determine average annual loads, lbs/year

AAL<-df%>%mutate(Year = as.numeric(format(Date, '%Y')))%>%group_by(Year)%>%summarise(CQ_Flux_lbs_yr = sum(loadHAT)*2.2)

####################### Summarizing Water-Quality Data Using WRTDS #######################

################### Using all Water Quality Data #######################

# The method requires the availability of measured concentrations of the constituent of interest and a complete record of
# daily mean discharge for some period of record. The method was designed for data sets with 200 or more measured concentration
# values that span a period of a decade or more.
# 
# However, testing has shown that in some cases, it can produce reliable
# estimates of mean concentrations or mean fluxes with data sets as small as about 60 samples spanning periods as short as a
# decade, but doing so requires special settings on some of the arguments of the modelEstimation function (described below).

# The method can be used for a variety of purposes including:
# 1. estimating long-term changes (trends) in average concentrations and average fluxes, both annually and for some selected PA;
# 2. estimating actual mean concentration or fluxes for specific years or specific PAs within the year;
# 3. estimating mean concentrations or mean fluxes over some specified period, such as a decade; and
# 4. for providing insights into the change in system behavior that may lead to a better understanding of the causative mechanism behind the trends that are observed.

# The WRTDS method creates a highly flexible statistical representation of the expected value of concentration for every day
# in the period of record and then uses that representation to produce four daily time series for the period of record. These are daily
# concentration, daily flux, flow-normalized daily concentration, and flow-normalized daily flux.

# For example, if we compare a sample value with q and T values of 3.0
# and 1995.0, respectively, to a grid point with q value of 3.8 and a T
# value of 1997.25, then the distance in log discharge would be 0.8, the
# distance in time would be 2.25 years, and the distance in season would
# be 0.25 years

# The half window widths (the h value in the formula for the weights) have 
# default values of 2 (in log discharge units), 7 years, and 0.5 years

# flow normalization: assumed flow pdf is stationary across all years.

# Running WRTDS: default values 

eList_WRTDS <- modelEstimation(eList)

# does not run because default for minimum number of uncensored observations is 50

eList_WRTDS <- modelEstimation(eList, minNumUncen = 30, minNumObs = 30)

# When the concentration record has a large data gap, the WRTDS estimates for the time of the data gap are likely to be highly unreliable. Because
# WRTDS makes no prior assumptions about the shape of the time trend, the computations can create large oscillations during long data gaps. These 
# are just numerical artifacts. A data gap of two years or less (regardless of the overall record length) is generally not a problem, but as gaps
# become longer, it may be prudent to use the blankTime function to eliminate the results for the gap period. The blankTime function should
# also be used if there is a period of a few years during which the sampling frequency is very low; for example, fewer than six observations per year. 
# If there is a long data gap or period of very sparse data, the modelEstimation step should be run as usual, followed by running of the 
# blankTime function. The blankTime function replaces all of the estimated values (yHat, SE, ConcDay, FluxDay, FNConc, FNFlux) in the Daily data 
# frame during the blank period with NA, the indicator for missing values. The user must specify the starting and ending dates of the gap. It may 
# be prudent to make the blank period a few months longer than the actual gap and to start and end it with the starting and ending dates of water
# years, if water years will be the basis for annual summary computations. Regardless of how the user sets the blank period, any water-quality data 
# that may exist during that period will be used in the estimation process to inform the model, but the model is not used to produce daily estimates 
# during this blank period because they are likely to be highly unreliable. The discharge data during this period are still used, along with the rest 
# of the discharge data, for making flow-normalized estimates. It is also possible to use blankTime more than once on a given data set if it contains
# multiple data gaps.

# given the gap between 2018 and 2021, I will use blanktime
# note this removes entires from the dataframes reuslting from setupYears and calculateMonthlyResults function

eList_WRTDS_bt <- blankTime(eList_WRTDS, startBlank = '2019-01-01', endBlank = '2021-09-01')
eList_WRTDS_bt <- blankTime(eList_WRTDS, startBlank = Daily$Date[1], endBlank = Sample$Date[1]-1)

# computing annual results using setupYears function

AR<-setupYears(eList_WRTDS$Daily, paLong = 12, paStart = 10)

# computing monthly results using calculateMonthlyResults function

MR<-calculateMonthlyResults(eList_WRTDS)

# Plotting Annual Results: plot the annual average concentration and 
# annual flow-normalized concentration. The annual average concentration 
# is displayed as individual points (plotted at the midpoint of the PA).
# The flow-normalized results are presented as a smooth curve (in green)
# even though they are computed for a single point in time for each year

# to consider the months of April, May, and June, the command would be:
# eList <- setPA(eList, paStart = 4, paLong = 3)

plotConcHist(eList_WRTDS, yearStart = 2017, yearEnd = 2022, concMax = NA)

plotFluxHist(eList_WRTDS, yearStart = 2017, yearEnd = 2022)

# printed table of some or all of the results at an annual time step.

resultsTable <- tableResults(eList_WRTDS)

# Computing and Displaying Tables of Change Over Time
# provides measures of change, in both flow-normalized concentrations and flow-normalized flux, between pairs of years selected by the user

# yearPoints <- c(2017,2021)
# tableChange(eList_WRTDS, fluxUnit=6, yearPoints)

# In addition to comparing rates of change over various time periods, another comparison particularly worthy of note is of changes 
# in flow-normalized concentration, in percent, to changes in flow-normalized flux, in percent, for the same time period. If the 
# nature of the change in the system were such that the trend in the log of concentration was the same across the full range of 
# discharges and the full range of seasons, then it is mathematically assured that the changes, in percent, for flow-normalized 
# concentration and flow-normalized flux would be equal to each other.

# no code given to address this proposition. 

# To facilitate rapid exploration of serious problems with WRTDS models, the EGRET software has a single function that produces a 
# set of eight diagnostic graphics on a single page. This function is fluxBiasMulti and it is described in more detail below. The 
# graphic it produces is designed to help the hydrologist quickly spot potential problems.

################### Excluding 2012 Water Quality Data #######################

# dont need this section for Sucker Brook but is in Dutch Hollow (orginal code for EGRET analysis)

####################### Combining C-Q and WRTDS Results #######################

df1<-resultsTable[,c(1,5,6)]%>%mutate(across(c(2:3), ~ . * ((10^6)*2.2)))%>%rename(WRTDS_Flux_lbs_yr = 2, WRDTS_FN_Flux_lbs_yr = 3)%>%
  left_join(.,AAL[-c(1,15),], by = 'Year')%>%mutate(across(4, round, 1))

# df2<-resultsTable_reduce[,c(1,5,6)]%>%mutate(across(c(2:3), ~ . * ((10^6)*2.2)))%>%rename(WRTDS_Flux_lbs_yr = 2, WRDTS_FN_Flux_lbs_yr = 3)%>%
#   left_join(.,AAL[-c(1,15),], by = 'Year')%>%mutate(across(4, round, 1))


setwd("C:/Users/ryrug/Downloads")
pdf("data_output.pdf", height=11, width=8.5)
gridExtra::grid.table(df1)
dev.off()








