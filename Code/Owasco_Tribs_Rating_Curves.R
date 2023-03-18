# Ryan Ruggiero

rm(list=ls(all=T)) # clear global env.
gc()

####################### Goal of Code #######################

# Dana Hall Shared 2021 and 2022 spot flow measurment data
# that pairs with the Hyfi data. The stage-discharge realtionships 
# on each date is used to creatign a rating curve

####################### Load packages #######################

library(tidyverse)
library(lubridate)
library(ggpmisc)
library(ggpubr)
library(expss)

####################### Read in Data and Clean up #######################

# read in csv

df<-read.csv("C:/PhD/Owasco/Owasco/Discharge/Owasco_trib_rating_curves.csv")

# format columns

df<-df%>%mutate(Date = as.Date(Date, format = '%m/%d/%Y'))%>%mutate(Year = as.character(year(Date)), Stage = as.numeric(Stage_ft), Q = as.numeric(Q_ft3_s), ln_Stage = log(Stage), ln_Q = log(Q))

# remove zeros

df<-df%>%filter(Q!=0)

# calcualte summary stats
# number of sample dates per site

df %>% group_by(Site, Year) %>% summarise(count = length(unique(Date)))%>%arrange(Year)

####################### plot #######################

# remove veneese brook because of lack of data

df<-df%>%filter(Site != 'Veness Brook')

# duplicate data for a third Year 'all'

df<-df%>%mutate(Year = 'all')%>%rbind(.,df)

# plot

gg<- ggplot(data= df, aes(x= ln_Stage, y= ln_Q, color = Year)) +
  stat_poly_line() +
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "*`,`~")), 
               parse = TRUE,
               label.x.npc = "left",
               vstep = 0.05,
               size = 3) + # sets vertical spacing
  geom_point() +
  # geom_smooth(method="lm") +
  # scale_x_continuous(trans = 'log') +
  # scale_y_continuous(trans = 'log')+
  facet_wrap(~ Site, ncol= 3, scales = "free")


gg



####################### Rating Curve Construction #######################

################### 2021 #######################








