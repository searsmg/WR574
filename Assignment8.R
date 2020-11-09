# WR574 Assignment 8 Sublimation 

library(ggplot2)
library(dplyr)
library(lubridate)
library(waldo)
libary(plyr)

rm(list=ls())

#load("~/Repos/WR574/Assignment4data.RData")

setwd("C:/Users/sears/Documents/4_Classes_FA20/WR 574/Assignments/Assignment 8/")

#making sure all the vars needed are here
#GetUz <- Kal_T %>%
#  select(date.time, Uz)

#GetRH <- Kal %>%
#  select(date.time, RH_.)

#rm(list= ls()[!(ls() %in% c('GetRH','GetUz', 'Kal_Correct'))])

#SeaLevelPress <- read.csv("C:/Users/sears/Documents/4_Classes_FA20/WR 574/Assignments/Assignment 8/SeaLevelPress.csv")%>%
#  mutate(date.time = mdy_hm(date.time))

#create Rdata file specific for this assignment
#save.image(file="Kal8.Rdata") #commented out now since the file is created

#Kal8 <- merge(Kal_Correct, GetRH, by="date.time")
#Kal8 <- merge(Kal8, GetUz, by="date.time")
#Kal8 <- merge(Kal8, SeaLevelPress, by="date.time")

#Kal8 <- Kal8 %>%
#  select(date.time, AirTemp_C, PrecipCorr, Precip_Snow_Dew, RH_., Uz, -SeaLevelPress_mB)

#save again - ready for calcs now
#save.image(file="Kal8.Rdata")

Kal8_new <- read.csv("C:/Users/sears/Documents/4_Classes_FA20/WR 574/Assignments/Assignment 8/Kal8_update.csv")%>%
  mutate(date.time = mdy_hm(date.time))

##################################################################
##################################################################

#Plot formatting

#X-axis labels for plots that are monthly
MonthLabels = c("Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug")

#Plot size
PlotWidth = 15
PlotHeight = 9

# use this plot format on remaining assignments - tweaked to remove grid lines and gray background
PlotFormat = theme(axis.text=element_text(size=20),
                   axis.title.x=element_text(size=24, hjust=0.5, margin=margin(t=20, r=20, b=20, l=20)),              
                   axis.title.y=element_text(size=24, vjust=0.5,  margin=margin(t=20, r=20, b=20, l=20)),              
                   plot.title=element_text(size=26,face="bold",hjust=0.5, margin=margin(t=20, r=20, b=20, l=20)),      
                   legend.title=element_blank(),                                                                    
                   legend.text=element_text(size=20),                                                                   
                   legend.position = "bottom", 
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"))


# a really long way to create minor tick axis tick marks
every_nth <- function(x, nth, empty = TRUE, inverse = FALSE) 
{
  if (!inverse) {
    if(empty) {
      x[1:nth == 1] <- ""
      x
    } else {
      x[1:nth != 1]
    }
  } else {
    if(empty) {
      x[1:nth != 1] <- ""
      x
    } else {
      x[1:nth == 1]
    }
  }
}

##################################################################
##################################################################

# Question 2 - snowpack sublimation

kconstant <- -11.3

#find K
Kal8_new <- Kal8_new %>%
  mutate(KalPress_mB = SeaLevelPress_mB*exp(-0.00012*908)) %>%
  mutate(AirDens = (0.34722*KalPress_mB)/(AirTemp_C + 273.15)) %>%
  mutate(esat = (6.112*exp((17.62*AirTemp_C)/(243.12+AirTemp_C)))) %>%
  mutate(ea = (RH_. * esat)/100) %>% 
  mutate(sublim = if_else(AirTemp_C <= 0, kconstant * (AirDens/KalPress_mB)*Uz*(ea-esat), 0)) %>%
  mutate(sublim_cum = cumsum(sublim))


