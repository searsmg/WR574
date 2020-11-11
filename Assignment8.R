# WR574 Assignment 8 Sublimation 

library(ggplot2)
library(dplyr)
library(lubridate)
library(waldo)
library(scales)

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

PLOT = "Hourly Cumulative Sublimation"
custombreaks2 <- seq(0, 70, 5)
ggplot(Kal8_new) + geom_line(aes(x=date.time, y=sublim_cum), group=1) + PlotFormat + scale_x_datetime(date_breaks = "2 month", labels = date_format("%b %Y")) + labs(x="Water Year 2020", y="Sublimation (mm)") + scale_y_continuous(breaks = custombreaks2, labels = every_nth(custombreaks2, 2, inverse=TRUE))

ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight)

##################################################################
##################################################################

#Question 3 - sublimation vs. blowing snow vs. undercatch

#get undercatch - this is from the Kal_T df in the Assignment4data Rdata file
Kalunder <- Kal_T %>%
  select(date.time, PrecipCorr, Precip_mm) %>%
  mutate(Undercatch_mm = (PrecipCorr-Precip_mm)) %>%
  mutate(Under_CumSum = cumsum(Undercatch_mm))

#get blowing snow cum sum
Kalblow <- Kalblow %>%
  mutate(Redis_Cum = cumsum(SnowRedis_mm))


#get blowing and redis amount - this is from assignment 7
Kalblow <- Kal7 %>%
  select(date.time, SnowRedis_mm)

Kal8SubBlow <- merge(Kalblow, Kal8_new, by="date.time")

Kal8SubBlow <- Kal8SubBlow %>%
  select(date.time, SnowRedis_mm, sublim) %>%
  mutate(BlowSub = SnowRedis_mm + sublim,
         BlowSub_cum = cumsum(BlowSub))


PLOT = "Snow Mass"
custombreaks2 <- seq(0, 120, 20)
ggplot() + geom_line(data=Kal8_new, aes(x=date.time, y=sublim_cum, colour="Sublimation"), group=1, size=1) + PlotFormat + scale_x_datetime(date_breaks = "2 month", labels = date_format("%b %Y")) + labs(x="Water Year 2020", y="Snow Mass (mm)") + scale_y_continuous(breaks = custombreaks2, labels = every_nth(custombreaks2, 2, inverse=TRUE)) + geom_line(data=Kalunder, aes(x=date.time, y=Under_CumSum, colour="Undercatch"), group=1, size=1) + geom_line(data=Kalblow, aes(x=date.time, y=Redis_Cum, colour="Blowing Snow"), group=1, size=1) + geom_line(data=Kal8SubBlow, aes(x=date.time, y=BlowSub_cum, colour="Sublimation + Blowing Snow"), group=1, size=1)

ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight)

#question 3ii - cumulative undercatch to sub + blowing snow
DoubleMass <- Kal8SubBlow

DoubleMass <- merge(DoubleMass, Kalunder, by="date.time")


PLOT="Double Mass Curve"
custombreaks3 <- seq(0,120, 20)
ggplot(DoubleMass) + geom_line(aes(x=BlowSub_cum, y=Under_CumSum), size=1) + PlotFormat + labs(x="Sublimation + Blowing Snow (mm)",y="Undercatch (mm)") + geom_abline(intercept=0, slope=1, colour="Red") + scale_y_continuous(breaks = custombreaks3, labels = every_nth(custombreaks3, 2, inverse=TRUE)) + expand_limits(x=125) + scale_x_continuous(breaks = custombreaks3, labels = every_nth(custombreaks3, 2, inverse=TRUE))

ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight)
