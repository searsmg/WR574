#Assignment 7 - Redistribution based on Kalispell, MT ASOS station data

library(ggplot2)
library(dplyr)
library(lubridate)
library(RColorBrewer)
library(scales)

rm(list=ls())

load("~/Repos/WR574/Assignment4data.RData")

setwd("C:/Users/sears/Documents/4_Classes_FA20/WR 574/Assignments/Assignment 7")

#clean up Kal_Correct for this assignment
Kal7 <- Kal_Correct %>%
  select(date.time, AirTemp_C, Wind_ms, PrecipCorr, PrecipCorr_cumsum, Precip_Snow_Dew, Precip_Snow_DewCum)

#remove all other dfs that were associated with Assignment4data Rdata file since those won't be used
rm(list=setdiff(ls(), "Kal7"))

###############################################################
###############################################################

#Plot Format

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

###############################################################
###############################################################

#Question 1i - monthly frequency of blowing snow

#apply a lapse rate from 3 m to 1m since the U10 is measured at 10m
Kal7 <- Kal7 %>%
  mutate(AirTlapse = AirTemp_C - 0.0455)

#first, find the U10 using the empericl model (eq 8-2) and 3 conditions
Kal7 <- Kal7 %>%
  mutate(U10case = case_when(
    between(AirTlapse, 0.1, 4.999) ~ "Wet",
    between(AirTlapse, 5, 1000) ~ "Melting",
    between(AirTlapse, -100, 0) ~ "Dry"))
           
Kal7$U10 <- recode(Kal7$U10case, "Wet" = 9.9, "Melting" = 0, "Dry" = (9.43 + (0.18*Kal7$AirTlapse) + (0.0033* (Kal7$AirTlapse^2))))         
  
# find how often the wind speed goes above U10
Kal7 <- Kal7 %>%
  mutate(U10exceed = if_else(Wind_ms > U10 & Precip_Snow_Dew >0, 1, 0)) 

#%>%
 # mutate(U10exceedsnw = if_else(Precip_Snow_Dew > 0, U10exceed, 0))

#get freq value
KalSnowBlow_Freq <- Kal7 %>%
  mutate(month = month(date.time)) %>%
  group_by(month, U10exceed) %>%
  summarize(n=n()) %>%
  mutate(freq = n / sum(n))

#get freq value for ONLY when its exceeding (ie, snow is present and blowing)
KalMo_snowblowfreq <- KalSnowBlow_Freq %>%
  filter(U10exceed > 0)

#factor months so they're in the same order as WYs
KalMo_snowblowfreq$month <- factor(KalMo_snowblowfreq$month, levels=c(9, 10, 11, 12, 1, 2, 3, 4, 5, 6, 7, 8))

PLOT = "Monthly Freq of Snow Blowing"
custombreaks2 <- seq(0, .1, .01)
ggplot(KalMo_snowblowfreq) + geom_col(aes(x=month, y=freq), group=1) + PlotFormat + scale_x_discrete(labels=MonthLabels) + labs(x="Water Year 2020", y="Frequency of blowing snow") + scale_y_continuous(breaks = custombreaks2, labels = every_nth(custombreaks2, 2, inverse=TRUE))

ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight)

###############################################################
###############################################################

#Question 1ii
Kal7 <- Kal7 %>%
  mutate(SnowRedis_mm = if_else(U10exceed == 1, ((0.0000022*(U10^4.04))*3.6), 0)) %>%
  mutate(SnowRedis_cum_mm = cumsum(SnowRedis_mm))
  
PLOT="Cumulative Snow Redis Loss"
breaks <- seq(0,7, 0.5)
ggplot(Kal7) + geom_line(aes(x=date.time, y=SnowRedis_cum_mm), size=1) + PlotFormat + labs(y="Snow Redistribution Loss (mm)", x="Date") + scale_y_continuous(breaks = breaks, labels = every_nth(breaks, 2, inverse=TRUE)) + scale_x_datetime(date_breaks = "2 month", labels = date_format("%b %Y")) 
  
ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight)
