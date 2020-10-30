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

#first, find the U10 using the empericl model (eq 8-2)
Kal7 <- Kal7 %>%
  mutate(U10 = (9.43 + (0.18*AirTemp_C) + (0.0033* (AirTemp_C^2))))

# find how often the wind speed goes above U10
Kal7 <- Kal7 %>%
  mutate(U10exceed = if_else(U10 > Wind_ms, 1, 0)) %>%
  mutate(U10exceedsnw = if_else(Precip_Snow_Dew > 0, U10exceed, 0))

#get freq value
KalSnowBlow_Freq <- Kal7 %>%
  mutate(month = month(date.time)) %>%
  group_by(month, U10exceedsnw) %>%
  summarize(n=n()) %>%
  mutate(freq = n / sum(n))

#get freq value for ONLY when its exceeding (ie, snow is present and blowing)
KalMo_snowblowfreq <- KalSnowBlow_Freq %>%
  filter(U10exceedsnw > 0)

#factor months so they're in the same order as WYs
KalMo_snowblowfreq$month <- factor(KalMo_snowblowfreq$month, levels=c(9, 10, 11, 12, 1, 2, 3, 4, 5, 6, 7, 8))

PLOT = "Monthly Freq of Snow Blowing"
custombreaks2 <- seq(0, 0.50, 0.1)
ggplot(KalMo_snowblowfreq) + geom_col(aes(x=month, y=freq), group=1) + PlotFormat + scale_x_discrete(labels=MonthLabels) + labs(x="Water Year 2020", y="Frequency of blowing snow") + scale_y_continuous(breaks = custombreaks2, labels = every_nth(custombreaks2, 2, inverse=TRUE))

ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight)

###############################################################
###############################################################

#Question 1ii


  

