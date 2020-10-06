#Assignment 4 for WR574 - Trace events, Precip undercatch, and probability

rm(list=ls())

library(ggplot2)
library(dplyr)
library(lubridate)
library(RColorBrewer)
library(scales)

#set wd for saving plots
setwd("C:/Users/sears/Documents/4_Classes_FA20/WR 574/Assignments/Assignment 4/")

#Kalispell ASOS station hourly data. Mutate date to work with in R 
Kal <- read.csv("C:/Users/sears/Documents/4_Classes_FA20/WR 574/Assignments/Assignment 3/Assignment3_KalASOS.csv")%>%
  mutate(date.time = mdy_hm(date.time))

Kal <-na.omit(Kal)

#no trace included
Kal_noT <- Kal[!(Kal$HourlyPrecip_in=="T"),]

#trace included
Kal_T <- read.csv("C:/Users/sears/Documents/4_Classes_FA20/WR 574/Assignments/Assignment 3/Kal_T.csv")%>%
  mutate(date.time = mdy_hm(date.time))

###########################################################################
########################PLOT FORMATTING###################################

#X-axis labels for monthly plots
MonthLabels = c("Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug")

#Height and width 
PlotWidth = 15
PlotHeight = 9

# dont use this plot theme in future bc it was tweaked #
PlotTheme = theme(axis.text=element_text(size=20),    #Text size for axis tick mark labels
                  axis.title.x=element_text(size=24, hjust=0.5, margin=margin(t=20, r=20, b=20, l=20)),               #Text size and alignment for x-axis label
                  axis.title.y=element_text(size=24, vjust=0.5,  margin=margin(t=20, r=20, b=20, l=20)),              #Text size and alignment for y-axis label
                  plot.title=element_text(size=26,face="bold",hjust=0.5, margin=margin(t=20, r=20, b=20, l=20)),      #Text size and alignment for plot title
                  legend.title=element_blank(),                                                                    #Text size of legend category labels
                  legend.text=element_text(size=20),                                                                   #Text size of legend title
                  legend.position = "bottom") 

# a really long and annoying way to create minor tick axis tick marks
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
#Question 1 - cum sum of precip w/o trace and w/trace

#cumsum WITHOUT TRACE
Kal_noT$HourlyPrecip_in <- as.numeric(as.character(Kal_noT$HourlyPrecip_in))
Kal_noT$Precip_mm <- Kal_noT$HourlyPrecip_in * 25.4

Kal_noT <- Kal_noT %>%
  mutate(precip_cum_mm = cumsum(Precip_mm))

#cumsum WITH TRACE
Kal_T <- Kal_T %>% 
  mutate(HourlyPrecip_in = ifelse(HourlyPrecip_in == 0.001, 0.005, HourlyPrecip_in))

Kal_T$Precip_mm <- Kal_T$HourlyPrecip_in *25.4

Kal_T <- Kal_T %>% 
  mutate(precip_cum_mm = cumsum(Precip_mm))

PLOT = "Cum Precip with Trace and without"
custombreaks1 <- seq(0, 800, 100)
ggplot() + geom_line(data = Kal_noT, aes(x=date.time, y=precip_cum_mm), size=1) + theme_classic() + geom_line(data= Kal_T, aes(x=date.time, y=precip_cum_mm), colour="blue", size=1) + PlotTheme + labs(x="Water Year 2020", y="Cumulative Hourly Precipitation (mm)") + scale_y_continuous(breaks = custombreaks1, labels = every_nth(custombreaks1, 2, inverse=TRUE)) + scale_x_datetime(date_breaks = "1 month", labels = date_format("%b"))

ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight)
