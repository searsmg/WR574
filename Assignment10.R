# WR574 Assignment 10 - Modeling snowmelt

library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)

rm(list=ls())

setwd("C:/Users/sears/Documents/4_Classes_FA20/WR 574/Assignments/Assignment 10/")

SNOTEL <- read.csv("C:/Users/sears/Documents/4_Classes_FA20/WR 574/Assignments/Assignment 6/SNOTEL.csv")%>%
  mutate(Date = ymd(Date))


##################################################################
##################################################################

#Plot formatting

#X-axis labels for plots that are monthly and in order for WY
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