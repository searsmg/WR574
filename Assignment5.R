rm(list=ls())

library(ggplot2)
library(dplyr)
library(lubridate)
library(RColorBrewer)
library(scales)
library(plotly)

#set wd for saving plots
setwd("C:/Users/sears/Documents/4_Classes_FA20/WR 574/Assignments/Assignment 5/")

load("~/Repos/WR574/Kal_Correct.RData")

SNOTEL <- read.csv("C:/Users/sears/Documents/4_Classes_FA20/WR 574/Assignments/Assignment 5/SNOTEL_daily.csv")

#################################################################

#plot formatting

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
#Question 1 - this is very sloppy

PLOT = "Air Temp and Cumulative Snow"
q1 <- ggplot(Kal_Correct) + geom_line(aes(x=date.time, y=Precip_Snow_AirCum, colour="Temp"), size=1) + theme_classic() + geom_line(aes(x=date.time, y=AirTemp_C, colour="Temp"), size=1) + PlotTheme + labs(x="Water Year 2020", y="AirTemp (C)") + scale_y_continuous(sec.axis = sec_axis(~ . *3/300, name="Temp")) + scale_x_datetime(date_breaks = "1 month", labels = date_format("%b")) + scale_color_manual(values = c("Temp"= "blue", "Snow" = "green"))

ay <- list(
  tickfont = list(size=11.7),
  titlefont=list(size=14.6),
  overlaying = "y",
  nticks = 5,
  side = "right",
  title = "Second y axis"
)


ggplotly(q1) %>%
  add_lines(x=~date.time, y=~AirTemp_C, colors=NULL, yaxis="y2", 
            data=Kal_Correct, showlegend=FALSE, inherit=FALSE) %>%
  layout(yaxis2 = ay)

###################################################################

#Question 3 - Steven said to do Q3 before Q2 and it'd be easier




ggplot(SNOTEL) + geom_line(aes(x=Date, y=SWE_mm, colour="SWE"), group=1)+ geom_line(aes(x=Date, y=SnowDepth_mm, colour="Snow Depth"), group=1)+ geom_line(aes(x=Date, y=Precip_mm, colour="Precip"),group=1)
