# Assignment 1 

library(ggplot2)
library(dplyr)
library(scales)
library(Hmisc)
library(lubridate)
library(openair)

setwd("C:/Users/sears/Documents/4_Classes_FA20/WR 574/Assignments/Assignment 1/")

#Kalispell, MT monthly stats - Monthly Stats csv from ASOS station
MonthlyStats <- read.csv("C:/Users/sears/Documents/4_Classes_FA20/WR 574/Assignments/Assignment 1/Kal_MonthlyStats.csv", header=TRUE)

#X-axis labels for monthly plots
MonthLabels = c("Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug")

#Height and width 
PlotWidth = 15
PlotHeight = 9

PlotTheme = theme(axis.text=element_text(size=20),    #Text size for axis tick mark labels
                  axis.title.x=element_text(size=24, hjust=0.5, margin=margin(t=20, r=20, b=20, l=20)),               #Text size and alignment for x-axis label
                  axis.title.y=element_text(size=24, vjust=0.5,  margin=margin(t=20, r=20, b=20, l=20)),              #Text size and alignment for y-axis label
                  plot.title=element_text(size=26,face="bold",hjust=0.5, margin=margin(t=20, r=20, b=20, l=20)),      #Text size and alignment for plot title
                  legend.title=element_blank(),                                                                    #Text size of legend category labels
                  legend.text=element_text(size=20),                                                                   #Text size of legend title
                  legend.position = "bottom") 

#factor so months are in correct order
MonthlyStats$Month <- factor(MonthlyStats$Month, levels=c("Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug"))

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


###PRECIP
PLOT ="Monthly Sum of Precip_Line"
custom_breaks1 <- seq(0, 160, 5)

ggplot(MonthlyStats, aes(x=Month, y=SumPrecip_mm, group=1)) +
  geom_line(size = 1, colour = "black", stat="identity") + labs(x="Month", y="Total Monthly Precipitation [mm]") + scale_x_discrete(labels=MonthLabels) + scale_y_continuous(breaks = custom_breaks1, labels = every_nth(custom_breaks1, 4, inverse=TRUE)) + theme_classic()+ PlotTheme 

ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight)

#TEMP
PLOT="Monthly Temps"
custom_breaks2 <- seq(-20, 40, 5)
ggplot(MonthlyStats) + geom_line(aes(x=Month, y=MaxTemp_C, group=1, colour="Max Temp"), size=1) + geom_line(aes(x=Month, y=MinTemp_C, group=1, colour="Min Temp"), size=1) + geom_line(aes(x=Month, y=AvgTemp_C, group=1, colour="Avg Temp"), size=1) + labs(x="Month", y="Air Temperature [deg C]") + scale_x_discrete(labels=MonthLabels) + scale_y_continuous(breaks = custom_breaks2, labels = every_nth(custom_breaks2, 2, inverse=TRUE)) + theme_classic()+ PlotTheme + scale_color_manual(values = c("black", "red", "blue"))

ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight)


###RH
PLOT ="Monthly RH"
custom_breaks3 <- seq(50, 85, 1)
ggplot(MonthlyStats, aes(x=Month, y=AvgRH_., group=1)) +
  geom_line(size = 1, colour = "black", stat="identity") + labs(x="Month", y="RH [%]") + scale_x_discrete(labels=MonthLabels) + scale_y_continuous(breaks = custom_breaks3, labels = every_nth(custom_breaks3, 4, inverse=TRUE)) + theme_classic()+ PlotTheme 

ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight)

#WIND SPEED
PLOT="Monthly Wind Speeds"
custom_breaks4 <- seq(0, 25, 1)
ggplot(MonthlyStats) + geom_line(aes(x=Month, y=AvgWindSpeed_m.s, group=1, colour="Avg Wind Speed"), size=1) + geom_line(aes(x=Month, y=MaxWindSpeed_m.s, group=1, colour="Max Wind Speed"), size=1) + labs(x="Month", y="Wind Speed [m/s]") + scale_x_discrete(labels=MonthLabels) + scale_y_continuous(breaks=custom_breaks4, labels = every_nth(custom_breaks4, 5, inverse=TRUE)) + theme_classic()+ PlotTheme + scale_color_manual(values = c("black", "blue"))

ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight)

##############################################
##SNOTEL DATA for Black Mtn near Kalispell, MT
SNOTEL <- read.csv("C:/Users/sears/Documents/4_Classes_FA20/WR 574/Assignments/Assignment 1/SNOTEL_daily.csv", header=TRUE)%>%
  mutate(Date = mdy(Date))

##SWE
PLOT ="Daily SWE"
custom_breaks5 <- seq(0, 270, 10)
ggplot(SNOTEL, aes(x=Date, y=SWE_mm, group=1)) +
  geom_line(size = 1, colour = "black", stat="identity") + labs(x="Date", y="SWE [mm]") + scale_y_continuous(breaks=custom_breaks5, labels= every_nth(custom_breaks5, 5, inverse=TRUE)) + scale_x_date(date_labels = "%b", date_breaks = "31 days") + theme_classic()+ PlotTheme 

ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight)

##SNOW DEPTH
PLOT ="Daily Snow Depth"
custom_breaks6 <- seq(0, 1, 0.05)
ggplot(SNOTEL, aes(x=Date, y=SnowDpeth_m, group=1)) +
  geom_line(size = 1, colour = "black", stat="identity") + labs(x="Date", y="Snow Depth [m]") + scale_y_continuous(breaks=custom_breaks6, labels = every_nth(custom_breaks6, 2, inverse=TRUE)) + scale_x_date(date_labels = "%b", date_breaks = "31 days") + theme_classic()+ PlotTheme 

ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight)

##SNOW DENSITY
PLOT ="Daily Snow Density"
custom_breaks7 <- seq(0, 700, 20)
ggplot(SNOTEL, aes(x=Date, y=density_kgm3, group=1)) +
  geom_line(size = 1, colour = "black", stat="identity") + labs(x="Date", y="Snow Density [kg/m^3]")+ scale_y_continuous(breaks=custom_breaks7, labels=every_nth(custom_breaks7, 5, inverse=TRUE)) + scale_x_date(date_labels = "%b", date_breaks = "31 days") + theme_classic() + PlotTheme 

ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight)

###########################################
###STREAMFLOW for Mill Creek SE of Kalispell, MT

DailyQ <- read.csv("C:/Users/sears/Documents/4_Classes_FA20/WR 574/Assignments/Assignment 1/MillCr_Q.csv", header=TRUE)%>%
  mutate(datetime = mdy(datetime))

##DAILY Q
PLOT ="Daily Q"
custom_breaks8 <- seq(0, 2, 0.1)
ggplot(DailyQ, aes(x=datetime, y=discharge_cms, group=1)) +
  geom_line(size = 1, colour = "black", stat="identity") + labs(x="Date", y="Daily Discharge [m^3/s]") + scale_y_continuous(breaks = custom_breaks8, labels = every_nth(custom_breaks8, 2, inverse=TRUE)) + scale_x_date(date_labels = "%b", date_breaks = "31 days") + theme_classic()+ PlotTheme 

ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight)

##WIND ROSE
WindRose <- read.csv("C:/Users/sears/Documents/4_Classes_FA20/WR 574/Assignments/Assignment 1/windrose.csv", header=TRUE)%>%
  mutate(datetime = mdy_hm(datetime))

windRose(WindRose, ws = "WindSp_ms", wd = "WindDir_DegfromN", ws2 = NA, wd2 = NA,
         ws.int = 2, angle = 30, type = "default", bias.corr = TRUE, cols
         = "jet", grid.line = NULL, width = 1.5, seg = NULL, auto.text
         = TRUE, breaks = 6, offset = 10, normalise = FALSE, max.freq =
           NULL, paddle = FALSE, key.header = NULL, key.footer = "(m/s)",
         key.position = "bottom", key = list(height=1), dig.lab = 5, statistic =
           "prop.count", pollutant = NULL, annotate = TRUE, angle.scale =
           315, border = NA)