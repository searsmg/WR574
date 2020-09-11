# Assignment 1 

library(ggplot2)
library(dplyr)
library(scales)
library(Hmisc)
library(lubridate)

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

###PRECIP
PLOT ="Monthly Sum of Precip_Line"
ggplot(MonthlyStats, aes(x=Month, y=SumPrecip_mm, group=1)) +
  geom_line(size = 1, colour = "black", stat="identity") + labs(x="Month", y="Total Monthly Precipitation [mm]") + scale_x_discrete(labels=MonthLabels) + scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) + theme_classic()+ PlotTheme 

ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight)

#TEMP
PLOT="Monthly Temps"
ggplot(MonthlyStats) + geom_line(aes(x=Month, y=MaxTemp_C, group=1, colour="Max Temp"), size=1) + geom_line(aes(x=Month, y=MinTemp_C, group=1, colour="Min Temp"), size=1) + geom_line(aes(x=Month, y=AvgTemp_C, group=1, colour="Avg Temp"), size=1) + labs(x="Month", y="Air Temperature [deg C]") + scale_x_discrete(labels=MonthLabels) + scale_y_continuous(breaks = scales::pretty_breaks(n = 7)) + theme_classic()+ PlotTheme + scale_color_manual(values = c("black", "red", "blue"))

ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight)


###RH
PLOT ="Monthly RH"
ggplot(MonthlyStats, aes(x=Month, y=AvgRH_., group=1)) +
  geom_line(size = 1, colour = "black", stat="identity") + labs(x="Month", y="RH [%]") + scale_x_discrete(labels=MonthLabels) + scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) + theme_classic()+ PlotTheme 

ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight)

#WIND SPEED
PLOT="Monthly Wind Speeds"
ggplot(MonthlyStats) + geom_line(aes(x=Month, y=AvgWindSpeed_m.s, group=1, colour="Avg Wind Speed"), size=1) + geom_line(aes(x=Month, y=MaxWindSpeed_m.s, group=1, colour="Max Wind Speed"), size=1) + labs(x="Month", y="Wind Speed [m/s]") + scale_x_discrete(labels=MonthLabels) + scale_y_continuous(breaks = scales::pretty_breaks(n = 7)) + theme_classic()+ PlotTheme + scale_color_manual(values = c("black", "blue"))

ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight)

##############################################
##SNOTEL DATA for Black Mtn near Kalispell, MT
SNOTEL <- read.csv("C:/Users/sears/Documents/4_Classes_FA20/WR 574/Assignments/Assignment 1/SNOTEL_daily.csv", header=TRUE)%>%
  mutate(Date = mdy(Date))

##SWE
PLOT ="Daily SWE"
ggplot(SNOTEL, aes(x=Date, y=SWE_mm, group=1)) +
  geom_line(size = 1, colour = "black", stat="identity") + labs(x="Date", y="SWE [mm]") + scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + scale_x_date(date_labels = "%b", date_breaks = "31 days") + theme_classic()+ PlotTheme 

ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight)

##SNOW DEPTH
PLOT ="Daily Snow Depth"
ggplot(SNOTEL, aes(x=Date, y=SnowDpeth_m, group=1)) +
  geom_line(size = 1, colour = "black", stat="identity") + labs(x="Date", y="Snow Depth [m]") + scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + scale_x_date(date_labels = "%b", date_breaks = "31 days") + theme_classic()+ PlotTheme 

ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight)

##SNOW DENSITY
PLOT ="Daily Snow Density"
ggplot(SNOTEL, aes(x=Date, y=density_kgm3, group=1)) +
  geom_line(size = 1, colour = "black", stat="identity") + labs(x="Date", y="Snow Density [kg/m^3]")+ scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + scale_x_date(date_labels = "%b", date_breaks = "31 days") + theme_classic() + PlotTheme 

ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight)

###########################################
###STREAMFLOW for Mill Creek SE of Kalispell, MT

DailyQ <- read.csv("C:/Users/sears/Documents/4_Classes_FA20/WR 574/Assignments/Assignment 1/MillCr_Q.csv", header=TRUE)%>%
  mutate(datetime = mdy(datetime))

##DAILY Q
PLOT ="Daily Q"
ggplot(DailyQ, aes(x=datetime, y=discharge_cms, group=1)) +
  geom_line(size = 1, colour = "black", stat="identity") + labs(x="Date", y="Daily Discharge [m^3/s]") + scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + scale_x_date(date_labels = "%b", date_breaks = "31 days") + theme_classic()+ PlotTheme 

ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight)