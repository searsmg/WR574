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

SNOTEL <- read.csv("C:/Users/sears/Documents/4_Classes_FA20/WR 574/Assignments/Assignment 5/SNOTEL_daily.csv")%>%
  mutate(Date = mdy(Date))

#SNOTEL$Date <- as.POSIXct(SNOTEL$Date)

#################################################################

#plot formatting

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
ggplot(Kal_Correct) + geom_line(aes(x=date.time, y=Precip_Snow_AirCum, colour="Snow"), size=1) + theme_classic() + geom_line(aes(x=date.time, y=AirTemp_C, colour="Temp"), size=1) + PlotTheme + labs(x="Water Year 2020", y="AirTemp (C)") + scale_x_datetime(date_breaks = "1 month", labels = date_format("%b")) + scale_color_manual(values = c("Temp"= "blue", "Snow" = "green"))

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

PLOT="Snow Depth"
custombreaks1 <- seq(0, 4, .25)
ggplot(SNOTEL) + geom_line(aes(x=Date, y=Snowfall_mm/1000, colour="Computed Snowfall"), size =1, group=1)+ geom_line(aes(x=Date, y=SnowDepthCum_mm/1000, colour="Observed Snow Depth"), size=1, group=1) + scale_y_continuous(breaks = custombreaks1, labels = every_nth(custombreaks1, 2, inverse=TRUE)) + labs(x="Date", y="Snowfall/Snow Depth (m)") + scale_color_manual(values = c("Computed Snowfall"= "purple", "Observed Snow Depth" = "blue")) + theme_classic()+ PlotTheme + scale_x_date(date_labels = "%b %Y", date_breaks = "2 month", limits=as.Date(c("2019-09-01", "2020-08-31")))

ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight)


#######################################################################
#Question 2i

Kal_Correct <- Kal_Correct %>%
  mutate(SnowDens = ifelse(Precip_Snow_Dew > 0,67.92+(51.25*exp(AirTemp_C/2.59)), NA)) %>% 
  mutate(SnowDensFix = ifelse(SnowDens > 700,NA, SnowDens))

Kal_MonthlyDens <- Kal_Correct %>%
  mutate(month = month(date.time)) %>% 
  group_by(month) %>%
  summarize(MoAvgDens = mean(SnowDensFix, na.rm=TRUE))

Kal_MonthlyDens$month <- factor(Kal_MonthlyDens$month, levels=c(9, 10, 11, 12, 1, 2, 3, 4, 5, 6, 7, 8))

PLOT = "Fresh Snow Dens - Q2i"
custombreaks2 <- seq(0,200,20)
ggplot(Kal_MonthlyDens) + geom_line(aes(x=month, y=MoAvgDens), group=1, size=1) + labs(x="Water Year 2020", y="Monthly Average Fresh Snow Density (kg/m^3)") + theme_classic() + PlotTheme + scale_x_discrete(labels=MonthLabels) + scale_y_continuous(breaks = custombreaks2, labels = every_nth(custombreaks2, 2, inverse=TRUE))

ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight)

###################################################################################
#Question 2ii - this is just the cumulative hourly precip as snow

PLOT = "Cumulative Hourly Snowfall"
custombreaks3 <- seq(0, 400, 50)
ggplot(Kal_Correct) + theme_classic() + geom_line(aes(x=date.time, y=Precip_Snow_DewCum), size=1) + PlotTheme + labs(x="Date", y="Cumulative Hourly Snowfall (mm)") + scale_y_continuous(breaks = custombreaks3, labels = every_nth(custombreaks3, 2, inverse=TRUE)) + scale_x_datetime(date_breaks = "2 month", labels = date_format("%b %Y")) 

ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight)

###################################################################################
#Question 4


#####################################################################################
#Question 5

PLOT="Snow Dens and Air Temp"
custombreaks4 <- seq(0, 700, 50)
regress <- ggplot(SNOTEL, aes(x=AirTempAvg_C, y=FreshSnowDens)) + geom_point(size =1, group=1)+scale_y_continuous(breaks = custombreaks4, labels = every_nth(custombreaks4, 2, inverse=TRUE)) + labs(x="Air Temp (deg C)", y="Fresh Snow Density (kg/m^3") + theme_classic()+ PlotTheme + geom_smooth(method="lm", formula=y~x) + stat_poly_eq(formula = my.formula, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE)


ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight)

