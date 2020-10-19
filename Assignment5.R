rm(list=ls())

library(ggplot2)
library(dplyr)
library(lubridate)
library(RColorBrewer)
library(scales)
library(plotly)
library(ggpubr)

#set wd for saving plots
setwd("C:/Users/sears/Documents/4_Classes_FA20/WR 574/Assignments/Assignment 5/")

load("~/Repos/WR574/Kal_Correct.RData")

SNOTEL <- read.csv("C:/Users/sears/Documents/4_Classes_FA20/WR 574/Assignments/Assignment 5/SNOTEL_daily.csv")%>%
  mutate(Date = mdy(Date))

#SNOTEL$Date <- as.POSIXct(SNOTEL$Date)

#################################################################

#plot formatting

#X-axis labels for monthly plots
MonthLabels = c("Sep 2019", "Oct 2019", "Nov 2019", "Dec 2019", "Jan 2020", "Feb 2020", "Mar 2020", "Apr 200", "May 2020", "Jun 2020", "Jul 2020", "Aug 2020")

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
ggplot(Kal_Correct) + geom_line(aes(x=date.time, y=SnowDepth, colour="Snow"), size=1) + theme_classic() + geom_line(aes(x=date.time, y=AirTemp_C, colour="Temp"), size=1) + PlotTheme + labs(x="Water Year 2020", y="AirTemp (C)") + scale_x_datetime(date_breaks = "1 month", labels = date_format("%b")) + scale_color_manual(values = c("Temp"= "blue", "Snow" = "green"))

write.csv(Kal_Correct, "q1.csv")
ggplotly(q1)

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

#PLOT = "Fresh Snow Dens - Q2i"
#custombreaks2 <- seq(0,200,20)
#ggplot(Kal_MonthlyDens) + geom_line(aes(x=month, y=MoAvgDens), group=1, size=1) + labs(x="Water Year 2020", y="Monthly Average Fresh Snow Density (kg/m^3)") + theme_classic() + PlotTheme + scale_x_discrete(labels=MonthLabels) + scale_y_continuous(breaks = custombreaks2, labels = every_nth(custombreaks2, 2, inverse=TRUE))

#ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight)

KalSnowDens <- Kal_Correct %>%
  select(date.time, SnowDensFix)

KalSnowDens <- na.omit(KalSnowDens)

KalSnowDens_Mo <- KalSnowDens %>%
  mutate(month = month(date.time)) %>%
  group_by(month) %>%
  summarize(MoSnoDens = mean(SnowDensFix))

# DO NOT USE
PLOT = "Fresh Snow Dens - Q2i-explore"
custombreaks2 <- seq(0,700,20)
ggplot(KalSnowDens) + geom_point(aes(x=date.time, y=SnowDensFix), group=1, size=1, na.rm=TRUE) + labs(x="Water Year 2020", y="Fresh Snow Density (kg/m^3)") + theme_classic() + PlotTheme + scale_x_datetime(date_breaks = "1 month", labels = date_format("%b")) + scale_y_continuous(breaks = custombreaks2, labels = every_nth(custombreaks2, 2, inverse=TRUE))

ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight)

KalSnowDens_Mo$month <- factor(KalSnowDens_Mo$month, levels=c(9, 10, 11, 12, 1, 2, 3, 4, 5, 6, 7, 8))

# USE THIS ONE
PLOT = "Fresh Snow Dens - Q2i"
custombreaks2 <- seq(0,700,20)
ggplot(KalSnowDens_Mo) + geom_line(aes(x=month, y=MoSnoDens), group=1, size=1) + labs(x="Date", y="Monthly Average Fresh Snow Density (kg/m^3)") + theme_classic() + PlotTheme + scale_x_discrete(labels=MonthLabels) + scale_y_continuous(breaks = custombreaks2, labels = every_nth(custombreaks2, 2, inverse=TRUE))

ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight)

###################################################################################
#Question 2ii - this is just the cumulative hourly precip as snow

Kal_Correct$SnowDepth <- Kal_Correct$Precip_Snow_Dew / Kal_Correct$SnowDensFix

Kal_SnowDepth <- Kal_Correct %>%
  select(date.time, SnowDepth)

Kal_SnowDepth <- na.omit(Kal_SnowDepth)

Kal_SnowDepth <- Kal_SnowDepth %>%
  mutate(SnowD_Cum = cumsum(SnowDepth))

PLOT = "Cumulative Hourly Snowfall"
custombreaks3 <- seq(0, 5, 0.5)
ggplot(Kal_SnowDepth) + theme_classic() + geom_line(aes(x=date.time, y=SnowD_Cum), size=1) + PlotTheme + labs(x="Date", y="Cumulative Hourly Snowfall (m)") + scale_y_continuous(breaks = custombreaks3, labels = every_nth(custombreaks3, 2, inverse=TRUE)) + scale_x_datetime(date_breaks = "1 month", labels = date_format("%b %Y")) 

ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight)

###################################################################################
#Question 4 - ALBEDO MONSTER

Kal_Albedo <- Kal_Correct %>%
  select(date.time, AirTemp_C, Precip_Snow_Dew)

Kal_Albedo <- Kal_Albedo %>%
  mutate(month = month(date.time))

Kal_Albedo <- Kal_Albedo %>%
  mutate(Snow_Fresh = ifelse(AirTemp_C < 0 & Precip_Snow_Dew > 0,Precip_Snow_Dew,0),
         soil_al = 0.18,
         fresh_al = ifelse(Snow_Fresh > 0, 0.84,NA),
         al_min = ifelse(AirTemp_C < 0, 0.7, 0.5)) %>%
  mutate(al_min = ifelse(month %in% c(5:9), 0.18, al_min)) %>%
  mutate(Snow_Cum = cumsum(Snow_Fresh)) %>%
  mutate(actual_al = ifelse(Snow_Cum == 0, soil_al, NA)) %>%
  mutate(SnowStop = ifelse(Snow_Cum == lag(Snow_Cum), 0, 1)) %>%
  mutate(SnowStop = ifelse(is.na(SnowStop), 0, SnowStop)) %>%
  mutate(actual_al = ifelse(Snow_Fresh > 0, 0.84, actual_al))

albedo = Kal_Albedo$actual_al

for(i in 2:nrow(Kal_Albedo)){
  if(is.na(albedo[i])){
    albedo[i] = ((albedo[i-1]-Kal_Albedo$al_min[i])*exp(-0.01)) + Kal_Albedo$al_min[i]
  }
}


PLOT = "Albedo"
custombreaks4<- seq(0, 0.85, 0.05)
ggplot(Kal_Albedo) + geom_line(aes(x=date.time, y=actual_al), size=1) + labs(x="Date", y="Modeled Albedo") + theme_classic() + PlotTheme + scale_y_continuous(breaks = custombreaks4, labels = every_nth(custombreaks4, 2, inverse=TRUE)) + scale_x_datetime(date_breaks = "2 month", labels = date_format("%b %Y")) 

ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight)


#####################################################################################
#Question 5

PLOT="Snow Dens and Air Temp"
custombreaks4 <- seq(0, 700, 50)
regress <- ggplot(SNOTEL, aes(x=AirTempAvg_C, y=FreshSnowDens)) + geom_point(size =1, group=1)+scale_y_continuous(breaks = custombreaks4, labels = every_nth(custombreaks4, 2, inverse=TRUE)) + labs(x="Air Temp (deg C)", y="Fresh Snow Density (kg/m^3") + theme_classic()+ PlotTheme + geom_smooth(method="lm", formula=y~x) + stat_poly_eq(formula = my.formula, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE)


ggscatter(SNOTEL, x="AirTempAvg_C", y="FreshSnowDens", color = "black", shape = 21, size = 3, 
          add= "reg.line",
          add.params = list(color="blue", fill = "lightgray"),
          conf.int=TRUE,
          cor.coef=TRUE,
          cor.coeff.args = list(method="pearson", label.x=3, label.sep="\n"))


SNOTEL.lm = lm(FreshSnowDens ~ AirTempAvg_C, data=SNOTEL)
