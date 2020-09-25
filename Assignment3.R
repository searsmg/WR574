#Assignment 3 for WR574 - Cloud Cover and Precip Freq

rm(list=ls())

library(ggplot2)
library(dplyr)
library(lubridate)
library(RColorBrewer)

#set wd for saving plots
setwd("C:/Users/sears/Documents/4_Classes_FA20/WR 574/Assignments/Assignment 3/")

#Kalispell ASOS station hourly data. Mutate date to work with in R 
Kal <- read.csv("C:/Users/sears/Documents/4_Classes_FA20/WR 574/Assignments/Assignment 3/Assignment3_KalASOS.csv", na.rm=TRUE)%>%
      mutate(date.time = mdy_hm(date.time))

Kal <-na.omit(Kal)

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
#QUESTION 1

#add cloud fraction number column based on cloud descriptions
Kal$CloudFrac <- recode(Kal$skyc1, "BKN" = 6/8, "CLR" = 0/8, "FEW" = 1/8, "OVC" = 8/8, "SCT" = 3.5/8)

#get freq value
KalCloud <- Kal %>%
      mutate(month = month(date.time)) %>%
      group_by(month, CloudFrac) %>%
      summarize(n=n()) %>%
      mutate(freq = n / sum(n))

write.csv(KalCloud, "KalCloud.csv")
      
#imported df of monthly freq
KalCloud_Mo <- read.csv("C:/Users/sears/Documents/4_Classes_FA20/WR 574/Assignments/Assignment 3/KalCloud_Month.csv")

KalCloud_MoAvg <- Kal %>%
  mutate(month= month(date.time)) %>%
  group_by(month) %>%
  summarize(MonthAvg = mean(CloudFrac))

#factor so months are in correct order - THIS IS FOR GGPLOT SO IT DOESN'T MIX UP THE MONTHS 
KalCloud_Mo$month <- factor(KalCloud_Mo$month, levels=c(9, 10, 11, 12, 1, 2, 3, 4, 5, 6, 7, 8))

#factor so cloud type are in order below (least to greatest CC)
KalCloud_Mo$CloudTyp <- factor(KalCloud_Mo$CloudTyp, levels=c("CLR", "FEW", "SCT", "BKN", "OVC"))

#CC freq col plot
PLOT = "Cloud Cov Freq"
custombreaks1 <- seq(0,100, 5)
ggplot() + geom_col(data = KalCloud_Mo, aes(x=factor(month), y=freq*100, fill=factor(CloudTyp))) + theme_classic() + PlotTheme + labs(x="Month", y="Cloud Cover Frequency (%)") + scale_x_discrete(labels=MonthLabels) + scale_fill_brewer(palette = "Dark2") + scale_y_continuous(breaks = custombreaks1, labels = every_nth(custombreaks1, 2, inverse=TRUE)) + geom_line(data = KalCloud_MoAvg, aes(x=month, y=MonthAvg*100, color="Avg. Cloud Fraction"), group=1, size=2) + scale_color_manual(values="black")

ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight)

###############################################################################
#QUESTION 2
#no trace included
Kal_noT <- Kal[!(Kal$HourlyPrecip_in=="T"),]

Kal_PrecipFreq <- Kal_noT %>%
  mutate(Precip_Y_N =  case_when(
    HourlyPrecip_in == 0 ~ "No Precip"))

Kal_PrecipFreq[is.na(Kal_PrecipFreq)] = "Precip"

KalPrecipFreq_Mo <- Kal_PrecipFreq %>%
  mutate(month = month(date.time)) %>%
  group_by(month, Precip_Y_N) %>%
  summarize(n=n()) %>%
  mutate(freq = n / sum(n))

#trace included
Kal_Tfix <- read.csv("C:/Users/sears/Documents/4_Classes_FA20/WR 574/Assignments/Assignment 3/Kal_T.csv")

Kal_PrecipFreq_T <- Kal_T %>%
  mutate(Precip_Y_N =  case_when(
    HourlyPrecip_in == 0 ~ "No Precip"))

Kal_PrecipFreq_T[is.na(Kal_PrecipFreq_T)] = "Precip"

KalPrecipFreqT_Mo <- Kal_PrecipFreq_T %>%
  mutate(month = month(date.time)) %>%
  group_by(month, Precip_Y_N) %>%
  summarize(n=n()) %>%
  mutate(freq = n / sum(n))

KalPrecip_noT <- KalPrecipFreq_Mo %>%
  filter(Precip_Y_N == "Precip")

KalPrecip_T <- KalPrecipFreqT_Mo%>%
  filter(Precip_Y_N == "Precip")

KalPrecip_noT <- KalPrecip_noT %>%
  mutate(newcol = "Precip w/ no trace")

KalPrecip_T <- KalPrecip_T %>%
  mutate(newcol = "Precip w/ trace")

KalPrecip <- bind_rows(KalPrecip_T, KalPrecip_noT)

PLOT = "Precip Freq"
custombreaks1 <- seq(0,100, 5)
ggplot() + geom_col(data = KalPrecip, aes(x=factor(month), y=freq*100, fill=newcol), position="dodge2") + theme_classic() + PlotTheme + labs(x="Month", y="Precip Frequency (%)") + scale_x_discrete(labels=MonthLabels) + scale_fill_brewer(palette = "Dark2") + scale_y_continuous(breaks = custombreaks1, labels = every_nth(custombreaks1, 2, inverse=TRUE)) 

ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight)

######################################
