#Assignment 3 for WR574 - Cloud Cover and Precip Freq

rm(list=ls())

library(ggplot2)
library(dplyr)
library(lubridate)
library(RColorBrewer)

#set wd for saving plots
setwd("C:/Users/sears/Documents/4_Classes_FA20/WR 574/Assignments/Assignment 3/Code_Test/")

#Kalispell ASOS station hourly data. Mutate date to work with in R 
Kal <- read.csv("C:/Users/sears/Documents/4_Classes_FA20/WR 574/Assignments/Assignment 3/Assignment3_KalASOS.csv")%>%
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

#exported csv
write.csv(KalCloud, "KalCloud.csv")
      
#imported df of monthly freq
KalCloud_Mo <- read.csv("C:/Users/sears/Documents/4_Classes_FA20/WR 574/Assignments/Assignment 3/KalCloud_Month.csv")

#group by month and get avg month cloud fraction
KalCloud_MoAvg <- Kal %>%
  mutate(month= month(date.time)) %>%
  group_by(month) %>%
  summarize(MonthAvg = mean(CloudFrac))

#factor so months are in correct order - THIS IS FOR GGPLOT SO IT DOESN'T MIX UP THE MONTHS 
KalCloud_Mo$month <- factor(KalCloud_Mo$month, levels=c(9, 10, 11, 12, 1, 2, 3, 4, 5, 6, 7, 8))
KalCloud_MoAvg$month <- factor(KalCloud_MoAvg$month, levels=c(9, 10, 11, 12, 1, 2, 3, 4, 5, 6, 7, 8))

#factor so cloud type are in order below (least to greatest CC)
KalCloud_Mo$CloudTyp <- factor(KalCloud_Mo$CloudTyp, levels=c("CLR", "FEW", "SCT", "BKN", "OVC"))

#CC freq col plot
PLOT = "Cloud Cov Freq"
custombreaks1 <- seq(0,1, 0.05)
ggplot() + geom_col(data = KalCloud_Mo, aes(x=factor(month), y=freq, fill=factor(CloudTyp))) + theme_classic() + PlotTheme + labs(x="Month", y="Cloud Cover Frequency") + scale_x_discrete(labels=MonthLabels) + scale_fill_brewer(palette = "Dark2") + scale_y_continuous(breaks = custombreaks1, labels = every_nth(custombreaks1, 2, inverse=TRUE)) + geom_line(data = KalCloud_MoAvg, aes(x=factor(month), y=MonthAvg, color="Avg. Cloud Fraction"), group=1, size=2) + scale_color_manual(values="black")

ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight)

###############################################################################

#QUESTION 2

#no trace included
Kal_noT <- Kal[!(Kal$HourlyPrecip_in=="T"),]

#set to precip occurring or not
Kal_PrecipFreq <- Kal_noT %>%
  mutate(Precip_Y_N =  case_when(
    HourlyPrecip_in == 0 ~ "No Precip"))

#shitty way to say precip is occurring, got lazy
Kal_PrecipFreq[is.na(Kal_PrecipFreq)] = "Precip"

#get freq of precip occurring
KalPrecipFreq_Mo <- Kal_PrecipFreq %>%
  mutate(month = month(date.time)) %>%
  group_by(month, Precip_Y_N) %>%
  summarize(n=n()) %>%
  mutate(freq = n / sum(n))

#trace included
Kal_Tfix <- read.csv("C:/Users/sears/Documents/4_Classes_FA20/WR 574/Assignments/Assignment 3/Kal_T.csv")%>%
  mutate(date.time = mdy_hm(date.time))

#precip occurring or not
Kal_PrecipFreq_T <- Kal_Tfix %>%
  mutate(Precip_Y_N =  case_when(
    HourlyPrecip_in == 0 ~ "No Precip"))

#again, shitty way to say precip is occurring
Kal_PrecipFreq_T[is.na(Kal_PrecipFreq_T)] = "Precip"

#get freq of precip occurring
KalPrecipFreqT_Mo <- Kal_PrecipFreq_T %>%
  mutate(month = month(date.time)) %>%
  group_by(month, Precip_Y_N) %>%
  summarize(n=n()) %>%
  mutate(freq = n / sum(n))

#filter for precip for trace and no trace
KalPrecip_noT <- KalPrecipFreq_Mo %>%
  filter(Precip_Y_N == "Precip")

KalPrecip_T <- KalPrecipFreqT_Mo%>%
  filter(Precip_Y_N == "Precip")

KalPrecip_noT <- KalPrecip_noT %>%
  mutate(newcol = "Precip w/ no trace")

KalPrecip_T <- KalPrecip_T %>%
  mutate(newcol = "Precip w/ trace")

#combine two df of precip trace and no trace
KalPrecip <- bind_rows(KalPrecip_T, KalPrecip_noT)

#factor so months are in correct order - THIS IS FOR GGPLOT SO IT DOESN'T MIX UP THE MONTHS 
KalPrecip$month <- factor(KalPrecip$month, levels=c(9, 10, 11, 12, 1, 2, 3, 4, 5, 6, 7, 8))

PLOT = "Precip Freq"
custombreaks1 <- seq(0, 1, 0.05)
ggplot() + geom_col(data = KalPrecip, aes(x=factor(month), y=freq, fill=newcol), position="dodge2") + theme_classic() + PlotTheme + labs(x="Month", y="Precip Frequency") + scale_x_discrete(labels=MonthLabels) + scale_fill_brewer(palette = "Dark2") + scale_y_continuous(breaks = custombreaks1, labels = every_nth(custombreaks1, 2, inverse=TRUE)) 

ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight)

#################################################

#QUESTION 4

#df that includes ALL CLOUD DECKS
Clouds <- read.csv("C:/Users/sears/Documents/4_Classes_FA20/WR 574/Assignments/Assignment 3/CloudDecks.csv")%>%
  mutate(date.time = mdy_hm(date.time))

#remove NAs rows - for some reason there are whole rows that are blank
Clouds <- Clouds %>% 
  filter_all(any_vars(!is.na(.)))
Clouds <- Clouds %>% 
  filter_all(any_vars(complete.cases(.)))  

#############STOP AND RUN THIS LINE BY ITSELF
#average the cloud decks together to get cloud frac for all decks
Clouds$Mean <- rowMeans(Clouds[,6:8], na.rm=TRUE)

#add cloud fraction number column based on cloud descriptions
Clouds$CloudFrac1 <- recode(Clouds$skyc1, "BKN" = 6/8, "CLR" = 0/8, "FEW" = 1/8, "OVC" = 8/8, "SCT" = 3.5/8)
Clouds$CloudFrac2 <- recode(Clouds$skyc2, "BKN" = 6/8, "CLR" = 0/8, "FEW" = 1/8, "OVC" = 8/8, "SCT" = 3.5/8)
Clouds$CloudFrac3 <- recode(Clouds$skyc3, "BKN" = 6/8, "CLR" = 0/8, "FEW" = 1/8, "OVC" = 8/8, "SCT" = 3.5/8)

#get cloud cover type based on cloud frac ranges
Clouds <- Clouds %>%
  mutate(CloudTyp = case_when(
    between(Mean, 0, 0.0001) ~ "CLR",
    between(Mean, 0.125, 0.35) ~ "FEW",
    between(Mean, 0.99, 1) ~ "OVC",
    between(Mean, 0.36, 0.65) ~ "SCT",
    between(Mean, 0.66, 0.99) ~ "BKN")
  )

Clouds <- Clouds %>% 
  filter(!is.na(Mean))

#got frustrated
write.csv(Clouds, "Clouds_fix.csv")
  
#get mean cloud type by month and determine freq
Clouds_Mean <- Clouds %>%
  mutate(month = month(date.time)) %>%
  group_by(month, CloudTyp) %>%
  summarize(n=n()) %>%
  mutate(freq = n / sum(n))

#factor so cloud type are in order below (least to greatest CC)
Clouds_Mean$CloudTyp <- factor(Clouds_Mean$CloudTyp, levels=c("CLR", "FEW", "SCT", "BKN", "OVC"))

#factor so months are in correct order - THIS IS FOR GGPLOT SO IT DOESN'T MIX UP THE MONTHS 
Clouds_Mean$month <- factor(Clouds_Mean$month, levels=c(9, 10, 11, 12, 1, 2, 3, 4, 5, 6, 7, 8))

#CC freq col plot FOR ALL CLOUD DECKS
PLOT = "Cloud Cov Freq - ALL DECKS"
custombreaks1 <- seq(0,1, 0.05)
ggplot() + geom_col(data = Clouds_Mean, aes(x=factor(month), y=freq, fill=factor(CloudTyp))) + theme_classic() + PlotTheme + labs(x="Month", y="Cloud Cover Frequency") + scale_x_discrete(labels=MonthLabels) + scale_fill_brewer(palette = "Set1") + scale_y_continuous(breaks = custombreaks1, labels = every_nth(custombreaks1, 2, inverse=TRUE)) 

ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight)

## CC freq for lower deck
PLOT = "Cloud Cov Freq - Low Deck"
custombreaks1 <- seq(0,1, 0.05)
ggplot() + geom_col(data = KalCloud_Mo, aes(x=factor(month), y=freq, fill=factor(CloudTyp))) + theme_classic() + PlotTheme + labs(x="Month", y="Cloud Cover Frequency") + scale_x_discrete(labels=MonthLabels) + scale_fill_brewer(palette = "Dark2") + scale_y_continuous(breaks = custombreaks1, labels = every_nth(custombreaks1, 2, inverse=TRUE)) 

ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight)

#####################################################################################
#question 5

CloudTemp <- read.csv("C:/Users/sears/Documents/4_Classes_FA20/WR 574/Assignments/Assignment 3/CloudTemp.csv")%>%
  mutate(date.time = mdy_hm(date.time))

#determine crystal type by range of cloud temps
CloudTemp <- CloudTemp %>%
  mutate(CrystalTyp = case_when(
    between(CloudTemp_C, -35, -21) ~ "Columns & Plates",
    between(CloudTemp_C, -21, -10) ~ "Plates",
    between(CloudTemp_C, -10, -4) ~ "Columns",
    between(CloudTemp_C, -4, 0) ~ "Plates",
    between(CloudTemp_C, 0, 100) ~ "Rain")
  )

#get freq of crystal type or rain occurring
CrystalTyp <- CloudTemp %>%
  mutate(month = month(date.time)) %>%
  group_by(month, CrystalTyp) %>%
  summarize(n=n()) %>%
  mutate(freq = n / sum(n))

#factor so cloud type are in order below (least to greatest CC)
CrystalTyp$CrystalTyp <- factor(CrystalTyp$CrystalTyp, levels=c("Rain", "Plates", "Columns", "Columns & Plates"))

#factor so months are in correct order - THIS IS FOR GGPLOT SO IT DOESN'T MIX UP THE MONTHS 
CrystalTyp$month <- factor(CrystalTyp$month, levels=c(9, 10, 11, 12, 1, 2, 3, 4, 5, 6, 7, 8))

## CC freq for lower deck
PLOT = "Crystal Freq"
custombreaks1 <- seq(0,1, 0.05)
ggplot() + geom_col(data = CrystalTyp, aes(x=factor(month), y=freq, fill=factor(CrystalTyp))) + theme_classic() + PlotTheme + labs(x="Month", y="Precipitaion Form and/or Shape") + scale_x_discrete(labels=MonthLabels) + scale_fill_brewer(palette = "Spectral") + scale_y_continuous(breaks = custombreaks1, labels = every_nth(custombreaks1, 2, inverse=TRUE)) 

ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight)
