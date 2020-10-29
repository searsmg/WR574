# Assignment 6 - Snowpack metamorphism and Canopy Interception

rm(list=ls())

library(ggplot2)
library(dplyr)
library(lubridate)
library(RColorBrewer)
library(scales)

load("~/Repos/WR574/KalUpdate.RData")
setwd("C:/Users/sears/Documents/4_Classes_FA20/WR 574/Assignments/Assignment 6")

#lets clean up Kal_Correct before starting assignment - so many columns it's getting confusing. 
Kal_6 <- Kal_Correct %>%
  select(date.time, AirTemp_C, DewTemp_C, PrecipCorr, PrecipCorr_cumsum, Wind_ms, Precip_Snow_Dew, Precip_Snow_DewCum, SnowDensFix)

############################################################################

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
####################################################

#Question 2

#density at peak SWE is 315.2 kg/m^3 aka density max (from Black Mtn SNOTEL data)
#using first order exponential function to model densification of snowpack

Kal_6$SWE_fresh <- Kal_6$Precip_Snow_Dew

#start a new df as I step through Q2

#calculate fresh snow depth
Kal6_Q2 <- Kal_6 %>%
  select(date.time, AirTemp_C, SWE_fresh, DewTemp_C) %>%
  mutate(Dens_fresh = ifelse(SWE_fresh>0, 67.92 + (51.25*exp(DewTemp_C/2.59)), 0)) %>%
  mutate(FreshDepth = ifelse(Dens_fresh > 0, SWE_fresh/Dens_fresh, 0)) #now we have all 3 "fresh" vars

#calculate old snow vars

#depth and SWE old
Kal6_Q2 <- Kal6_Q2 %>%
  mutate(SWE_old = lag(cumsum(SWE_fresh)))

#density old
Kal6_Q2$Dens_old <- 0
n <- nrow(Kal6_Q2)

if(n>1) for (i in 562:n) Kal6_Q2$Dens_old[i] <- ((Kal6_Q2$Dens_all[i-1] - 315.2)*exp(-0.01)) + 315.2
Kal6_Q2$Dens_old[1] <- NA


Kal6_Q2 <- Kal6_Q2 %>%
  mutate(Depth_old = ifelse(Dens_old >0, SWE_old/Dens_old, 0))


#old + new calcs
Kal6_Q2 <- Kal6_Q2 %>%
  mutate(SWE_all = SWE_fresh + SWE_old,
         Depth_all = Depth_old + FreshDepth) %>%
  mutate(Dens_all = ifelse(Depth_all>0, SWE_all/Depth_all, 0))

PLOT = "Dens All-2b"
custombreaks1 <- seq(0, 350, 50)
ggplot(Kal6_Q2) + geom_line(aes(x=date.time, y=Dens_all), size=1) + PlotFormat + labs(y="Bulk snowpack density (kg/m^3)", x="Date") + scale_x_datetime(date_breaks = "2 month", labels = date_format("%b %Y")) + scale_y_continuous(breaks = custombreaks1, labels = every_nth(custombreaks1, 2, inverse=TRUE))

ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight)

Kal6_Q2_edit <- Kal6_Q2 %>%
  filter(date.time <= as.Date("2020-05-01"))

PLOT = "Depths"
custombreaks2 <- seq(0,4,0.5)
ggplot() + geom_line(data = Kal6_Q2_edit, aes(x=date.time, y=Depth_all, colour="Depth w/ metamorphism"), size = 1) + geom_line(data=Kal_SnowDepth, aes(x=date.time, y=SnowD_Cum, colour="Cumulative Snowfall"), size=1) + PlotFormat + labs(x="Date", y="Snowfall/Depth (m)") + scale_x_datetime(date_breaks = "2 month", labels = date_format("%b %Y")) + scale_y_continuous(breaks = custombreaks2, labels = every_nth(custombreaks2, 2, inverse=TRUE))

ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight)

write.csv(Kal_SnowDepth, "CumSnowfall.csv")

###################################################################################
#Question 3 - canopy interception modeling. Selected coniferous for species

Kal6Q3 <- Kal_Correct %>%
  select(date.time, AirTemp_C, PrecipCorr) %>%
  mutate(month = month(date.time))

Kal6Q3$FreshDens <- Kal6_Q2$Dens_fresh

Kal6Q3 <- Kal6Q3 %>%
  mutate(LAI = ifelse(month %in% c(6:9), 2, 1.6)) %>%
  mutate(Rain = ifelse(AirTemp_C > 0, PrecipCorr, 0),
         Snow = ifelse(AirTemp_C <= 0, PrecipCorr, 0)) %>%
  mutate(RainInt_mm = ifelse(Rain > 0, 0.2*LAI, 0)) %>%
  mutate(SnowIntMax_mm = ifelse(Snow > 0, 5.9*LAI*(0.27 + (46/FreshDens)), 0))

Kal6Q3$SnowInt <- 0
n <- nrow(Kal6Q3)

for (i in 2:n){Kal6Q3$SnowInt[i] <-
  if_else(Kal6Q3$snowyes == 1,
          0.697 * (Kal6Q3$SnowIntMax_mm - Kal6Q3$SnowInt[i-1]) * (-(1-exp((0.25*Kal6Q3$Snow)/Kal6Q3$SnowIntMax_mm))), 0)}

Kal6_Q2$Dens_old[1] <- NA
#################################################################################
#lets try Q3 again

Q3 <- read.csv("C:/Users/sears/Documents/4_Classes_FA20/WR 574/Assignments/Assignment 6/Q3.csv")


Q3 <- Q3 %>%
  mutate(AllInt = SnowInt + RainInt_mm)

Q3mo <- Q3 %>%
  group_by(month) %>%
  summarize(IntAll_moavg = mean(AllInt))
            
Q3_storms <- Q3 %>%
  filter(AllInt >0)
 
Q3storm_mo <- Q3_storms %>%
  group_by(month) %>%
  summarize(IntStorms = mean(AllInt))

Q3mo$month <- factor(Q3mo$month, levels=c(9, 10, 11, 12, 1, 2, 3, 4, 5, 6, 7, 8))

PLOT = "Monthly Avg for All Int"
custombreaks1 <- seq(0, .1, 0.025)
ggplot(Q3mo) + geom_line(aes(x=month, y=IntAll_moavg), group=1, size=1) + PlotFormat + scale_x_discrete(labels=MonthLabels) + labs(x="Water Year 2020", y="Monthly Average Interception (mm)") + scale_y_continuous(breaks = custombreaks1, labels = every_nth(custombreaks1, 2, inverse=TRUE))

ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight)


Q3storm_mo$month <- factor(Q3storm_mo$month, levels=c(9, 10, 11, 12, 1, 2, 3, 4, 5, 6, 7, 8))

PLOT = "Monthly Avg for Storm Int"
custombreaks2 <- seq(0, 0.5, 0.05)
ggplot(Q3storm_mo) + geom_line(aes(x=month, y=IntStorms), group=1, size=1) + PlotFormat + scale_x_discrete(labels=MonthLabels) + labs(x="Water Year 2020", y="Monthly Average Interception (mm)") + scale_y_continuous(breaks = custombreaks2, labels = every_nth(custombreaks2, 2, inverse=TRUE))

ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight)

#######################################################################################



