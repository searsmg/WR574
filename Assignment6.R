# Assignment 6 - Snowpack metamorphism and Canopy Interception

rm(list=ls())

library(ggplot2)
library(dplyr)
library(lubridate)
library(RColorBrewer)

load("~/Repos/WR574/KalUpdate.RData")
setwd("C:/Users/sears/Documents/4_Classes_FA20/WR 574/Assignments/Assignment 6")

#lets clean up Kal_Correct before starting assignment - so many columns it's getting confusing. 
Kal_6 <- Kal_Correct %>%
  select(date.time, AirTemp_C, DewTemp_C, PrecipCorr, PrecipCorr_cumsum, Wind_ms, Precip_Snow_Dew, Precip_Snow_DewCum, SnowDensFix)

############################################################################

#Plot Format

#X-axis labels for plots that are monthly
MonthLabels = c("Sep 2019", "Oct 2019", "Nov 2019", "Dec 2019", "Jan 2020", "Feb 2020", "Mar 2020", "Apr 200", "May 2020", "Jun 2020", "Jul 2020", "Aug 2020")

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

if(n>1) for (i in 562:n) Kal6_Q2$Dens_old[i] <- ((Kal6_Q2$Dens_old[i-1] - 315.2)*exp(-0.01)) + 315.2
Kal6_Q2$Dens_old[1] <- NA

Kal6_Q2 <- Kal6_Q2 %>%
  mutate(Depth_old = ifelse(Dens_old >0, SWE_old/Dens_old, 0))

#old + new calcs
Kal6_Q2 <- Kal6_Q2 %>%
  mutate(SWE_all = SWE_fresh + SWE_old,
         Depth_all = Depth_old + FreshDepth) %>%
  mutate(Dens_all = ifelse(Depth_all>0, SWE_all/Depth_all, 0))

ggplot(Kal6_Q2) + geom_line(aes(x=date.time, y=Dens_all)) + PlotFormat

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

x <- Kal6Q3$snowyes
a <- Kal6Q3$SnowInt

if (x[i] == 1) a[i] <- (Kal6Q3$SnowIntMax_mm - a[i-1]) * (-(1-exp((0.25*Kal6Q3$Snow)/Kal6Q3$SnowIntMax_mm))),
else if (x[i] == 0]) a[i] <- 0
