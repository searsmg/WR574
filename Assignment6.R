# Assignment 6 - Snowpack metamorphism and Canopy Interception

rm(list=ls())

library(ggplot2)
library(dplyr)
library(lubridate)
library(RColorBrewer)

load("~/Repos/WR574/KalUpdate.RData")

#lets clean up Kal_Correct before starting assignment - so many columns it's getting confusing. 
Kal_6 <- Kal_Correct %>%
  select(date.time, AirTemp_C, DewTemp_C, PrecipCorr, PrecipCorr_cumsum, Wind_ms, Precip_Snow_Dew, Precip_Snow_DewCum,
         SnowDensFix, SnowDepth)

# adding a few Kal_Albedo columns to Kal_6
Kal_6$albedo <- Kal_Albedo$actual_al
Kal_6$FreshSnow_m <- Kal_Albedo$Snow_Fresh

# use Kal_6 for this assignment

####################################################
#Question 2

#density at peak SWE is 315.2 kg/m^3 aka density max (from Black Mtn SNOTEL data)
#using first order exponential function to model densification of snowpack
