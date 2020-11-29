# WR574 Assignment 9 - Modeling radiation and the energy balance

library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)

rm(list=ls())

setwd("C:/Users/sears/Documents/4_Classes_FA20/WR 574/Assignments/Assignment 9/")

#getting the DFs together
#rm(list= ls()[!(ls() %in% c('Kal9'))])

#Kal <- Kal %>%
  select(date.time, CloudFrac)

#Kal9 <- merge(Kal8_new, Kal, by="date.time")

##################################################################
##################################################################

#Plot formatting

#X-axis labels for plots that are monthly and in order for WY
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

##################################################################
##################################################################

#question 1 - net longwave radiation

# longwave out first - Ess, StefanBoltz constant, and Tss (in Kelvin)

#assign the SB constant
omega <- 5.67 * 10^-8
#assign the Ess (emissivity of snow surface)
Ess <- 1

#convert temp to K, determine Tss, and Hl_out (longwave rad out)
Kal9 <- Kal9 %>%
  mutate(AirTemp_K = AirTemp_C + 273.15) %>%
  mutate(Tss = if_else(AirTemp_C < 0, AirTemp_K, 273.15)) %>%
  mutate(Hl_out = omega * Ess * (Tss^4))

#find Hl_in using AirTemp_K, SB constant, and emissivity of atmos. 
Kal9 <- Kal9 %>%
  mutate(Eatmos = (0.53+(0.065*ea))*(1+(0.4*CloudFrac))) %>%
  mutate(Hl_in = Eatmos*omega*(AirTemp_K^4))

#Hl_total
Kal9filter<- Kal9 %>%
  mutate(Hl_total = Hl_in - Hl_out) %>%
  filter(date.time < "2020-04-15 00:00:00")

#plot net hourly longwave rad
PLOT = "Net Hourly Longwave Rad"
custombreaks1 <- seq(0, 600, 100)
ggplot(Kal9filter) + geom_line(aes(x=date.time, y=Hl_total), group=1) + PlotFormat + scale_x_datetime(date_breaks = "1 month", labels = date_format("%b %Y")) + labs(x="Water Year 2020", y="Net Hourly Longwave Radiation (W/m^2)") + scale_y_continuous(breaks = custombreaks1, labels = every_nth(custombreaks1, 2, inverse=TRUE))

ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight)

##################################################################
##################################################################

#question 2 - estimating net hourly shortwave radiation

# get "J" which is day of year, then determine day angle in radians.
Kal9filter <- Kal9filter %>%
  mutate(Jday = as.numeric(strftime(date.time, format = "%j")),
         DayAngle = (2*pi*(Jday-1))/365) %>%
  mutate(Eo = 1.000110 + (0.34221 * cos(DayAngle)) + (0.001280 * sin(DayAngle)) + (0.000719 * cos (2*DayAngle)) + (0.000077 * sin(2*DayAngle)),
         Dec = 0.006918 - (0.39912 * cos(DayAngle)) + (0.070257 * sin(DayAngle)) - (0.006758 * cos(2*DayAngle)) + (0.000907 * sin(2*DayAngle)) - (0.002697 * cos(3*DayAngle)) + (0.00148 * sin(3*DayAngle))) %>%
  mutate(Dec_deg = (Dec*180)/pi,
         Et = 0.000292 + (0.007264 *  cos(DayAngle)) - (0.12474*sin(DayAngle)) - (0.05684 * cos(2*DayAngle)) - (0.15886 * sin(2*DayAngle)))

# longitude correction= (-114 - 8)/15 then convert to radians to get -0.1419. -114 = longitude of Kalispell MT
Kal9filter$hour <- hour(Kal9filter$date.time)
Kal9filter <- Kal9filter %>%
  mutate(Tsn = (hour + (-0.1419) + Et) - 12,
         Zenith = acos(cos(Dec)*cos(-1.98968)*cos(0.2618*Tsn)+sin(Dec)*sin(-1.98968))) %>%
  mutate(Zenith_use = if_else(cos(Zenith)<0,0,Zenith))

# now actually calculate Hkin
Kal9filter <- Kal9filter %>%
  mutate(Hkin = 1367*Eo*(cos(Dec)*cos(-1.98968)*cos(0.2618*Tsn)+sin(Dec)*sin(-1.98968))*(0.355+(0.68*(1-CloudFrac))))
