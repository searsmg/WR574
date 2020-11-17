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
Kal9 <- Kal9 %>%
  mutate(Hl_total = Hl_in - Hl_out)

#plot net hourly longwave rad
PLOT = "Net Hourly Longwave Rad"
custombreaks1 <- seq(0, 600, 100)
ggplot(Kal9) + geom_line(aes(x=date.time, y=Hl_total), group=1) + PlotFormat + scale_x_datetime(date_breaks = "2 month", labels = date_format("%b %Y")) + labs(x="Water Year 2020", y="Net Hourly Longwave Radiation (W/m^2)") + scale_y_continuous(breaks = custombreaks1, labels = every_nth(custombreaks1, 2, inverse=TRUE))

ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight)

##################################################################
##################################################################

#question 2 - estimating net hourly shortwave radiation
