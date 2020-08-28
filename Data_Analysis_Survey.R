library(ggplot2)
library(lubridate)
library(dplyr)

rm(list=ls())

setwd("C:/Users/sears/Documents/4_Classes_FA20/WR 574/Assignments/Data Analysis Survey")

Max<-read.csv("C:/Users/sears/Documents/4_Classes_FA20/WR 574/Assignments/Data Analysis Survey/WY_PeakSWE.csv", header=TRUE)%>%
  mutate(Date = mdy(Date))

TS<-read.csv("C:/Users/sears/Documents/4_Classes_FA20/WR 574/Assignments/Data Analysis Survey/TS_PeakSWE.csv", header=TRUE)%>%
  mutate(date = mdy(date))


  
  ## All 508-compliant color scheme -- 
  colors2 <- c("light blue", "red")


#Height and width 
PlotWidth = 15
PlotHeight = 9

#ggplot theme to control formatting parameters for plots with month on the x-axis
PlotTheme = theme(axis.text=element_text(size=20),    #Text size for axis tick mark labels
                  axis.title.x=element_text(size=24, hjust=0.5, margin=margin(t=20, r=20, b=20, l=20)),               #Text size and alignment for x-axis label
                  axis.title.y=element_text(size=24, vjust=0.5,  margin=margin(t=20, r=20, b=20, l=20)),              #Text size and alignment for y-axis label
                  plot.title=element_text(size=26,face="bold",hjust=0.5, margin=margin(t=20, r=20, b=20, l=20)),      #Text size and alignment for plot title
                  legend.title=element_text(size=24),                                                                    #Text size of legend category labels
                  legend.text=element_text(size=20),                                                                   #Text size of legend title
                  legend.position = "bottom")   


###LinePlot and Max SWE
PLOT ="TS & Max SWE"
ggplot(TS, aes(x=date, y=SWE_mm, group=1)) +
  geom_line(size = 2, colour = "blue", stat="identity") + PlotTheme + labs(x="Date", y="SWE (mm)") + scale_x_date(date_breaks = "1 year", date_labels = "%Y") +  scale_y_continuous(limits=c(floor(0), ceiling(600), breaks=c(0, 100, 200, 300, 400, 500, 600))) + geom_point(data = Max, aes(x=Date, y=Peak_SWE), size = 4, colour="red")

ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight)

Max$MonthDay <- c("Apr-08", "Mar-09", "Mar-12", "Mar-13", "Mar-11", "Mar-10", "Mar-1", "Mar-9", "Apr-4", "Apr-8")

PLOT = "Max SWE"
ggplot(Max, aes(x=Date, y=Peak_SWE, group=1)) + geom_point(size=4, colour="blue") + PlotTheme + labs(x="Year", y="Peak SWE (mm)") + scale_y_continuous(limits=c(floor(100), ceiling(600))) + geom_line(size = 2, colour = "blue", stat="identity") + geom_text(aes(label=MonthDay, hjust=0.5, vjust=-1.5))

ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight)


PLOT = "Max SWE_2"
ggplot(Max, aes(x=Date, y=Peak_SWE, group=1)) + geom_point(size=4, colour="blue") + PlotTheme + labs(x="Year", y="Peak SWE (mm)") + scale_y_continuous(limits=c(floor(100), ceiling(600))) + geom_line(size = 2, colour = "blue", stat="identity") + geom_text(aes(label=MonthDay, hjust=0.5, vjust=-1.5))

ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight)
