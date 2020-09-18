# Assignment 2

library(ggplot2)
library(dplyr)
library(scales)
library(Hmisc)
library(lubridate)
library(openair)

setwd("C:/Users/sears/Documents/4_Classes_FA20/WR 574/Assignments/Assignment 2/")

#LULC %s - from NCDC LULC raster and processing in ArcMap
LandCover <- read.csv("C:/Users/sears/Documents/4_Classes_FA20/WR 574/Assignments/Assignment 2/LULC.csv", header=TRUE)

##################################################
#Plot formatting
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
##########################################################################

##Col graph of land cover types or Mill Creek basin
PLOT ="LULC_Plot"
custom_breaks <- seq(0, 100, 5)
ggplot(LandCover, aes(x=Land_Cover, y=Percent)) + geom_bar(stat="identity") + theme_classic() + labs(x="Land Cover", y="Percent") + scale_y_continuous(breaks = custom_breaks, labels = every_nth(custom_breaks, 2, inverse=TRUE)) + geom_errorbarh(aes(xmax=as.numeric(Land_Cover)+0.45,xmin=as.numeric(Land_Cover)-0.45,height=0),position=position_dodge(width=0.9))

ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight)