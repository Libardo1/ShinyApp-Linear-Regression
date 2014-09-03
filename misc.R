rm(list=ls(all=TRUE))

# Loading packages
library(ggplot2)
library(reshape2)
library(GGally)
library(predictmeans)
library(pbkrtest)
library(parallel)
library(plyr)
library(car)

# Function for get info from data
datainfo <- function(x) {
  Variable <- names(x)
  Class <- sapply(x, function(y) class(y)[1])
  No.of.level.or.unique.values<- sapply(x, function(x) length(unique(na.omit(x))))
  datainfo <- data.frame(Variable, Class, No.of.level.or.unique.values)
  row.names(datainfo) <- NULL
  return(datainfo)
}

# ggplot2 Colour and theme like highchart
# http://www.r-bloggers.com/ggplot-with-a-highcharts-taste/

colors_hc <- c("#7CB5EC", "#313131", "#F7A35C", "#90EE7E", 
  "#7798BF", "#AAEEEE", "#FF0066", "#EEAAEE", "#55BF3B", "#DF5353")
               
colors_pc <- c("#377EB8", "#E41A1C", "#4DAF4A", "#984EA3",
  "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999")
  
theme_hc <- function(){
  theme(
    text                = element_text(size = 12),
    title               = element_text(hjust=0),
    axis.title.x        = element_text(hjust=.5),
    axis.title.y        = element_text(hjust=.5),
    panel.grid.major.y  = element_line(color='gray', size = .3),
    panel.grid.minor.y  = element_blank(),
    panel.grid.major.x  = element_blank(),
    panel.grid.minor.x  = element_blank(),
    panel.border        = element_blank(),
    panel.background    = element_blank(),
    legend.position     = "bottom",
    legend.title        = element_blank()
  )
}Enter file contents here
