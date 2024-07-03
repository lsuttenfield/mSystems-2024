# Presets ####
library(ggplot2)
library(dplyr)

mytheme <-   theme(panel.grid.minor=element_blank(), #gets rid of grey and lines in the middle
                   panel.grid.major=element_blank(), #gets rid of grey and lines in the middle
                   panel.background=element_rect(fill="white"),#gets rid of grey and lines in the middle
                   panel.border=element_blank(), #gets rid of square going around the entire graph
                   axis.line = element_line(colour = 'black', size = 0.5),#sets the axis line size
                   axis.ticks=element_line(colour = 'black', size = 0.5), #sets the tick lines
                   axis.title.x = element_text(size=14, color="black"), #size of x-axis title
                   axis.title.y = element_text(size=14, color="black"), #size of y-axis title
                   axis.text.x = element_text( size=11, color="black"), #size of x-axis text
                   axis.text.y = element_text( size=12, color="black"),#size of y-axis text
                   legend.key = element_rect(fill="transparent"),
                   legend.background = element_blank())

# Data ####
d <- read.csv("one_step_growth_curve.csv")

# Graph ####
d %>% 
  ggplot(aes(x=time,y=mean_amount))+
  geom_errorbar(aes(ymin=mean_amount - sd_amount, ymax=mean_amount + sd_amount),width=2)+
  geom_line() + 
  geom_point(size=2) + 
  
  # specs
  ggtitle("one step growth curve of DMS3")+
  ylab("log10(PFU/mL)")+
  xlab("time post infection (mins)")+
  scale_y_continuous(limits=c(3,6), breaks=seq(3,6,0.5))+
  scale_x_continuous(limits=c(10,140),breaks=seq(20,130,10))+
  mytheme
