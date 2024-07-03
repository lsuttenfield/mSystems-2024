# Presets ####
library(ggplot2)
library(dplyr)

mytheme <-   theme(panel.grid.minor=element_blank(), #gets rid of grey and lines in the middle
                   panel.grid.major=element_blank(), #gets rid of grey and lines in the middle
                   panel.background=element_rect(fill="white"),#gets rid of grey and lines in the middle
                   panel.border=element_blank(), #gets rid of square going around the entire graph
                   axis.line = element_line(colour = 'black', linewidth = 0.5),#sets the axis line size
                   axis.ticks=element_line(colour = 'black', linewidth = 0.5), #sets the tick lines
                   axis.title.x = element_text( size=14, color="black"), #size of x-axis title
                   axis.title.y = element_text( size=14, color="black"), #size of y-axis title
                   axis.text.x = element_text( size=11, color="black"), #size of x-axis text
                   axis.text.y = element_text( size=12, color="black"),#size of y-axis text
                   legend.key = element_rect(fill="transparent"),
                   legend.background = element_blank())

# Data ####
d <- read.csv("spi_data_S9.csv")
d$mutation_type <- factor(d$mutation_type, levels = c("large_deletion", "spacer_mutation", "spacer_deletion",
                                                      "cas_mutation", "cas_deletion", "∆CRISPR", "ancestral"))

# Graph ####

d %>% 
  ggplot(aes(x=mutation_type,y=spi*100))+
  geom_boxplot(aes(group=mutation_type, fill=mutation_type),outlier.colour = "white",width = 0.5,alpha=0.35)+
  geom_point(aes(fill=mutation_type),size=2,shape=21,position=position_jitter(width=0.25, height=0))+
  stat_summary(fun="mean",geom = "point", size=3,shape=3)+
  xlab("")+
  ylab("% induced cells/min")+
  scale_y_continuous(limits=c(0,0.5))+
  guides(fill = guide_legend(override.aes=list(shape=21)))+
  mytheme+theme(axis.text.x=element_text(angle=45,hjust=1))
