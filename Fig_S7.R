# Presets ####
library(tidyverse)

mytheme <-   theme(panel.grid.minor=element_blank(), #gets rid of grey and lines in the middle
                   panel.grid.major=element_blank(), #gets rid of grey and lines in the middle
                   panel.background=element_rect(fill="white"),#gets rid of grey and lines in the middle
                   panel.border=element_blank(), #gets rid of square going around the entire graph
                   axis.line = element_line(colour = 'black', linewidth = 0.5),#sets the axis line size
                   axis.ticks=element_line(colour = 'black', size = 0.5), #sets the tick lines
                   axis.title.x = element_text(size=14, color="black"), #size of x-axis title
                   axis.title.y = element_text(size=14, color="black"), #size of y-axis title
                   axis.text.x = element_text( size=11, color="black"), #size of x-axis text
                   axis.text.y = element_text( size=12, color="black"),#size of y-axis text
                   legend.key = element_rect(fill="transparent"),
                   legend.background = element_blank())

# Data ####
dat <- read.csv("crispr_polylysogeny_data.csv")
dat$CRISPR <- factor(dat$CRISPR, levels=c("Y","N"), labels=c("yes", "no"))
dat$group <- factor(dat$group)
dat$source <- factor(dat$source, labels = c("this study", "Rollie et al"))
dat$mutation_type <- factor(dat$mutation_type, levels=c("none", "cas_deletion", "cas_mutation", 
                                                        "spacer_deletion","spacer_mutation","large_deletion"))


# Graph ####
dat %>% 
  ggplot(aes(x=source,y=count))+
  stat_summary(aes(y=count), fun = "mean", geom= "crossbar", width=0.5)+
  geom_point(aes(fill=group),shape=21, size=4, position=position_jitter(width=0.2, height=0.15))+
  scale_fill_manual(values=c("grey60", "grey30", "#CE7B91","#247BA0","#B8D3D1", "black"))+
  ylab("count of viral insertion sites")+
  mytheme+
  theme(axis.text.x = element_text(color="black", angle=45,hjust=1)) +
  facet_wrap(~mutation_type, nrow=1)
