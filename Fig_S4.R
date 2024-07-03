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

d <- read.csv("spi_statPhase.csv")

d$sample_id <- factor(d$sample_id, levels=c("Lys2_ev_1_1","Lys2_ev_1_2","Lys2_ev_1_3","Lys2_ev_1_4","Lys2_ev_1_5","Lys2_ev_1_6",
                                            "Lys2_ev_2_1","Lys2_ev_2_2","Lys2_ev_2_3","Lys2_ev_2_4","Lys2_ev_2_5","Lys2_ev_2_6",
                                            "Lys2_ev_3_1","Lys2_ev_3_2","Lys2_ev_3_3","Lys2_ev_3_4","Lys2_ev_3_5","Lys2_ev_3_6",
                                            "Lys2_1","Lys2_2","Lys2_3","Lys2_4","Lys2_5","Lys2_6"))

d$group <- factor(d$group, levels=c("ev_pop_1","ev_pop_2","ev_pop_3","anc_pop_2"))

# Graph ####
d %>% 
  ggplot(aes(x=sample_id, y=spi_min))+ 
  geom_hline(aes(yintercept=0), linetype="longdash")+
  geom_boxplot(aes(group=sample_id,fill=group),outlier.colour = "white",width = 0.5,alpha=0.35)+
  geom_point(aes(fill=group),show.legend=FALSE,size=2,shape=21,position=position_jitter(width=0.2,height=0.00002))+
  stat_summary(fun="mean",geom = "point", size=3,shape=3)+
  xlab("sample ID")+
  ylab("% induced cells/min")+
  scale_y_continuous(limits=c(-4e-4, 1e-3), breaks=seq(-4e-4, 1e-3, 0.0002))+
  scale_fill_manual(values=c("#CE7B91","#247BA0","#B8D3D1","grey30","black"))+  
  mytheme+theme(axis.text.x = element_text(angle=45,hjust=1))
