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
all_dat <- read.csv("zong_spi.csv")
all_dat$sample_id <- factor(all_dat$sample_id, 
                            levels = c("Lys2_ev_1_1","Lys2_ev_1_2","Lys2_ev_1_3","Lys2_ev_1_4","Lys2_ev_1_5","Lys2_ev_1_6",
                                       "Lys2_ev_2_1","Lys2_ev_2_2","Lys2_ev_2_3","Lys2_ev_2_4","Lys2_ev_2_5","Lys2_ev_2_6",
                                       "Lys2_ev_3_1","Lys2_ev_3_2","Lys2_ev_3_3","Lys2_ev_3_4","Lys2_ev_3_5","Lys2_ev_3_6",
                                       "Lys2_1","Lys2_2","Lys2_3","Lys2_4","Lys2_5","Lys2_6"))

all_dat$group <- factor(all_dat$group,levels=c("ev_pop_1","ev_pop_2","ev_pop_3","anc_pop_2"))


# Fig S3A ####
all_dat %>% 
  filter(time<9) %>% 
  ggplot(aes(x=sample_id,y=zong_spi))+
  geom_boxplot(aes(group=sample_id,fill=group),outlier.colour = "white",width = 0.5,alpha=0.35)+
  geom_point(aes(fill=group),position=position_jitter(width=0.2), shape=21, size=3)+
  scale_fill_manual(values=c("#CE7B91","#247BA0","#B8D3D1","grey25","black"))+
  scale_color_manual(values=c("#CE7B91","#247BA0","#B8D3D1","grey25","black"))+
  scale_y_continuous(limits=c(0,0.125))+
  ggtitle("Exponential phase")+
  xlab("sample ID")+
  ylab("spontnaneous induction per 1.6 cell generations")+
  #guides(fill=guide_legend(override.aes=list(shape=21)))+
  mytheme+theme(axis.text.x = element_text(angle=45,hjust=1))

# Fig S3B ####
all_dat %>% 
  filter(time>9) %>% 
  ggplot(aes(x=sample_id,y=zong_spi))+
  geom_boxplot(aes(group=sample_id,fill=group),outlier.colour = "white",width = 0.5,alpha=0.35)+
  geom_point(aes(fill=group),position=position_jitter(width=0.2), shape=21, size=3)+
  scale_fill_manual(values=c("#CE7B91","#247BA0","#B8D3D1","grey25","black"))+
  scale_color_manual(values=c("#CE7B91","#247BA0","#B8D3D1","grey25","black"))+
  scale_y_continuous(limits=c(0,0.125))+
  ggtitle("Stationary phase")+
  xlab("sample ID")+
  ylab("spontnaneous induction per 1.6 cell generations")+
  #guides(fill=guide_legend(override.aes=list(shape=21)))+
  mytheme+theme(axis.text.x = element_text(angle=45,hjust=1))


