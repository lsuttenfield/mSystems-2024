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

# Spontaneous Induction Data ####
spi_data <- read.csv("spi_data.csv")

spi_data$sample_id <- factor(spi_data$sample_id, 
                             levels = c("Lys2_ev_1_1","Lys2_ev_1_2","Lys2_ev_1_3","Lys2_ev_1_4","Lys2_ev_1_5","Lys2_ev_1_6",
                                        "Lys2_ev_2_1","Lys2_ev_2_2","Lys2_ev_2_3","Lys2_ev_2_4","Lys2_ev_2_5","Lys2_ev_2_6",
                                        "Lys2_ev_3_1","Lys2_ev_3_2","Lys2_ev_3_3","Lys2_ev_3_4","Lys2_ev_3_5","Lys2_ev_3_6",
                                        "Lys2_1","Lys2_2","Lys2_3","Lys2_4","Lys2_5","Lys2_6"))

spi_data$group <- factor(spi_data$group,
                         levels=c("ev_pop_1", "ev_pop_2", "ev_pop_3",
                                  "anc_pop_2"))

spi_data$mutation_type <- factor(spi_data$mutation_type, levels=c("ancestral",
                                                                  "cas_mutation", "cas_deletion",
                                                                  "spacer_deletion","spacer_mutation","large_deletion"))

# note ####
# this is how the SPI metric is constructed (already included in the dataset)
# spi_data$spi_burst_infectedCells_perCFU <- (spi_data$V_T2.T1/(41.78939 * (5*60) * spi_data$cfu_geomean))

# Fig 1A ####
s <- spi_data %>% 
  filter(sample_id != "Lys1") %>% 
  filter(sample_id != "Lys2") %>% 
  filter(group!="anc_pop_1") %>% 
  ggplot(aes(x=sample_id,y=spi_burst_infectedCells_perCFU*100))+
  geom_boxplot(aes(group=sample_id,fill=group),outlier.colour = "white",width = 0.5,alpha=0.35)+
  geom_point(aes(fill=group),show.legend=FALSE,size=2,shape=21,position=position_jitter(width=0.25))+
  stat_summary(fun="mean",geom = "point", size=3,shape=3)+
  scale_fill_manual(values=c("#CE7B91","#247BA0","#B8D3D1","grey30","black"))+  
  scale_y_continuous(expand=c(0,0),limits = c(0,1.2),breaks=seq(0,1.2,0.2))+
  xlab("")+
  ylab("% induced cells/min")+
  #ggtitle("spontaneous induction\n(∆free viruses / ( burst size = 50 * ∆time * mean CFU))")+
  guides(fill = guide_legend(override.aes=list(shape=21)))+
  mytheme+theme(axis.text.x=element_text(angle=45,hjust=1))
s


# Growth Curve Data ####
all_dat <- na.omit(read.csv("growth_data.csv"))
all_dat$sample_id <- factor(all_dat$sample_id)
all_dat$group <- factor(all_dat$group,levels=c("ev_pop_1","ev_pop_2","ev_pop_3","anc_pop_2"))
all_dat$type <- factor(all_dat$type)
all_dat$mutation_type <- factor(all_dat$mutation_type,
                                levels=c("ancestral",
                                         "cas\nmutation", "cas\ndeletion",
                                         "spacer\ndeletion","spacer\nmutation", "large\ndeletion"))

str(all_dat)

# Fig 1B ####
expPhase <- all_dat %>% filter(type=="cfu") %>% filter(time<10)
statPhase <- all_dat %>% filter(type=="cfu") %>% filter(time>9)

all_data_cfu <- all_dat %>% 
  filter(type=="cfu") %>% 
  ggplot(aes(x=time,y=mean_amount))+
  geom_line(data=expPhase, aes(color=group,group=sample_id),alpha=0.35)+
  geom_line(data=statPhase, aes(color=group,group=sample_id),alpha=0.35, show.legend = FALSE)+  
  stat_summary(aes(x=time,y=mean_amount,group=sample_id, fill=group),size=2,shape=21,position=position_jitter(),fun="mean",geom="point")+
  
  scale_fill_manual(values=c("#CE7B91","#247BA0","#B8D3D1","grey25","black"))+  
  scale_color_manual(values=c("#CE7B91","#247BA0","#B8D3D1","grey25","black"))+  
  scale_shape_manual(values=c(21,21,21,21,21,24))+
  scale_y_log10(limits=c(1e4,1e11),breaks=c(1,1e1,1e2,1e3,1e4,1e5,1e6,1e7,1e8,1e9,1e10,1e11))+
  scale_x_continuous(limits=c(-1,19),breaks=c(0,2,3,4,5,6,7,10,12,15,18))+
  xlab("time (hr)")+
  ylab("CFU/mL")+
  mytheme

all_data_cfu


# Fig 1C ####
expPhase <- all_dat %>% filter(type=="pfu") %>% filter(time<10)
statPhase <- all_dat %>% filter(type=="pfu") %>% filter(time>9)


all_data_pfu <- all_dat %>% 
  filter(type=="pfu") %>% 
  ggplot(aes(x=time,y=mean_amount))+
  geom_line(data=expPhase, aes(color=group,group=sample_id),alpha=0.35)+
  geom_line(data=statPhase, aes(color=group,group=sample_id),alpha=0.35, show.legend = FALSE)+
  stat_summary(aes(x=time,y=mean_amount,group=sample_id, fill=group),size=2,shape=21,position=position_jitter(),fun="mean",geom="point")+
  
  scale_fill_manual(values=c("#CE7B91","#247BA0","#B8D3D1","grey25","black"))+
  scale_color_manual(values=c("#CE7B91","#247BA0","#B8D3D1","grey25","black"))+
  scale_y_log10(limits=c(1e4,1e11),breaks=c(1,1e1,1e2,1e3,1e4,1e5,1e6,1e7,1e8,1e9,1e10,1e11))+
  scale_x_continuous(limits=c(-1,19),breaks=c(0,2,3,4,5,6,7,10,12,15,18))+
  xlab("time (hr)")+
  ylab("PFU/mL")+
  mytheme

all_data_pfu

