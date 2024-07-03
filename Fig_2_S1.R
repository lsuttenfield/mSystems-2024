# Presets ####
library(ggplot2)
library(dplyr)

mytheme <-   theme(panel.grid.minor=element_blank(), #gets rid of grey and lines in the middle
                   panel.grid.major=element_blank(), #gets rid of grey and lines in the middle
                   panel.background=element_rect(fill="white"),#gets rid of grey and lines in the middle
                   panel.border=element_blank(), #gets rid of square going around the entire graph
                   axis.line = element_line(colour = 'black', linewidth = 0.5),#sets the axis line size
                   axis.ticks=element_line(colour = 'black', linewidth = 0.5), #sets the tick lines
                   axis.title.x = element_text(size=14, color="black"), #size of x-axis title
                   axis.title.y = element_text(size=14, color="black"), #size of y-axis title
                   axis.text.x = element_text( size=11, color="black"), #size of x-axis text
                   axis.text.y = element_text( size=12, color="black"),#size of y-axis text
                   legend.key = element_rect(fill="transparent"),
                   legend.background = element_blank())

# Data ####
bre_dat <- read.csv("all_ev_mutations.csv")
bre_dat$CHROM <- factor(bre_dat$CHROM, labels = c("PA14 chromosome"))
bre_dat$POS <- as.numeric(bre_dat$POS)
bre_dat$sample_id <- factor(bre_dat$sample_id, levels=c("PA14_ev_pop1_1", "PA14_ev_pop1_2", "PA14_ev_pop1_3", "PA14_ev_pop1_4", "PA14_ev_pop1_5", "PA14_ev_pop1_6",
                                                        "PA14_ev_pop2_1", "PA14_ev_pop2_2", "PA14_ev_pop2_3", "PA14_ev_pop2_4", "PA14_ev_pop2_5", "PA14_ev_pop2_6",
                                                        "PA14_ev_pop3_1", "PA14_ev_pop3_2", "PA14_ev_pop3_3", "PA14_ev_pop3_4", "PA14_ev_pop3_5", "PA14_ev_pop3_6",
                                                        "Lys2_ev_pop1_1", "Lys2_ev_pop1_2", "Lys2_ev_pop1_3", "Lys2_ev_pop1_4", "Lys2_ev_pop1_5", "Lys2_ev_pop1_6",
                                                        "Lys2_ev_pop2_1", "Lys2_ev_pop2_2", "Lys2_ev_pop2_3", "Lys2_ev_pop2_4", "Lys2_ev_pop2_5", "Lys2_ev_pop2_6",
                                                        "Lys2_ev_pop3_1", "Lys2_ev_pop3_2", "Lys2_ev_pop3_3", "Lys2_ev_pop3_4", "Lys2_ev_pop3_5", "Lys2_ev_pop3_6" ))
bre_dat$group <- factor(bre_dat$group, levels = c(
  "wt_pop_1","wt_pop_2","wt_pop_3",
  "ev_pop_1","ev_pop_2","ev_pop_3"))

bre_dat$type <- factor(bre_dat$type,levels=c("point","deletion", "duplication"))
bre_dat$mutation_type <- factor(bre_dat$mutation_type)
bre_dat$virus <- factor(bre_dat$virus)
bre_dat_PA14 <- filter(bre_dat, CHROM == "PA14 chromosome")


# Fig 2A ####
bre_dat_PA14 %>% 
  filter(!grepl("wt", group)) %>% 
  ggplot(aes(y=sample_id))+ 
  
  # CRISPR lines 
  geom_vline(aes(xintercept=2936644), color="grey", linetype="longdash")+
  geom_vline(aes(xintercept=2936675), color="grey", linetype="longdash")+
  
  # data 
  geom_segment(aes(x=start_del, xend=end_del, y=sample_id, yend=sample_id, color=group))+
  geom_point(aes(x= start_del, fill=group,color=group,shape=type),color="black", size=3)+
  geom_point(aes(x= end_del, fill=group,color=group,shape=type),color="black", size=3)+
  
  # graphing virus shapes
  geom_point( data = filter(bre_dat_PA14, virus=="yes") , aes( x = start_del , shape=type) , size=1.5 , fill="white", color="black", show.legend = FALSE)+
  geom_point( data = filter(bre_dat_PA14, virus=="yes") , aes( x = end_del   , shape=type) , size=1.5 , fill="white", color="black", show.legend = FALSE)+
  
  # specs
  scale_y_discrete(limits=rev)+
  scale_x_continuous(limits=c(1,6.7e6), breaks=seq(1, 6.7e6, 1e6), labels=seq(0, 6.7, 1))+
  scale_fill_manual(values=c("#CE7B91","#247BA0","#B8D3D1","grey25","black"))+  
  scale_color_manual(values=c("#CE7B91","#247BA0","#B8D3D1","grey25","black"))+  
  scale_shape_manual(values=c(21,24,22))+
  xlab("PA14 chromosome (Mb)")+
  ylab("")+
  guides(fill = guide_legend(override.aes=list(shape=21)))+
  mytheme+theme(panel.grid.major.y=element_line(color="gray90"))


# Fig 2B ####
## Cas gene SNPs ####
bre_dat_PA14 %>% 
  filter(sample_id == "Lys2_ev_pop2_2" | sample_id=="Lys2_ev_pop3_1" | sample_id == "Lys2_ev_pop3_3" | sample_id == "Lys2_ev_pop3_6") %>% 
  ggplot(aes(y=sample_id))+ 
  geom_segment(aes(x=start_del, xend=end_del, y=sample_id, yend=sample_id, color=group), size=1)+
  geom_point(aes(x= start_del, fill=group,color=group,shape=type),color="black", size=5)+
  geom_point(aes(x= end_del, fill=group,color=group,shape=type),color="black", size=5)+
  scale_y_discrete(limits=rev)+
  scale_x_continuous(expand = c(0,0),limits=c(2927000,2931000),breaks=seq(2927000,2931000,1000), labels=seq(2927,2931,1))+
  scale_fill_manual(values=c("#247BA0","#B8D3D1","grey25","black"))+  
  scale_color_manual(values=c("#247BA0","#B8D3D1","grey25","black"))+  
  
  scale_shape_manual(values=c(21,24))+
  xlab("PA14 chromosome (kb)")+
  ylab("")+
  mytheme+theme(panel.grid.major.y=element_line(color="gray90"))+
  theme(axis.text.x = element_text( size=11, color="black"))+
  guides(fill = guide_legend(override.aes=list(shape=21)))

## CRISPR2 spacer1 SNPs ####
bre_dat_PA14 %>% 
  filter(grepl("Lys",sample_id)) %>% 
  filter(mutation_type!="large_deletion") %>% #filtering out large deletions to just focus on the SNPs
  filter(grepl("spacer", mutation_type)) %>% 
  ggplot(aes(y=sample_id))+ 
  geom_segment(aes(x=start_del, xend=end_del, y=sample_id, yend=sample_id, color=group), size=1)+
  geom_point(aes(x= start_del, fill=group,color=group,shape=type),color="black", size=5)+
  geom_point(aes(x= end_del, fill=group,color=group,shape=type),color="black", size=5)+
  scale_y_discrete(limits=rev)+
  scale_x_continuous(expand = c(0,0),limits=c(2935000,2937000),breaks=seq(2935000,2937000,1000), labels=seq(2935,2937,1))+
  scale_fill_manual(values=c("#CE7B91","#247BA0","#B8D3D1","grey25","black"))+  
  scale_color_manual(values=c("#CE7B91","#247BA0","#B8D3D1","grey25","black"))+  
  
  scale_shape_manual(values=c(21,24))+
  xlab("PA14 chromosome (kb)")+
  ylab("")+
  mytheme+theme(panel.grid.major.y=element_line(color="gray90"))+
  theme(axis.text.x = element_text( size=11, color="black"))+
  guides(fill = guide_legend(override.aes=list(shape=21)))


# Fig 2C ####

bre_dat_PA14 %>% 
  filter(grepl("Lys",sample_id)) %>% 
  filter(grepl("large_deletion", mutation_type)) %>% 
  ggplot(aes(y=sample_id))+ 
  # geom_vline(xintercept=2926222, linetype="longdash",color="darkblue")+ #CRISPR1 beg
  geom_vline(xintercept=2935415, linetype="longdash",color="grey30")+ #CRISPR2 beg
  geom_vline(xintercept=2936703, linetype="longdash",color="grey30")+ #CRISPR2 end
  
  geom_segment(aes(x=start_del, xend=end_del, y=sample_id, yend=sample_id, color=group),size=1)+
  geom_point(aes(x= start_del, fill=group,color=group,shape=type),color="black",size=3)+
  geom_point(aes(x= end_del, fill=group,color=group,shape=type),color="black",size=3)+
  
  # graphing virus shapes
  geom_point( data = filter(bre_dat_PA14, virus=="yes") , aes( x = start_del , shape=type) , size=1.5 , fill="white", color="black", show.legend = FALSE)+
  geom_point( data = filter(bre_dat_PA14, virus=="yes") , aes( x = end_del   , shape=type) , size=1.5 , fill="white", color="black", show.legend = FALSE)+
  
  scale_y_discrete(limits=rev)+
  scale_x_continuous(expand = c(0,0),limits=c(2800000,3160000),breaks=seq(2800000,3160000,40000),labels=seq(2800,3160,40))+
  scale_fill_manual(values=c("#CE7B91","#247BA0","#B8D3D1","grey25","black"))+  
  scale_color_manual(values=c("#CE7B91","#247BA0","#B8D3D1","grey25","black"))+  
  scale_shape_manual(values=c(24,22))+
  xlab("PA14 chromosome (kb)")+
  ylab("")+
  guides(fill = guide_legend(override.aes=list(shape=21)))+
  mytheme+theme(panel.grid.major.y=element_line(color="gray90"))

# Fig S1 ####
bre_dat_PA14 %>%   
  ggplot(aes(y=sample_id))+ 
  geom_vline(xintercept=2926222, linetype="longdash",color="grey30")+ #CRISPR1 beg
  geom_vline(xintercept=2936703, linetype="longdash",color="grey30")+ #CRISPR2 end
  
  # graph
  geom_segment(aes(x=start_del, xend=end_del, y=sample_id, yend=sample_id, color=group),show.legend = FALSE, size=1, alpha=0.7)+
  geom_point(aes( x = start_del , fill=group , shape=type) , size=5)+
  geom_point(aes( x = end_del   , fill=group , shape=type) , size=5)+
  scale_y_discrete(limits=rev)+
  
  # graphing virus shapes
  geom_point( data = filter(bre_dat_PA14, virus=="yes") , aes( x = start_del , shape=type) , size=2.5, fill="white",color="black" , show.legend = FALSE)+
  geom_point( data = filter(bre_dat_PA14, virus=="yes") , aes( x = end_del   , shape=type) , size=2.5, fill="white",color="black" , show.legend = FALSE)+
  
  # specs
  scale_x_continuous(limits=c(1,6.7e6), breaks=seq(1, 6.7e6, 1e6), labels=seq(0, 6.7, 1))+
  scale_fill_manual(values=c( "grey25", "grey50", "grey75","#CE7B91","#247BA0","#B8D3D1"))+
  scale_color_manual(values=c( "grey25", "grey50", "grey75","#CE7B91","#247BA0","#B8D3D1"))+
  scale_shape_manual(values=c(21,24,22))+
  xlab("PA14 chromosome (Mb)")+
  ylab("")+
  guides(fill = guide_legend(override.aes=list(shape=21)))+
  mytheme+theme(panel.grid.major.y=element_line(color="gray90"))