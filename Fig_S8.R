# Presets ####
library(tidyverse)

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

# Fig S8 ####
## Data ####
d <- read.csv("cfu_pfu_gen_rearr_Fig_S8.csv")

d$gen_rearr <- factor(d$gen_rearr)
d$type <- factor(d$type)

## Fig S8A ####
d %>% 
  filter(type=="cfu") %>% 
  
  # graph
  ggplot(aes(x=time,y=mean_amount))+
  
  # summary stats
  geom_errorbar(aes(group=gen_rearr, color=gen_rearr), stat = "summary", fun.data = "mean_sdl", fun.args = list(mult = 1), width=0.3)+
  stat_summary(aes(x=time, y=mean_amount, group=gen_rearr, color=gen_rearr), geom="line", fun="mean", alpha=0.5)+
  stat_summary(aes(x=time,y=mean_amount,group=gen_rearr, fill=gen_rearr, shape=gen_rearr),size=2,fun="mean",geom="point")+
  
  # graph specs
  ggtitle("Genome rearrangements are tolerated in evolved lysogens")+
  scale_shape_manual(values=c(21,22,24))+
  scale_y_log10(limits=c(1e4,1e9),breaks=c(1,1e1,1e2,1e3,1e4,1e5,1e6,1e7,1e8,1e9))+
  scale_x_continuous(limits=c(-0.5,7.5), breaks=seq(0,7.5,1))+
  xlab("time (hr)")+
  ylab("CFU/mL")+
  mytheme


## Fig S8B ####
d %>% 
  filter(type=="pfu") %>% 
  
  # graph
  ggplot(aes(x=time,y=mean_amount))+
  
  # summary stats
  geom_errorbar(aes(group=gen_rearr, color=gen_rearr), stat = "summary", fun.data = "mean_sdl", fun.args = list(mult = 1), width=0.3)+
  stat_summary(aes(x=time, y=mean_amount, group=gen_rearr, color=gen_rearr), geom="line", fun="mean", alpha=0.5)+
  stat_summary(aes(x=time,y=mean_amount,group=gen_rearr, fill=gen_rearr, shape=gen_rearr),size=2,fun="mean",geom="point")+
  
  # graph specs
  ggtitle("Genome rearrangements are tolerated in evolved lysogens")+
  scale_shape_manual(values=c(21,22,24))+
  scale_linetype_manual(values=c("solid","longdash"))+
  scale_y_log10(limits=c(1e4,1e9),breaks=c(1,1e1,1e2,1e3,1e4,1e5,1e6,1e7,1e8,1e9))+
  scale_x_continuous(limits=c(-0.5,7.5), breaks=seq(0,7.5,1))+
  xlab("time (hr)")+
  ylab("PFU/mL")+
  mytheme


# Fig S8C ####
## Graph ####
dat <- read.csv("~/Documents/Illinois/1_whitaker_lab/lab_paper_drafts/1_spon_ind_paper/cleaned_data_files/mmc_polylysogens_foldchanges.csv")
str(dat)
dat$sample_id <- factor(dat$sample_id, levels=c("PA14", "Lys2", "∆CRISPR_lys", "Lys2_ev_pop1_1", "Lys2_ev_pop1_3", "Lys2_ev_pop2_3"))
dat$type <- factor(dat$type)

dat %>% 
  
  # graph 
  ggplot(aes(x=sample_id, y=mean_foldchange, fill=type))+
  
  # data 
  stat_summary(fun="mean",geom="bar", width=0.5,color="black",alpha=0.6)+
  geom_point(aes(x=sample_id,y=mean_foldchange),shape=21,size=2,color="black",position=position_jitter(width=0.25))+
  
  # specs
  geom_hline(yintercept = 1, linetype="longdash")+
  xlab("")+
  ylab("mean fold change")+
  scale_y_log10()+
  mytheme+
  theme(axis.text.x = element_text(angle=45,hjust=1))


### Foldchanges from raw data ####
dat <- read.csv("mmc_polylysogeny_raw.csv")
dat$sample_id <- factor(dat$sample_id)
dat$type <- factor(dat$type)
dat$treatment <- factor(dat$treatment)
dat$day <- factor(dat$day)

# creating the data in "mmc_polylysogens_foldchanges.csv" in same folder
trt <- dat %>% filter(treatment=="treated")
untrt <- dat %>% filter(treatment=="untreated")

trtCFU <- trt %>% filter(type=="cfu")
trtPFU <- trt %>% filter(type=="pfu")  

untrtCFU <- untrt %>% filter(type=="cfu")
untrtPFU <- untrt %>% filter(type=="pfu")

length(trtCFU$sample_id) # checking length 
length(trtPFU$sample_id)

length(untrtCFU$sample_id)
length(untrtPFU$sample_id)

trtCFU$foldchange <- trtCFU$amount / untrtCFU$amount
trtPFU$foldchange <- trtPFU$amount / untrtPFU$amount

alltrt <- rbind(trtCFU, trtPFU)

alltrt$sample_id <- factor(alltrt$sample_id, levels=c("PA14","Lys2","dCRISPR_lys","13_1","13_3","14_3"),
                           labels=c("PA14","Lys2","∆CRISPR_lys","Lys2_ev_pop1_1","Lys2_ev_pop1_3","Lys2_ev_pop2_3"))

sum_trt <- summarize(group_by(alltrt, sample_id, type, day), 
                     mean_foldchange = mean(foldchange),
                     sd_foldchange = sd(foldchange))
sum_trt

sum_trt <- na.omit(sum_trt)


### Graph ####

sum_trt %>% 
  filter(day!="R3") %>% # outlier replicate
  ggplot(aes(x=sample_id, y=mean_foldchange, fill=type))+
  stat_summary(fun="mean",geom="bar", width=0.5,color="black",alpha=0.6)+
  geom_point(aes(x=sample_id,y=mean_foldchange),shape=21,size=2,color="black",position=position_jitter(width=0.25))+
  geom_hline(yintercept = 1, linetype="longdash")+
  xlab("")+
  ylab("mean fold change")+
  scale_y_log10()+
  mytheme+
  theme(axis.text.x = element_text(angle=45,hjust=1))
