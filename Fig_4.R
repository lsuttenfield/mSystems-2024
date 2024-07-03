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
## Note ####
# This is the same data as Figure 1
spi_data <- read.csv("spi_data.csv")

# Manually ordered them into groups
spi_data$sample_id <- factor(spi_data$sample_id, 
                             levels = c("Lys2_ev_1_3", "Lys2_ev_1_4", "Lys2_ev_2_1", "Lys2_ev_2_3", "Lys2_ev_2_6", "Lys2_ev_3_4", "Lys2_ev_3_5", # large deletion
                                        "Lys2_ev_1_5", "Lys2_ev_1_6", "Lys2_ev_2_5", # spacer_mutation
                                        "Lys2_ev_1_1", "Lys2_ev_1_2", "Lys2_ev_2_4", "Lys2_ev_3_2",  # spacer deletion
                                        "Lys2_ev_2_2", "Lys2_ev_3_1", "Lys2_ev_3_3", # cas mutation
                                        "Lys2_ev_3_6", # cas deletion
                                        "Lys2_1", "Lys2_2", "Lys2_3", "Lys2_4", "Lys2_5", "Lys2_6", # ancestral
                                        ))

spi_data$mutation_type <- factor(spi_data$mutation_type, levels=
                                   c("large_deletion", "spacer_mutation", "spacer_deletion",
                                     "cas_mutation", "cas_deletion", "ancestral"))

# Graph ####

spi_data %>% 
  ggplot(aes(x=sample_id,y=spi_burst_infectedCells_perCFU*100))+
  geom_boxplot(aes(group=sample_id,fill=mutation_type),
               show.legend=TRUE,outlier.colour = "white",width = 0.5,alpha=0.35)+
  geom_point(aes(fill=mutation_type),show.legend=FALSE,size=2,shape=21,position=position_jitter(width=0.2))+
  scale_y_continuous(expand=c(0,0),limits = c(0,1.2),breaks=seq(0,1.2,0.2))+
  xlab("")+
  ylab("% induced cells/min")+
  guides(fill = guide_legend(override.aes=list(shape=21)))+
  mytheme+
  theme(axis.text.x=element_text(angle=45,hjust=1))
