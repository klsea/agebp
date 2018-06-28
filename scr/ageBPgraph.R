# agebp graphs
# KLS 6.28.18

# Load packages and data
rm(list=ls())
library(ggplot2)
setwd('~/R_Projects/agebp/')
dt <- read.csv('data/SV_DND_long_web.csv')
dt$nStudy[dt$Study == 'SV'] <- 'Study 1'
dt$nStudy[dt$Study == 'DND'] <- 'Study 2'
dt$Age2 <- dt$Age*dt$Age
dt$Study_Sex <- interaction(dt$nStudy, dt$Sex)

#Select regions of interest
d1 <- dt[which(dt$ROI == 'Ventral_striatum_bil' | dt$ROI == 'Putamen_bil'| dt$ROI == 'Hippocampus_bil'| 
                 dt$ROI == 'Amygdala_bil'| dt$ROI == 'Insula_bil'| dt$ROI == 'Subcall_area_bil'),]

# Reorder rois for figure
d1$name_f <- factor(d1$name, levels = c('Ventral striatum', 'Hippocampus', 'Subcallosal area', 'Putamen', 'Amygdala', 'Insula'))

#create graph
agebp_plot <- ggplot(d1, aes(x=Age, y=BPnd)) + geom_point(aes(shape = Study_Sex)) + theme_bw() + 
  scale_shape_manual(values=c(0,1,15,16)) + geom_smooth(method=lm, colour = 'red', fill = 'red') +
  geom_smooth(method=lm, formula = y ~ x + I(x^2), colour = 'blue', fill = 'blue') + 
  facet_wrap(~ name_f, ncol = 3, scales = "free_y")


#save
ggsave(plot=agebp_plot,height=6,width=7,dpi=200, filename="tables/ageBP.pdf", useDingbats=FALSE)
