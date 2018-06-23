# quality control on DND data set
# 6.5.18 KLS

# Open packages, set working directory and functions
rm(list=ls())
library(ggplot2); library(reshape2)
setwd("~/R_Projects/agebp/")

# Load data
dnd <- read.csv('data/DND_bilateral_PVC_BPnd.csv', header = TRUE)

## Remove cerebellum and brainstem
dnd$Cerebellum_bil <- NULL
dnd$Brainstem <- NULL

# ====================================
# Screen for outliers
# ====================================
source('Functions/outlierCut.R')
rois <-colnames(dnd)[4:length(dnd)]
dnd1 <- dnd[1:3]
for (r in 1:length(rois)) {
  dnd1[r+3] <- outlierCut(dnd,get(rois[r]))
}  
colnames(dnd1)[4:length(dnd)] <- rois

#Count outliers
na_count_dnd <- sapply(dnd1, function(y) sum(length(which(is.na(y)))))
na_count_dnd <- data.frame(na_count_dnd)
mean(na_count_dnd$na_count_dnd)
min(na_count_dnd$na_count_dnd)
max(na_count_dnd$na_count_dnd)

# ====================================
#Screen based on white matter average
# ====================================
dnd_means <- as.data.frame(cbind(colnames(dnd1)[4:40], colMeans(dnd1[4:40], na.rm = TRUE), apply(dnd1[4:40],2,sd, na.rm = TRUE)))
dnd_exclude <- ifelse(dnd_means$V2 <= dnd_means$V2[37], TRUE, FALSE)  
#write.csv(dnd_means, '~/Desktop/dnd_means.csv')
#How many regions to exclude?  
n_exclude <- table(dnd_exclude[1:36])["TRUE"]

# Save data file
write.csv(dnd1, 'data/DND_bilateral_PVC_BPnd_clean.csv', row.names = FALSE)
