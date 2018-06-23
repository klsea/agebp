# quality control on SV data set
# 6.8.18 KLS

# Open packages, set working directory and functions
rm(list=ls())
library(ggplot2); library(reshape2); library(matrixStats)
setwd("~/R_Projects/agebp/")

# Load data
sv <- read.csv('data/SV_bilateral_PVC_BPnd.csv', header = TRUE)

# ====================================
# Screen for outliers
# ====================================
source('Functions/outlierCut.R')
rois <-colnames(sv)[4:length(sv)]
sv1 <- sv[1:3]
for (r in 1:length(rois)) {
  sv1[r+3] <- outlierCut(sv,get(rois[r]))
}  
colnames(sv1)[4:length(sv)] <- rois

#Count outliers
na_count_sv <- sapply(sv1, function(y) sum(length(which(is.na(y)))))
na_count_sv <- data.frame(na_count_sv)
mean(na_count_sv$na_count_sv)
min(na_count_sv$na_count_sv)
max(na_count_sv$na_count_sv)

# ====================================
#Screen based on white matter average
# ====================================
sv_means <- as.data.frame(cbind(colnames(sv1)[4:40], colMeans(sv1[4:40], na.rm = TRUE), apply(sv1[4:40],2,sd, na.rm = TRUE)))
sv_means$V2 <- as.numeric(as.character(sv_means$V2))
sv_exclude <- ifelse(sv_means$V2 <= sv_means$V2[37], TRUE, FALSE);sv_include <- !sv_exclude
#write.csv(sv_means, '~/Desktop/sv_means.csv', row.names = FALSE)
roi_include <- colnames(sv1)[4:40][sv_include]
write.csv(roi_include, 'data/SV_DND_inclusions.csv', row.names = FALSE)

#How many regions to exclude?  
n_exclude <- table(sv_exclude[1:36])["TRUE"]

# Save data file
write.csv(sv1, 'data/SV_bilateral_PVC_BPnd_clean.csv', row.names = FALSE)
