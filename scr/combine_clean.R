# age bp - age and binding potential in subval and dnd data sets
# remove outlies and beging analysis
# 5.17.18 KLS

# Open packages, set working directory and functions
rm(list=ls())
library(reshape2); library(plyr); library(gdata)
setwd("~/R_Projects/agebp/")

# Load data
sv <- read.csv('data/SV_bilateral_PVC_BPnd_clean.csv', header = TRUE)
dnd <- read.csv('data/DND_bilateral_PVC_BPnd_clean.csv', header = TRUE)
dt <- rbind.fill(sv,dnd)
roi_include <- as.character(as.vector(read.csv('data/SV_DND_inclusions.csv', header = TRUE))$x)
dt <- dt[c('Subject', 'Age', 'Sex', roi_include)]

# Cut Occipital Lobes and white matter
dt <- dt[-grep('OL_', colnames(dt))]

rm(dnd,sv, roi_include)
# =============
#### Save #####
# =============
write.csv(dt, 'data/SV_DND_wide.csv', row.names = FALSE)

# Make data long
dt2 <- melt(dt, id.vars=c('Subject', 'Age', 'Sex'), variable.name = 'ROI', value.name = 'BPnd')
write.csv(dt2, 'data/SV_DND_long.csv', row.names = FALSE)


# ========================
# Add full names and lobe
# ========================
l1 <- read.xls('data/Hammers_atlas_details.xlsx')
l2 <- l1[which(l1$X.1 == 'L'),][c(2,6)]
names <- gsub(' left', '', l2$ROI.detail)
hammer <- as.matrix(strsplit(as.character(l2$Hammers.Atlas.ROI), '\\['))
h2 <- matrix(0,length(hammer),2)
for (i in 1:length(hammer)) {
  h2[i,1] <- hammer[[i]][1]
  h2[i,2] <- hammer[[i]][2]
}
rm(l1,l2,hammer,i)
h3 <- cbind(names, h2)
h3 <- as.data.frame(h3); colnames(h3) <- c('name','abbrev', 'lobe')
h3$match <- paste0(h3$abbrev, '_bil'); h3$abbrev <- NULL
s3 <- c('Amygdala','Hippocampus','Pallidum', 'Ventral striatum','Thalamus', 'Caudate nucleus', 'Putamen')
s2 <- c('Amygdala_bil','Hippocampus_bil','Pallidum_bil', 'Ventral_striatum_bil','Thalamus_bil', 'CaudateNucl_bil', 'Putamen_bil')
s1 <- c(rep('Temporal_lobe', 2), rep('Subcortical',5))
s4 <- data.frame(name = s3, lobe = s1,match = s2)
h4 <- rbind(h3,s4)
rm(h2,s1,s2,s3,h3,s4, names)

dt3 <- merge(dt2, h4, by.x = 'ROI', by.y = 'match')
dt4 <- dt3[order(dt3$Subject, dt3$ROI),][c(2:5,1,6,7)]
rm(dt, dt2, dt3, h3)
# ========================
# Add study
# ========================
dt4$Study <- substr(dt4$Subject,1,3); 
dt4$Study <- gsub('_', '', dt4$Study)
dt4$Study <- as.factor(dt4$Study)


write.csv(dt4, 'data/SV_DND_long_web.csv', row.names = FALSE)
write.csv(dt4, '~/shiny/agebp/data/SV_DND_long_web.csv', row.names = FALSE)
