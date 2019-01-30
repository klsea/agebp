# Calculate descriptive stats for BPND - Table 2
# 6.9.18 KLS
# updated 1.27.18 to include all data KLS

# Load packages and functions
rm(list=ls())
library(lmSupport); library(reshape2)

# Load data
setwd('~/R_Projects/agebp/')
dt <- read.csv('data/SV_DND_long_web.csv')

# =================
# Descriptive Stats
# =================

# Overall Means
d1 <- dcast(dt, Subject + Sex + Age + Study ~ ROI, value.var = 'BPnd')
t <- as.data.frame(cbind(colnames(d1)[5:ncol(d1)], paste0(round(colMeans(d1[5:ncol(d1)], na.rm = TRUE),2), 
                         ' ( ', round(apply(d1[5:ncol(d1)],2,sd, na.rm = TRUE), 2), ' )'),
                         colMeans(d1[5:ncol(d1)], na.rm = TRUE)))
d2 <- dt[5:7]; d3 <- unique(d2$ROI)
d4 <- d2[match(d3, d2$ROI),]; d5 <- merge(t,d4, by.x = 'V1', by.y = 'ROI')
d6 <- d5[order(d5$lobe),]
colnames(d6) <- c('Region', 'Overall BPND M (SD)', 'avg BPND', 'name', 'lobe')
ROI <- colnames(d1[5:ncol(d1)])
rm(d2,d3,d4,d5,t)

# Study Means
d2 <- d1[which(d1$Study == 'SV'),]
u <- as.data.frame(cbind(colnames(d2)[5:ncol(d2)], paste0(round(colMeans(d2[5:ncol(d2)], na.rm = TRUE),2), 
                                                          ' ( ', round(apply(d2[5:ncol(d2)],2,sd, na.rm = TRUE), 2), ' )')))
colnames(u) <- c('Region', 'Study 1 BPND M (SD)')
d3 <- d1[which(d1$Study == 'DND'),]
v <- as.data.frame(cbind(colnames(d3)[5:ncol(d3)], paste0(round(colMeans(d3[5:ncol(d3)], na.rm = TRUE),2), 
                                                          ' ( ', round(apply(d3[5:ncol(d3)],2,sd, na.rm = TRUE), 2), ' )')))
colnames(v) <- c('Region', 'Study 2 BPND M (SD)')
d4 <- merge(u,v, by = 'Region')
d5 <- merge(d6,d4, by ='Region')
rm(d1,d2,d3,d4,d6,u,v)

# T-test comparing each study means for each ROI
s <- matrix(nrow = 0, ncol=2)
for (r in ROI){
  a <- t.test(dt$BPnd[dt$ROI == r] ~ dt$Study[dt$ROI == r])
  row <- cbind(r, paste0(round(a$statistic,2), ' [ ', round(a$conf.int[1],2), ' , ', round(a$conf.int[2],2), ' ]'))
  s <- rbind(s,row)
}
d7 <- merge(d5[c(1, 4, 2:3, 6:7)],s, by.x = 'Region' , by.y = 'r' )

# clean up names 
d7$name <- as.character(d7$name)
d7$name[which(d7$Region == 'Ant_TL_med_bil')] <- 'Anterior temporal lobe, medial'
d7$name[which(d7$Region == 'Ant_TL_inf_lat_bil')] <- 'Anterior temporal lobe, lateral'
d7$name[which(d7$Region == 'G_sup_temp_post_bil')] <- 'Superior temporal gyrus, posterior'
d7$name[which(d7$Region == 'G_sup_temp_ant_bil')] <- 'Superior temporal gyrus, anterior'
d7$name[which(d7$Region == 'G_cing_ant_bil')] <- 'Cingulate gyrus, anterior'
d7$name[which(d7$Region == 'G_cing_post_bil')] <- 'Cingulate gyrus, posterior'
d7$name <- as.factor(d7$name)

# reorder by average BPND
d7$`avg BPND` <- as.numeric(as.character(d7$`avg BPND`))
d8 <- d7[order(-d7$`avg BPND`),]
d9 <- d8[c(2:3,5:7)]

colnames(d9) <- c('Region', 'Overall BPND M (SD)', 'Study 1 BPND M (SD)', 'Study 2 BPND M (SD)', 'Study t-value [95% CI]')

#Save
write.csv(d9, 'tables/descriptive.csv', row.names = FALSE)

