# 6.9.18 KLS

# Load packages and functions
rm(list=ls())
library(lmSupport); library(reshape2)

# Load data
setwd('~/R_Projects/agebp/')
dt <- read.csv('data/SV_DND_long_web.csv')

# Write function to calculate percent signal change
calcPercChng <- function(data, roi) {
  d0 <- data[which(data$ROI == roi),]
  m1 <- lm(BPnd ~ Age, data = d0)
  est_20 <- m1$coefficients[1] + 20*m1$coefficients[2]
  est_30 <- m1$coefficients[1] + 30*m1$coefficients[2]
  percChange <- round((((est_30 - est_20) / est_20)*100)[[1]], 2)
}

# =================
# Descriptive Stats
# =================

# Overall Means
d1 <- dcast(dt, Subject + Sex + Age + Study ~ ROI, value.var = 'BPnd')
t <- as.data.frame(cbind(colnames(d1)[5:ncol(d1)], paste0(round(colMeans(d1[5:ncol(d1)], na.rm = TRUE),2), 
                         ' ( ', round(apply(d1[5:ncol(d1)],2,sd, na.rm = TRUE), 2), ' )')))
d2 <- dt[5:7]; d3 <- unique(d2$ROI)
d4 <- d2[match(d3, d2$ROI),]; d5 <- merge(t,d4, by.x = 'V1', by.y = 'ROI')
d6 <- d5[order(d5$lobe),]
colnames(d6) <- c('Region', 'Overall BPND M (SD)', 'name', 'lobe')
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
d7 <- merge(d5,s, by.x = 'Region' , by.y = 'r' )

# ==========================================
# Calculate percent signal change per decade
# ==========================================
t <- matrix(nrow = 0, ncol=2)
for (r in ROI){
  row <- cbind(r, calcPercChng(dt, r))
  t <- rbind(t,row)
}
d8 <- merge(d7,t, by.x = 'Region', by.y = 'r')

d9 <- d8[c(3,2,5:8,4)]
colnames(d9) <- c('Region', 'Overall BPND M (SD)', 'Study 1 BPND M (SD)', 'Study 2 BPND M (SD)', 'Study t-value [95% CI]',
                  '% Change per Decade', 'Lobe')
d9 <- d9[order(d9$Lobe),]
rm(a,r,s,t,row,d0,d5,d6,d7,d8, ROI)

#Save
write.csv(d9, 'tables/descriptive.csv', row.names = FALSE)

# ====================
# Isolate high or low
# ====================

d10 <- d9[c(1,6)]
d10 <- d10[order(d10$`% Change per Decade`),]
low <- d10[1:5,1]
high <- d10[18:22,1]

dt_low <- dt[which(dt$name %in% low),]
dt_high <- dt[which(dt$name %in% high),]
rm(d9,d10, low, high)

source('functions/SummarySE.R')
low <- summarySE(dt_low, measurevar = 'BPnd', groupvars = 'Subject', na.rm = TRUE)[c(1,3)]
low$lowBPavg <- low$BPnd; low$BPnd <- NULL
high <- summarySE(dt_high, measurevar = 'BPnd', groupvars = 'Subject', na.rm = TRUE)[c(1,3)]
high$highBPavg <- high$BPnd; high$BPnd <- NULL
data <- merge(low, high, by='Subject')
data$BPdiff <- data$highBPavg - data$lowBPavg

write.csv(data, 'data/hiloBPvals.csv', row.names = FALSE)

