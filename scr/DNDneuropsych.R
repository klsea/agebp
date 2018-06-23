# Examine dnd pet neuropsych data
# Corresponds to Table 1 in manuscript
# 6.1.18 KLS

# Open packages, set working directory and functions
rm(list=ls())
setwd('~/R_Projects/agebp/')
library(gdata); library(reshape2); library(Hmisc); library(plyr)

# Read in and clean data
dt <- read.xls('data/DND_Behavioral_Neuropsych_05_31_18.xlsx')
d0 <- read.csv('data/SV_DND_wide.csv')[1:3]
dt <-merge(d0,dt, by.y='subject_id', by.x='Subject')
rm(d0)

data <- dt[c(1:3,11,12,16,8,15)]
colnames(data)[which(colnames(data) == 'verbal_paired_5')] <- 'pa_delay'
data$pa_delay <- as.integer(as.character(data$pa_delay))

# Add Group
data$Age_Grp <- ifelse(data$Age > 31, "MA", "YA")

# Create and save 
#write.csv(data, '~/Dropbox (MCAB Lab)/MCAB/Drafts/agebp/data/dndNeuro.csv', row.names = FALSE)

# Means and std. dev by group
source('~/Dropbox (Personal)/Functions/mean_sd_table.R')
ya <- data[which(data$Age_Grp == "YA"),]
ma <- data[which(data$Age_Grp == "MA"),]

msdya <- mean_sd_table(ya[c(2,4:8)])
msdma <- mean_sd_table(ma[c(2,4:8)])
colnames(msdya) <- c('Variable', 'M..SD.')
colnames(msdma) <- c('Variable', 'M..SD.')

# Check for group differences
ttest <- matrix(NA, 7, 2)
tbl <- table(data$Sex, data$Age_Grp)
ttest[2,1] <- 'Sex'
ttest[2,2] <- chisq.test(tbl)$statistic
a <- t.test(data$total_digit_span~data$Age_Grp)
ttest[3,1] <- 'total_digit_span'
ttest[3,2] <- paste0(round(a$statistic[[1]],2), ' [ ', round(a$conf.int[1],2), ' , ', round(a$conf.int[2],2), ' ]')
a <- t.test(data$letter_number~data$Age_Grp)
ttest[4,1] <- 'letter_number'
ttest[4,2] <- paste0(round(a$statistic[[1]],2), ' [ ', round(a$conf.int[1],2), ' , ', round(a$conf.int[2],2), ' ]')
a <- t.test(data$numeracy~data$Age_Grp)
ttest[5,1] <- 'numeracy'
ttest[5,2] <- paste0(round(a$statistic[[1]],2), ' [ ', round(a$conf.int[1],2), ' , ', round(a$conf.int[2],2), ' ]')
a <- t.test(data$pa_delay~data$Age_Grp)
ttest[6,1] <- 'pa_delay'
ttest[6,2] <- paste0(round(a$statistic[[1]],2), ' [ ', round(a$conf.int[1],2), ' , ', round(a$conf.int[2],2), ' ]')
a <- t.test(data$shipley~data$Age_Grp)
ttest[7,1] <- 'shipley'
ttest[7,2] <- paste0(round(a$statistic[[1]],2), ' [ ', round(a$conf.int[1],2), ' , ', round(a$conf.int[2],2), ' ]')

ttest <- as.data.frame(ttest)
# Gender
source('Functions/gender_table.R')
ya_g <- gender_table(ya) 
ma_g <- gender_table(ma)

## Create table for paper
ya_table <- rbind(ya_g, msdya)
ma_table <- rbind(ma_g, msdma)

t1 <- merge(ya_table, ma_table, by = 'Variable')
t2 <- merge(t1, ttest, by.x = "Variable", by.y = 'V1', all = TRUE)
colnames(t2) <- c('Variable', 'Younger Adults', 'Middle Aged Adults', 'Group t-value [95% CI]')
t2 <- t2[c(2,1,7,3:6),]
write.csv(t2,'tables/dndTable1.csv', row.names = FALSE)
