# Calculate Affect Scores
# 6.21.18 KLS

# Open packages, set working directory and functions
rm(list=ls())
setwd('~/R_Projects/agebp/')
library(stringr)

# Read in, clean and combine data
dnd <- read.csv('data/DNDactualAVI.csv')
sv <- read.csv('data/SVactualAVI.csv')
sv$Subject <- paste0('SV_', str_pad(sv$Subject, 3, pad = '0'))

dt <- rbind(dnd, sv)
dt$PosA <- (dt$HAP + dt$P + dt$LAP) / 3
dt$NegA <- (dt$HAN + dt$N + dt$LAN) / 3
dt$aDiff <- dt$PosA - dt$NegA

d0 <- dt[c(1,12)]
rm(dnd, sv, dt)
 
# Calculate Cognitive Scores

#Read in, clean, and combine
dnd <- read.csv('data/dndNeuro.csv')
sv <- read.csv('data/svNeuro.csv')
sv$Subject <- paste0('SV_', str_pad(sv$Subject, 3, pad = '0'))

dnd$ds_scaled <- scale(dnd$total_digit_span)
dnd$ln_scaled <- scale(dnd$letter_number)
dnd$fluid <- (dnd$ds_scaled + dnd$ln_scaled) /2
d1 <- dnd[c(1,12)]
rm(dnd)

sv$ds_scaled <- scale(sv$DigitSpanTotal)
sv$ln_scaled <- scale(sv$LetterNumSeq)
sv$fluid <- (sv$ds_scaled + sv$ln_scaled) /2
d2 <- sv[c(1,11)]
d2$Subject <- factor(d2$Subject)
rm(sv)

d3 <- rbind(d1, d2)
rm(d1,d2)

data <- merge(d0,d3, by= 'Subject')
#write.csv(data, 'ageBPaffectfluid.csv')
rm(d0,d3)

## Read in BP values
bp <- read.csv('data/hiloBPvals.csv')
dt <- merge(bp, data, by = 'Subject')
rm(bp, data)
source('functions/corrTableCI.R')

bp_affect_cog <- corrTableCI(dt[2:6])
