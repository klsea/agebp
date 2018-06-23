# Examine subval pet neuropsych data
# Corresponds to Table 1 in manuscript
# 4.5.18 KLS updated 6.5.18

# Open packages, set working directory and functions
rm(list=ls())
setwd('~/R_Projects/agebp/')
library(gdata); library(reshape2); library(Hmisc); library(plyr)

# Read in and clean data
pet <- read.csv('data/SV_DND_wide.csv')[1:3]
pet$study <- substr(pet$Subject, 1,3)
pet$study <- gsub('SV_', 'SV', pet$study)
pet <- pet[which(pet$study == 'SV'),]
pet$study <- NULL
pet$Subject <- as.character(pet$Subject)
pet$Subject <- as.numeric(gsub('SV_', '', pet$Subject))

# neuropsych data
neuro <- read.csv('data/DopamineSubjectiveValue_Measures.csv')
names(neuro)[names(neuro)=='Total.'] <- 'DigitSpanTotal'
names(neuro)[names(neuro)=='Total.Score.'] <- 'LetterNumSeq'
names(neuro)[names(neuro)=='Total.correct..sum.'] <- 'Numeracy'
names(neuro)[names(neuro) == 'X25.30.Minute.Recall'] <- 'paDelayedRecall'
names(neuro)[names(neuro)=='Raw.score'] <- 'ShipleyRaw'

names <- c('Subject.ID', 'DigitSpanTotal', 'LetterNumSeq', 'Numeracy', 'paDelayedRecall', 'ShipleyRaw')
neuropsych <- neuro[names]; 
neuropsych <- neuropsych[which(neuropsych$Subject.ID !='125'),] 

# create and save filtered data
data <- merge(pet, neuropsych, by.x = 'Subject', by.y = 'Subject.ID', all.x = TRUE)
rm(neuro, neuropsych, pet)
data <- data[c(1,3,2,4:8)]
#write.csv(data, 'data/svNeuro.csv', row.names = FALSE)

# Correlation with age
source('functions/corrTableCI.R')
r <- corrTableCI(data[3:length(data)])[,1:2]
colnames(r) <- c('Variable', 'R [95% CI] with age')

# Means and std. dev
library(matrixStats)
n <- colnames(data[3:length(data)])
m <- colMeans(data[3:length(data)], na.rm = TRUE)
s <- colSds(as.matrix(data[3:length(data)]), na.rm=TRUE)

t <- matrix(nrow=0, ncol=2)
u <- matrix(nrow=0, ncol=2)
for (i in 1:length(m)) {
        name <- n[i]
        value <- paste0(round(m[[i]],2), ' (', round(s[i],2), ')')
        row <- c(name, value)
        t <- rbind(t, row)
        n_missing <- sum(is.na(data[i+2]))
        row2 <- c(name, n_missing)
        u <- rbind(u, row2)
}
rm(n,m,s,i, name, value, row)

t <- as.data.frame(t)
u <- as.data.frame(u)
colnames(t) <- c('Variable', 'M (SD)')
colnames(u) <- c('Variable', 'N Missing')

# Gender
m <- table(data$Sex)[[2]]
f <- table(data$Sex)[[1]]
t.test(data$Age~data$Sex)
gender = paste0(as.character(f), "F/", as.character(m), "M")
data.frame('Variable' = 'Gender', 'R [95% CI] with age' = '', 'M (SD)' = gender)

## Create table for paper
table <- merge(r, t)
line <- data.frame('Gender', '', gender)
colnames(line) <- colnames(table)
table <- rbind(table,line)
target <- table$Variable; 
target <- target[c(1,7,2:6)]
table <- table[match(target, table$Variable),]; 
table$Variable <- c('Age', 'Gender', 'Digit Span', 'Letter-Number Sequencing', 'Numeracy', 'Paired Associates Delayed Recall', 
                    'Shipley Vocabulary Subscale')
write.csv(table, 'tables/SVTable1.csv', row.names = FALSE)
rm(r,t,line,target)
