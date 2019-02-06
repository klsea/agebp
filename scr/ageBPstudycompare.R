# 2.1.19 KLS
# Linear effect of age by study

# Set working dir and load data
rm(list=ls())
setwd('~/R_Projects/agebp/')
dt <- read.csv('data/SV_DND_long_web.csv')

# Functions to calculate study differences

study1age <- function(data,roi) {
  # Calculate the Age term in model with only Study 1 data
  d0 <- data[which(data$name == roi),]
  d1 <- d0[which(d0$Study == 'SV'),]
  m1 <- lm(BPnd ~ Age, data = d1)
  round(m1$coefficient[2][[1]],3)
}

study2age <- function(data,roi) {
  # Calculate the Age term in model with only Study 2 data
  d0 <- data[which(data$name == roi),]
  d1 <- d0[which(d0$Study == 'DND'),]
  m2 <- lm(BPnd ~ Age, data = d1)
  round(m2$coefficient[2][[1]],3)
}

model_diff <- function(data,roi) {
  # Compare Age terms from model with and without study
  diff <- round(study1age(data,roi) - study2age(data,roi),3)
}

getmode <- function(v) {
  # Calculate the mode of a vector
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Create table with values from all calculations and comparisons by ROI
name <- levels(dt$name)
t <- matrix(nrow=length(name), ncol = 4)

for (i in 1:length(name)) {
  t[i,1] <- name[i]
  t[i,2] <- study1age(dt,name[i])
  t[i,3] <- study2age(dt,name[i])
  t[i,4] <- model_diff(dt,name[i])
}

study_compare <- as.data.frame(t)
colnames(study_compare) <- c('ROI', 'Study 1 effect of Age', 'Study 2 effect of Age', 'Difference')
study_compare$Difference <- as.numeric(as.character(study_compare$Difference))
study_compare_order <- study_compare[order(-abs(study_compare$Difference)),]
write.csv(study_compare_order, 'tables/studycomparison.csv', row.names = FALSE)

mean(study_compare$Difference)
median(study_compare$Difference)
getmode(study_compare$Difference)
