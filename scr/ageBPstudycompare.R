# 2.1.19 KLS
# Linear effect of age by study

# Set working dir and load data
rm(list=ls())
setwd('~/R_Projects/agebp/')
dt <- read.csv('data/SV_DND_long_web.csv')

# Functions to calculate study differences

model0age <- function(data, roi) {
  # Calculate the Age x Study interaction term
  dt <- data[which(data$name == roi),]
  m0 <- lm(BPnd ~ Age + Study + Sex + Age*Study, data = dt)
  ageXstudy <- round(m0$coefficients[5][[1]],3)
}

model1age <- function(data,roi) {
  # Calculate the Age term in model without study
  dt <- data[which(data$name == roi),]
  m1 <- lm(BPnd ~ Age, data = dt)
  round(m1$coefficient[2][[1]],3)
}

model2age <- function(data,roi) {
  # Calculate the Age term in model with study
  dt <- data[which(data$name == roi),]
  m2 <- lm(BPnd ~ Study + Age, data = dt)
  round(m2$coefficient[3][[1]],3)
}

model_diff <- function(data,roi) {
  # Compare Age terms from model with and without study
  diff <- round(model1age(data,roi) - model2age(data,roi),3)
}

# Create table with values from all calculations and comparisons by ROI
name <- levels(dt$name)
t <- matrix(nrow=length(name), ncol = 5)

for (i in 1:length(name)) {
  t[i,1] <- name[i]
  t[i,2] <- model0age(dt,name[i])
  t[i,3] <- model1age(dt,name[i])
  t[i,4] <- model2age(dt,name[i])
  t[i,5] <- model_diff(dt,name[i])
}

study_compare <- as.data.frame(t)
colnames(study_compare) <- c('ROI', 'Age x Study Interaction', 'M1 effect of Age', 'M2 effect of Age', 'Difference')
study_compare$Difference <- as.numeric(as.character(study_compare$Difference))
study_compare_order <- study_compare[order(-abs(study_compare$Difference)),]
write.csv(study_compare_order, 'tables/studycomparison.csv', row.names = FALSE)
