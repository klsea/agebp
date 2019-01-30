# 6.9.18 KLS

# Load packages and data
rm(list=ls())
library(lme4); library(lmSupport); library(reshape2); library(Publish)
setwd('~/R_Projects/agebp/')
dt <- read.csv('data/SV_DND_long_web.csv')
dt$Age2 <- dt$Age*dt$Age

# Write functions
est_w_ci <- function(model) {
  coef <- model$coefficients
  ci <- confint(model)
  n = length(ci)/2
  t <- character()
  for (i in 2:n){
    t[i-1] <- paste0(as.character(signif(coef[i], 2)), " [ ", as.character(signif(ci[i], 2)), 
                     " , ", as.character(signif(ci[i+n], 2)), " ]")
  }
  cbind(attributes(coef)[[1]][2:n], t)
}
lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}
stars <- function(stat) {
  if( stat < .001) {
    '***' 
  } else if (stat < .01) {
    '**'
  } else if (stat < .05) {
    '*'
  } else {
    ''
  }
}
roi_column <- function(data, roi) {
  dt <- data[which(data$name == roi),]
  m0 <- lm(BPnd ~ Study + Sex, data = dt)
  m1 <- lm(BPnd ~ Study + Sex + Age, data = dt)
  m2 <- lm(BPnd ~ Study + Sex + Age + Age2, data = dt)
  m0_e <- est_w_ci(m0)
  m0_r <- cbind('R2 ', paste0(round(summary(m0)$adj.r.squared,3), stars(lmp(m0))))
  m0_t <- rbind(m0_e, m0_r)
  m1_e <- est_w_ci(m1)
  m1_r <- cbind('R2 change ', paste0(round(modelCompare(m0,m1)$DeltaR2,3), stars(modelCompare(m0,m1)$p)))
  m1_t <- rbind(m1_e, m1_r)
  m2_e <- est_w_ci(m2)
  m2_r <- cbind('R2 change', paste0(round(modelCompare(m1,m2)$DeltaR2,3), stars(modelCompare(m1,m2)$p)))
  m2_t <- rbind(m2_e, m2_r)
  roi_table <- as.data.frame(rbind(m0_t, m1_t, m2_t))
  colnames(roi_table) <- c('Parameter', roi)
  return(roi_table)
}

# ========================
# Frontal lobes and Insula
# ========================
d0 <- dt[which(dt$lobe == 'Frontal_lobe'),]
d0$name <- factor(d0$name); name <- levels(d0$name)

# Create table
t <- roi_column(d0, name[1])
for (i in 2:length(name)) {
  t[i+1] <- roi_column(d0, name[i])[2]
}
#write.csv(t, 'tables/frontal.csv', row.names = FALSE)

# ==============
# Subcortical and Insula
# ==============
d0 <- dt[which(dt$lobe == 'Subcortical'| dt$lobe == 'Insula_and_cingulate_gyri'),]
d0$name <- factor(d0$name); name <- levels(d0$name)

# Create table
t <- roi_column(d0, name[1])
for (i in 2:length(name)) {
  t[i+1] <- roi_column(d0, name[i])[2]
}
#write.csv(t, 'tables/subcortex.csv', row.names = FALSE)

# ==============
# Temporal lobes
# ==============
d0 <- dt[which(dt$lobe == 'Temporal_lobe'),]
levels(d0$name)[levels(d0$name)==" Superior temporal gyrus anterior part"] <- "Superior temporal gyrus anterior part"
d0$name <- factor(d0$name); name <- levels(d0$name)

# Split into medial and lateral regions
ml_class <- c('L', 'M', 'L', 'M', 'M', 'M', 'L', 'M', 'L', 'L')
a <- data.frame(name, ml_class)
d1 <- merge(d0,a, by='name')
rm(ml_class, a)

# Create table for medial regions
d2 <- d1[which(d1$ml_class == "M"),]
d2$name <- factor(d2$name); name <- levels(d2$name)

t <- roi_column(d2, name[1])
for (i in 2:length(name)) {
  t[i+1] <- roi_column(d2, name[i])[2]
}
#write.csv(t, 'tables/medialtemporal.csv', row.names = FALSE)

# Create table for lateral regions
d2 <- d1[which(d1$ml_class == "L"),]
d2$name <- factor(d2$name); name <- levels(d2$name)

t <- roi_column(d2, name[1])
for (i in 2:length(name)) {
  t[i+1] <- roi_column(d2, name[i])[2]
}
#write.csv(t, 'tables/lateraltemporal.csv', row.names = FALSE)
