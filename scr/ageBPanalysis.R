# 6.9.18 KLS
# update 1.30.19 include sample size in tables
# and use all data 
# manuscript tables 3-8

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
  n <- nrow(dt) - sum(is.na(dt))
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
  roi_table <- as.data.frame(rbind(n, m0_t, m1_t, m2_t))
  colnames(roi_table) <- c('Parameter', roi)
  return(roi_table)
}

# ==============
# Subcortical (T3)
# ==============
d0 <- dt[which(dt$lobe == 'Subcortical'),]
d0$name <- factor(d0$name); name <- levels(d0$name)

# Create table
t <- roi_column(d0, name[1])
for (i in 2:length(name)) {
  t[i+1] <- roi_column(d0, name[i])[2]
}
write.csv(t, 'tables/subcortex.csv', row.names = FALSE)

# ========================
# Medial Frontal lobes and Insula (T4)
# ========================
d0 <- dt[which(dt$lobe == 'Medial_frontal_lobe'),]
d0$name <- factor(d0$name); name <- levels(d0$name)

# Create table
t <- roi_column(d0, name[1])
for (i in 2:length(name)) {
  t[i+1] <- roi_column(d0, name[i])[2]
}
write.csv(t, 'tables/medialfrontal.csv', row.names = FALSE)

# ========================
# Anterior Frontal lobes (T5)
# ========================
d0 <- dt[which(dt$lobe == 'Anterior_frontal_lobe'),]
d0$name <- factor(d0$name); name <- levels(d0$name)

# Create table
t <- roi_column(d0, name[1])
for (i in 2:length(name)) {
  t[i+1] <- roi_column(d0, name[i])[2]
}
write.csv(t, 'tables/anteriorfrontal.csv', row.names = FALSE)

# ========================
# Posterior Frontal lobes (T6)
# ========================
d0 <- dt[which(dt$lobe == 'Posterior_frontal_lobe' | dt$lobe == 'Parietal_lobe'),]
d0$name <- factor(d0$name); name <- levels(d0$name)

# Create table
t <- roi_column(d0, name[1])
for (i in 2:length(name)) {
  t[i+1] <- roi_column(d0, name[i])[2]
}
write.csv(t, 'tables/posteriorfrontal.csv', row.names = FALSE)

# ========================
# Medial Temporal lobes (T7)
# ========================
d0 <- dt[which(dt$lobe == 'Medial_temporal_lobe'),]
d0$name <- factor(d0$name); name <- levels(d0$name)

# Create table
t <- roi_column(d0, name[1])
for (i in 2:length(name)) {
  t[i+1] <- roi_column(d0, name[i])[2]
}
write.csv(t, 'tables/medialtemporal.csv', row.names = FALSE)

# ========================
# Lateral Temporal lobes (T8)
# ========================
d0 <- dt[which(dt$lobe == 'Lateral_temporal_lobe'),]
d0$name <- factor(d0$name); name <- levels(d0$name)

# Create table
t <- roi_column(d0, name[1])
for (i in 2:length(name)) {
  t[i+1] <- roi_column(d0, name[i])[2]
}
write.csv(t, 'tables/lateraltemporal.csv', row.names = FALSE)

