# Calculate Difference per Decade, 
# Confidence Intervals, and Forest plot
# 1.25.18 KLS

# Load packages and functions
rm(list=ls())
library(lmSupport); library(reshape2); library(boot); library(forestplot)

# Load data
setwd('~/R_Projects/agebp/')
dt <- read.csv('data/SV_DND_long_web.csv')

# get rois
d1 <- dcast(dt, Subject + Sex + Age + Study ~ ROI, value.var = 'BPnd')
ROI <- colnames(d1[5:ncol(d1)])
rm(d1)

# get roi names
d2 <- dt[5:6]; d3 <- unique(d2$ROI)
d4 <- d2[match(d3, d2$ROI),]
rm(d2,d3)

# clean up names 
d4$name <- as.character(d4$name)
d4$name[which(d4$ROI == 'Ant_TL_med_bil')] <- 'Anterior temporal lobe, medial'
d4$name[which(d4$ROI == 'Ant_TL_inf_lat_bil')] <- 'Anterior temporal lobe, lateral'
d4$name[which(d4$ROI == 'G_sup_temp_post_bil')] <- 'Superior temporal gyrus, posterior'
d4$name[which(d4$ROI == 'G_sup_temp_ant_bil')] <- 'Superior temporal gyrus, anterior'
d4$name[which(d4$ROI == 'G_cing_ant_bil')] <- 'Cingulate gyrus, anterior'
d4$name[which(d4$ROI == 'G_cing_post_bil')] <- 'Cingulate gyrus, posterior'
d4$name <- as.factor(d4$name)

# functions
subsetData <- function(data, roi) {
  d0 <- data[which(data$ROI == roi),]
}

calcPercDiff <- function(data, indices) {
  d <- data[indices,]
  m1 <- lm(BPnd ~ Age, data = d)
  est_20 <- m1$coefficients[1] + 20*m1$coefficients[2]
  est_30 <- m1$coefficients[1] + 30*m1$coefficients[2]
  percDiff <- round((((est_30 - est_20) / est_20)*100)[[1]], 2)
  return (percDiff)
}

# loop thru ROIs
t <- matrix(nrow = 0, ncol=4)
for (r in ROI){
  d0 <- subsetData(dt, r)
  result <- boot(d0, calcPercDiff, R =2000)
  mean <- result$t0
  lci <- round(boot.ci(result, type='bca', index = 1)$bca[4], 2)
  uci <- round(boot.ci(result, type='bca', index = 1)$bca[5], 2)
  t <- rbind(t, cbind(r,mean, lci, uci))
}
rm(d0,result,mean,lci,uci)

# create data table and change columns to numeric
t <- as.data.frame(t)
t$mean <- as.numeric(as.character(t$mean))
t$lci <- as.numeric(as.character(t$lci))
t$uci <- as.numeric(as.character(t$uci))

# forestplot
#forestplot(labeltext = as.matrix(t$r), mean= t$mean, lower = t$lci, upper = t$uci)

# add labels
t0 <- merge(t,d4, by.x = 'r', by.y = 'ROI')
t0 <- t0[c(5,2:4)]

# reorder data
t1 <- t0[order(t0$mean),]
#forestplot(labeltext = as.matrix(t1$name), mean= t1$mean, lower = t1$lci, upper = t1$uci, vertices = TRUE)

# add point estimate and ci to right
tabletext = cbind(as.matrix(t1$name), paste0(t1$mean, ' [', t1$lci, ',', t1$uci, ']'))

#png("Forestplot.png",width=800, height=600)
#postscript("Forestplot.eps", width = 480, height = 480)
#tiff('Forestplot.tiff', width = 600, height = 600)
forestplot(labeltext = tabletext, graph.pos = 2, mean= t1$mean, lower = t1$lci, upper = t1$uci, 
           vertices = TRUE, graphwidth = unit(75,'mm'), xlab = '% difference per decade', 
           txt_gp = fpTxtGp(xlab = gpar(cex=.9), ticks = gpar(cex=.9)), #cex is font size
           xticks = c(5,0,-5,-10, -15, -20))
#dev.off()
# export graph manually in R Studio as eps with width=800, height=800
