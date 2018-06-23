corrTableCI <- function(data) {
        ## Input columns of data you would like to correlate 
        ## Creates a table with 95% confidence intervals
        source('~/Dropbox (Personal)/Functions/CIcorr.R')
        library(Hmisc); #library(Rcmdr)
        correlations <- rcorr(as.matrix(na.omit(data)));
        table <- matrix(0, nrow=length(data),ncol=length(data))
        for (x in 1:length(data)) {
                for (y in 1:length(data)) {
                        ci <- CIcorr(.05,correlations$r[x,y],1,nrow(data))
                        table[x,y] <- paste0(as.character(round(correlations$r[x,y],2)), ' [', as.character(round(ci[1],2)), ', ', as.character(round(ci[2],2)), ']')
                }
        }
        corrTable <- as.data.frame(table); colnames(corrTable) <- colnames(data); corrTable <- cbind(colnames(data), corrTable)
}        