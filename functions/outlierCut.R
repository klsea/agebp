outlierCut <- function(dt, var) {
  ## Modified from https://www.r-bloggers.com/identify-describe-plot-and-remove-the-outliers-from-the-dataset/
  var_name <- eval(substitute(var),eval(dt))
  outlier <- boxplot.stats(var_name)$out
  var_outlier <- ifelse(var_name %in% outlier, NA, var_name)
  return(var_outlier)
}