gender_table <- function(data) {
  # This assumes the data has a column called "Sex" and it is coded as 
  # males = 1 females = 2
  # Returns a table.
  m <- table(data$Sex)[[1]]
  f <- table(data$Sex)[[2]]
  t.test(data$Age~data$Sex)
  gender = paste0(as.character(f), "F/", as.character(m), "M")
  g <- data.frame('Variable' = 'Gender', 'M (SD)' = gender)
  return (g)
}