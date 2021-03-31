library(tidyverse)

df <- read.csv("seeds_dataset.txt", sep = "\t", header = F)

write.csv(df, "seeds_dataset.csv")

df <- read.csv("seeds_dataset.csv")

normalized <- data.frame(scale(df[-1])) %>% 
  drop_na()

pairs(df[-1], lower.panel = NULL)

varnames <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8")

i = 0
lapply(df[-1], function(x) {
  i <<- i + 1
  hist(x, main = varnames[i])
})

c_lloyd <- kmeans(normalized, 4, algorithm = "Lloyd")
c_macqueen <- kmeans(normalized, 4, algorithm = "MacQueen")
c_forgy <- kmeans(normalized, 4, algorithm = "Forgy")
c_h_w <- kmeans(normalized, 4, algorithm = "Hartigan-Wong")

