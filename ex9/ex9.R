df <- read.csv("seeds_dataset.txt", sep = "\t", header = F)

write.csv(df, "seeds_dataset.csv")

df <- read.csv("seeds_dataset.csv")

normalized <- scale(df[-1])

