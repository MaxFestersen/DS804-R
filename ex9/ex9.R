# Exercise 9-1 Clustering Lab-Session -------------------------------------
library(tidyverse)

# (a) Data Preprocessing --------------------------------------------------

# > Download the dataset. -------------------------------------------------
# done - http://archive.ics.uci.edu/ml/datasets/seeds


# > Try and reformat the dataset for use with the tool of your cho --------
#df <- read.csv("seeds_dataset.txt", sep = "\t", header = F)
#write.csv(df, "seeds_dataset.csv")

# 1. area A,
# 2. perimeter P,
# 3. compactness C = 4*pi*A/P^2,
# 4. length of kernel,
# 5. width of kernel,
# 6. asymmetry coefficient
# 7. length of kernel groove.
# All of these parameters were real-valued continuous.

df <- read.csv("seeds_dataset.csv")[-1]

names(df) <- c("area", "perim", "compact", "len_k", "width", "asym", "len_kg", "class")

normalized <- scale(df)

# > Visualizing some stuffs -----------------------------------------------

pairs(df, lower.panel = NULL)

boxplotter <- function(x) {
  ggplot(df, aes(x)) +
    geom_boxplot() +
    facet_grid(vars(class))
}


i = 0
lapply(df[-8], function(x) {
  i <<- i + 1
  boxplotter(x) +
    ggtitle(names(df)[i])
  })


# (b) k-means -------------------------------------------------------------
# > Use a k-means implementation to analyze the seeds dataset. ------------



# > What is a suitable choice for k? --------------------------------------

# a suitable choice for k would be 3, as there are 3 classes

# Try different k-means variants  -----------------------------------------
# Like MacQueen, Lloyd, Elkan, k-means++
# > you observe any differences or tendencies in the results? --------------

c_lloyd <- kmeans(df[-8], 3, algorithm = "Lloyd")
c_macqueen <- kmeans(df[-8], 3, algorithm = "MacQueen")
c_forgy <- kmeans(df[-8], 3, algorithm = "Forgy")
c_har_won <- kmeans(df[-8], 3)

test <- data.frame(lloyd = c_lloyd$cluster,
                   macqueen = c_macqueen$cluster,
                   forgy = c_forgy$cluster,
                   har_won = c_har_won$cluster,
                   class = df$class)
cor(test)

# (c) EM-clustering: ------------------------------------------------------
# > Run EM clustering on the seeds dataset. -------------------------------


# > What is a suitable choice for k? --------------------------------------


# > For different choices of k: -------------------------------------------
# >> compare the result to a similar choice of k in the k-means algorithm. -----



# (d) DBSCAN --------------------------------------------------------------
# > Run DBSCAN on the seeds dataset. --------------------------------------


# > Find suitable parameter values for epsilon and minpts. ----------------



# (e) ---------------------------------------------------------------------
# You might also want to try SNN clustering or hierarchical clustering.
# Given your experience with this dataset by now
# – does using these algorithms make sense on this dataset?



# (f) Comparison ----------------------------------------------------------
# Which type of clustering algorithm
# do you consider the most suitable for this dataset?


# (g) ---------------------------------------------------------------------
# Try a different tool, and start from the preprocessing step again
# to get familiarized with that tool.
