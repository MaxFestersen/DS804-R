# Exercise 9-1 Clustering Lab-Session -------------------------------------
library(tidyverse)
library(dbscan)
library(EMCluster)

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
print("A suitable choice for k would be 3, as there are 3 classes")


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

lloyd <- table(test$lloyd, test$class, dnn = c("Lloyd", "Class"))
macqueen <- table(test$macqueen, test$class, dnn = c("MacQueen", "Class"))
forgy <- table(test$forgy, test$class, dnn = c("Forgy", "Class"))
har_won <- table(test$har_won, test$class, dnn = c("Harting-Wong", "Class"))

lloyd
macqueen
forgy
har_won

prop.table(lloyd)
prop.table(macqueen)
prop.table(forgy)
prop.table(har_won)

# Functions for precision, recall and f1-score
precision <- function(x) {
  tp <- max(x[,1]) + max(x[,2]) + max(x[,3])
  tp_fp <- sum(x)
  tp/tp_fp
}

recall <- function(x, n) {
  tp <- max(x[,1]) + max(x[,2]) + max(x[,3])
  tp/n
}

f1 <- function(x, n) {
  2*(precision(x)*recall(x, n))/(precision(x)+recall(x, n))
}


# (c) EM-clustering: ------------------------------------------------------
# > Run EM clustering on the seeds dataset. -------------------------------
#df.em.1 <- init.EM(df, nclass = 1)
df.em.2 <- init.EM(df[-1], nclass = 2)
df.em.3 <- init.EM(df[-1], nclass = 3)
df.em.4 <- init.EM(df[-1], nclass = 4)
#df.em.5 <- init.EM(df[-1], nclass = 5)
#df.em.1
df.em.2
df.em.3
df.em.4
#df.em.5

# > What is a suitable choice for k? --------------------------------------
print("K can be 2-4. 3 makes the most sence, as it maches the orriginal data (we know there are 3 categories from the data description)")

# > For different choices of k: -------------------------------------------
# >> compare the result to a similar choice of k in the k-means algorithm. -----
df.em.2$nc
kmeans(df[-8], 2, algorithm = "Forgy")$size
df.em.3$nc
kmeans(df[-8], 3, algorithm = "Forgy")$size
df.em.4$nc
kmeans(df[-8], 4, algorithm = "Forgy")$size


# (d) DBSCAN --------------------------------------------------------------
# > Run DBSCAN on the seeds dataset. --------------------------------------
# Testrun
dbscan(df[-8], eps = 1, minPts = 10, weights = NULL, borderPoints = TRUE)


# > Find suitable parameter values for epsilon and minpts. ----------------
print("eps: suitable results at 0.1-1.2")
print("minpts: suitable resulls from 1-26")


# (e) ---------------------------------------------------------------------
# You might also want to try SNN clustering or hierarchical clustering.
# Given your experience with this dataset by now
# – does using these algorithms make sense on this dataset?
sNNclust(x = df[-8], k = 8, minPts = 5, eps = 3.5, borderPoints = TRUE)$cluster

# (f) Comparison ----------------------------------------------------------
# Which type of clustering algorithm
# do you consider the most suitable for this dataset?
print("k-means with floyd algorithm seems to yield the best result.")

# (g) ---------------------------------------------------------------------
# Try a different tool, and start from the preprocessing step again
# to get familiarized with that tool.
