# Exercise 9-1 Clustering Lab-Session -------------------------------------


# (a) Data Preprocessing --------------------------------------------------

# > Download the dataset. -------------------------------------------------
# done - http://archive.ics.uci.edu/ml/datasets/seeds


# > Try and reformat the dataset for use with the tool of your cho --------
df <- read.csv("seeds_dataset.txt", sep = "\t", header = F)

write.csv(df, "seeds_dataset.csv")

df <- read.csv("seeds_dataset.csv")

normalized <- scale(df[-1])


# (b) k-means -------------------------------------------------------------

# > Use a k-means implementation to analyze the seeds dataset. ------------


# > What is a suitable choice for k? --------------------------------------


# Try different k-means variants like MacQueen, Lloyd, Elkan, k-means++ ----


# > you observe any differences or tendencies in the results? --------------



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