library(Rlof)
library(ROCR)
library(readr)
library(tidyverse)


df <- data.frame(lab = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T"),
                 x = c(1, 2, 1, 2, 3, 3, 3, 4, 4, 5, 7, 10, 10, 9, 10, 11, 9, 10, 11, 10),
                 y = c(1, 1, 2, 2, 5, 9, 10, 10, 11, 10, 10, 9, 6, 5, 5, 5, 4, 4, 4, 3))

df$lof_2k <- lof(df[2:3], 2, cores = 4, method = "manhattan")

df$lof_2k[df$lab %in% c("E", "K", "O")]

df$lof_4k <- lof(df[2:3], 4, cores = 4, method = "manhattan")

df$lof_4k[df$lab %in% c("E", "K", "O")]

distance <- as.matrix(dist(df[2:3], "manhattan", T, T))

dimnames(distance) <- list(c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T"),
                           c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T"))

Two_NN_dist <- c()
Four_NN_dist <- c()
sum_2NN <- c()
sum_4NN <- c()
for (i in 1:nrow(distance)) {
  Two_NN_dist <- c(Two_NN_dist, nth(sort(distance[,i]), 3))
  Four_NN_dist <- c(Four_NN_dist, nth(sort(distance[,i]), 5))
  sum_2NN <- c(sum_2NN, sum(sort(distance[,i])[2:3]))
  sum_4NN <- c(sum_4NN, sum(sort(distance[,i])[2:5]))
}

df$`2NN_dist` <- Two_NN_dist
df$`4NN_dist` <- Four_NN_dist
df$aggr_2NN <- sum_2NN
df$aggr_4NN <- sum_4NN

# Exercise 10-3 ----------------------------------------------------------------

dat_s1 <- data.frame(obj = c("a_1", "a_2", "a_3", "a_4", "a_5", "a_6", "a_7", "a_8", "a_9", "a_10"),
                  label = c(0, 0, 0, 0, 1, 0, 0, 0, 1, 0),
                  k1 = c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0),
                  k2 = c(0, 0, 0, 0, 1, 1, 0, 0, 0, 0),
                  k3 = c(0, 0, 0, 0, 1, 1, 1, 0, 0, 0),
                  k4 = c(0, 0, 0, 0, 1, 1, 1, 0, 1, 0))

dat_s2 <- data.frame(obj = c("a_1", "a_2", "a_3", "a_4", "a_5", "a_6", "a_7", "a_8", "a_9", "a_10"),
                     label = c(0, 0, 0, 0, 1, 0, 0, 0, 1, 0),
                     k1 = c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0),
                     k2 = c(0, 0, 0, 0, 0, 0, 0, 1, 1, 0),
                     k3 = c(0, 0, 0, 0, 1, 0, 0, 1, 1, 0),
                     k4 = c(0, 0, 0, 1, 1, 0, 0, 1, 1, 0))

# ROC k1 -----------------------------------------------------------------------
pred <- prediction(dat_s1$label, dat_s1$k1)
perf <- performance(pred, "tpr", "fpr")

auc <- performance(pred, measure = "auc")@y.values[[1]]

plot(perf, main = "ROC-curve, s1", sub = paste("k=1","AUC", auc), colorize = TRUE)


pred <- prediction(dat_s2$label, dat_s2$k1)
perf <- performance(pred, "tpr", "fpr")

auc <- performance(pred, measure = "auc")@y.values[[1]]

plot(perf, main = "ROC-curve, s2", sub = paste("k=1","AUC", auc), colorize = TRUE)

# ROC k2 -----------------------------------------------------------------------
pred <- prediction(dat_s1$label, dat_s1$k2)
perf <- performance(pred, "tpr", "fpr")

auc <- performance(pred, measure = "auc")@y.values[[1]]

plot(perf, main = "ROC-curve", sub = paste("k=1","AUC", auc), colorize = TRUE)

# ROC k3 -----------------------------------------------------------------------
pred <- prediction(dat_s1$label, dat_s1$k3)
perf <- performance(pred, "tpr", "fpr")

auc <- performance(pred, measure = "auc")@y.values[[1]]

plot(perf, main = "ROC-curve, s1", sub = paste("k=3","AUC", auc), colorize = TRUE)

pred <- prediction(dat_s2$label, dat_s2$k3)
perf <- performance(pred, "tpr", "fpr")

auc <- performance(pred, measure = "auc")@y.values[[1]]

plot(perf, main = "ROC-curve, s2", sub = paste("k=3","AUC", auc), colorize = TRUE)

# ROC k4 -----------------------------------------------------------------------
pred <- prediction(dat_s1$label, dat_s1$k4)
perf <- performance(pred, "tpr", "fpr")

auc <- performance(pred, measure = "auc")@y.values[[1]]

plot(perf, main = "ROC-curve", sub = paste("k=4","AUC", auc), colorize = TRUE)

# Exercise 10-4 ----------------------------------------------------------------

X3clusters_and_noise_2d <- read_table2("3clusters-and-noise-2d.csv", 
                                       col_names = FALSE, comment = "#")

cool_lof <- lof(X3clusters_and_noise_2d[1:2], 3, cores = 4)

X3clusters_and_noise_2d$lof_3k <- cool_lof

distance <- as.matrix(dist(X3clusters_and_noise_2d[1:2], "euclidean", T, T))

Two_NN_dist <- c()
Four_NN_dist <- c()
sum_2NN <- c()
sum_4NN <- c()

for (i in 1:nrow(distance)) {
  Two_NN_dist <- c(Two_NN_dist, nth(sort(distance[,i]), 3))
  Four_NN_dist <- c(Four_NN_dist, nth(sort(distance[,i]), 5))
  sum_2NN <- c(sum_2NN, sum(sort(distance[,i])[2:3]))
  sum_4NN <- c(sum_4NN, sum(sort(distance[,i])[2:5]))
}

X3clusters_and_noise_2d$two_NN <- Two_NN_dist
X3clusters_and_noise_2d$four_NN <- Four_NN_dist
X3clusters_and_noise_2d$sum_2NN <- sum_2NN
X3clusters_and_noise_2d$sum_4NN <- sum_4NN

library(dbscan)

optic_cluster <- optics(X3clusters_and_noise_2d[1:2], minPts = 5)

plot(optic_cluster)


plot(X3clusters_and_noise_2d$X1, X3clusters_and_noise_2d$X2)
