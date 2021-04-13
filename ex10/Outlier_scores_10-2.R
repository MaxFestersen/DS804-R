library(Rlof)

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

