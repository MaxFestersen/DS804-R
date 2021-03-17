# Data mining opgaver -----------------------------------------------------


# Exercise 6.6 ------------------------------------------------------------

# > a ----------------------------------------------------------------------

vec <- c(1, 2, -2, -4, 3)

# > b ---------------------------------------------------------------------

mu <- mean(vec)
max_vec <- max(vec)
min_vec <- min(vec)


# > c ---------------------------------------------------------------------

vec[3] <- 42


# > d ---------------------------------------------------------------------

vec_2 <- c(1, 3, -5, 3, 5)

both_vecs <- c(vec, vec_2)

# > e ---------------------------------------------------------------------

rand_vec <- rnorm(100)

mu_rand <- mean(rand_vec)

rand_vec[-95:0]


# Exercise 6.7 ------------------------------------------------------------


# > a ---------------------------------------------------------------------

vec_3 <- c(1,2) 
vec_4 <- c(3,4)

A <- rbind(vec_3, vec_4) 


# > b ---------------------------------------------------------------------

B <- A * -1 

C <- A + B 


# > c ---------------------------------------------------------------------

D <- matrix(c(rep(2, 4)), nrow = 2, ncol = 2)

E <- A * D


# Exercise 6.8 ------------------------------------------------------------


# > a ---------------------------------------------------------------------

help("AirPassengers")

# >> i --------------------------------------------------------------------

plot(AirPassengers)

# >> ii -------------------------------------------------------------------

hist(AirPassengers)


# >> iii ------------------------------------------------------------------

class(AirPassengers)
mode(AirPassengers)

# The class is time-series which makes sense as it is time-series data
# The mode is numeric which also makes sense as the "data" is counts of 
# AirPassengers in some time interval


# > b ---------------------------------------------------------------------

help("Titanic")

mosaicplot(Titanic)

# Exercise 6.9 ------------------------------------------------------------

# > a ---------------------------------------------------------------------

iris_no_classes <- iris[-5]

cluster <- kmeans(iris_no_classes, 3)

# > b ---------------------------------------------------------------------

iris_no_classes['Cluster'] <- cluster$cluster

# > c ---------------------------------------------------------------------

# random data based on the mean and sd of iris, without thinking about different classes

generator <- function(x, n) {
  m <- mean(x)
  s <- sd(x)
  abs(rnorm(n, m, s))
}

artificial_data <- data.frame(lapply(iris[-5], generator, 150),
                              classs = c(rep("virginica", 50), rep("versicolor", 50), rep("setosa", 50)))


# random data based on the mean and sd of iris, thinking about different classes
library(tidyverse)

virginica <- filter(iris, iris$Species == "virginica")
versicolor <- filter(iris, iris$Species == "versicolor")
setosa <- filter(iris, iris$Species == "setosa")

artificial_data_2 <- data.frame(lapply(virginica[-5], generator),
                                Species = rep("virginica")) %>% 
  rbind(data.frame(lapply(versicolor[-5], generator),
                   Species = rep("versicolor"))) %>% 
  rbind(data.frame(lapply(setosa[-5], generator),
                   Species = rep("setosa")))

# > d ---------------------------------------------------------------------
library(class)

rnum <- sample(rep(1:150))
train <- artificial_data[rnum[1:100],-5] 
test <- artificial_data[rnum[101:150],-5]
cl <- artificial_data[rnum[1:100],5]

classifier_1 <- knn(train, test, cl, k = 3, prob=TRUE) # data based on all of iris

rnum <- sample(rep(1:150))
train <- artificial_data_2[rnum[1:100],-5] 
test <- artificial_data_2[rnum[101:150],-5]
cl <- artificial_data_2[rnum[1:100],5]

classifier_1.1 <- knn(train, test, cl, k = 3, prob=TRUE) # data with thought to different classes

# > e ---------------------------------------------------------------------

rnum <- sample(rep(1:150))
train <- iris[rnum[1:100],-5] 
test <- iris[rnum[101:150],-5]
cl <- iris[rnum[1:100],5]

classifier_2 <- knn(train, test, cl, k = 3, prob=TRUE) # iris dataset

# > f ---------------------------------------------------------------------

# When we generated the data set artificial_data it was generated based on the
# mean and standard deviation of the whole of the iris dataset, therefore a knn-classifier
# can't distinguish the classes from eachother as there is no real difference between them.
# When looking at classifier_1 each predicition is made with a probability of 0.33
# as there are a third of a chance that it's either of the classes.
# When looking at classifer_2 each prediction is made with changing probabilities,
# as some points are easier to classify than others. 
