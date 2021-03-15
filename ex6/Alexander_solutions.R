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

artificial_data <- data.frame(Sepal.Length = abs(rnorm(mean = mean(iris$Sepal.Length), sd = sd(iris$Sepal.Length), 150)),
                              Sepal.width = abs(rnorm(mean = mean(iris$Sepal.Width), sd = sd(iris$Sepal.Width), 150)),
                              Petal.width = abs(rnorm(mean = mean(iris$Petal.Width), sd = sd(iris$Petal.Width), 150)),
                              Petal.length = abs(rnorm(mean = mean(iris$Petal.Length), sd = sd(iris$Petal.Length), 150)),
                              classs = c(rep("virginica", 40), rep("versicolor", 60), rep("setosa", 50)))

# > d ---------------------------------------------------------------------
library(class)

train <- rbind(artificial_data[1:25,1:4,1], artificial_data[1:25,1:4,2], artificial_data[1:25,1:4,3])
test <- rbind(artificial_data[26:50,1:4,1], artificial_data[26:50,1:4,2], artificial_data[26:50,1:4,3])
cl <- factor(c(rep("s",25), rep("c",25), rep("v",25)))

classifier_1 <- knn(train, test, cl, k = 3, prob=TRUE)

# > e ---------------------------------------------------------------------

train <- rbind(iris3[1:25,,1], iris3[1:25,,2], iris3[1:25,,3])
test <- rbind(iris3[26:50,,1], iris3[26:50,,2], iris3[26:50,,3])
cl <- factor(c(rep("s",25), rep("c",25), rep("v",25)))

classifier_2 <- knn(train, test, cl, k = 3, prob=TRUE)


# > f ---------------------------------------------------------------------

# When we generated the data set artificial_data it was generated based on the
# mean and standard deviation of the whole of the iris dataset, therefore a knn-classifier
# can't distinguish the classes from eachother as there is no real difference between them.
# When looking at classifier_1 each predicition is made with a probability of 0.33
# as there are a third of a chance that it's either of the classes.
# When looking at classifer_2 each prediction is made with changing probabilities,
# as some points are easier to classify than others. 
