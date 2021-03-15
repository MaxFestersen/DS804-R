#### Data mining opgaver ####


#### Exercise 6.6 ####

# a
vec <- c(1, 2, -2, -4, 3)

# b
mu <- mean(vec)
max_vec <- max(vec)
min_vec <- min(vec)

# c
vec[3] <- 42

# d
vec_2 <- c(1, 3, -5, 3, 5)

both_vecs <- c(vec, vec_2)

# e
rand_vec <- rnorm(100)

mu_rand <- mean(rand_vec)

rand_vec[-95:0]

#### Exercise 6.7 ####

# a
vec_3 <- c(1,2) 
vec_4 <- c(3,4)

A <- rbind(vec_3, vec_4) 

# b
B <- A * -1 

C <- A + B 

# c
D <- matrix(c(rep(2, 4)), nrow = 2, ncol = 2)

E <- A * D

#### Exercise 6.8 ####

# a
help("AirPassengers")

# i

plot(AirPassengers)

# ii
hist(AirPassengers)

# iii
class(AirPassengers)
mode(AirPassengers)

# The class is time-series which makes sense as it is time-series data
# The mode is numeric which also makes sense as the "data" is counts of AirPassengers in some time interval

# b
help("Titanic")

mosaicplot(Titanic)

#### Exercise 6.9 ####

# a
iris_no_classes <- iris[-5]

cluster <- kmeans(iris_no_classes, 3)

# b
iris_no_classes['Cluster'] <- cluster$cluster

artificial_data <- data.frame(Sepal.Length = rnorm(mean = mean(iris$Sepal.Length), sd = sd(iris$Sepal.Length), 150),
                              Sepal.width = rnorm(mean = mean(iris$Sepal.Width), sd = sd(iris$Sepal.Width), 150),
                              Petal.width = rnorm(mean = mean(iris$Petal.Width), sd = sd(iris$Petal.Width), 150),
                              Petal.length = rnorm(mean = mean(iris$Petal.Length), sd = sd(iris$Petal.Length), 150))


