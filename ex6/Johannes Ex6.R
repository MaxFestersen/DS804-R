# CTRL +  SHIFT + R to make a section. ------------------------------------


# Exercise 6.6 - Vectors in R ---------------------------------------------


# a) Create a vector of length 5 containing both positive and negative numbers

v <- c(1,2,3,-1,-2)

# b) Find the mean(), max(), min() of the vector. 

sprintf("The mean is: %s", mean(v))

sprintf("The lowest number is: %d", min(v))

sprintf("The highest number is: %s", max(v))


# c) Insert 42 on the third position of the vector you created earlier.

v[3] <- 42

# d) Create a new vector and build the sum of the two vectors.

v2 <- c(1,5,3,-1,5)

v3 <- v + v2

