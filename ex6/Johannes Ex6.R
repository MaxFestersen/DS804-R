# CTRL +  SHIFT + R to make a section. ------------------------------------


# Exercise 6.6 - Vectors in R ---------------------------------------------


# a) Create a vector of length 5 containing both positive and negative numbers

vector1 <- c(1,2,3,-1,-2)

# b) Find the mean(), max(), min() of the vector. 

sprintf("The mean is: %s", mean(vector1))

sprintf("The lowest number is: %d", min(vector1))

sprintf("The highest number is: %s", max(vector1))


# c) Insert 42 on the third position of the vector you created earlier.

vector1[3] <- 42

# d) Create a new vector and build the sum of the two vectors.

vector2 <- c(1,5,3,-1,5)

sprintf("The sum of the two vectors are: ")
print(vector1+vector2)

# f) Create a random vector using the rnorm() function with no additional arguments.

vector3 <- rnorm(20)

# Calculate the mean — what do you observe?

sprintf("The mean is: %s", mean(vector3))

# Take the last 5 elements of the vector using the indexing described above

vector3[15:20]


# Exercise 6-7 - Matrices in R --------------------------------------------


