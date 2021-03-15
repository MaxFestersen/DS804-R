# CTRL +  SHIFT + R to make a section. ------------------------------------


# Exercise 6-6 - Vectors in R ---------------------------------------------


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

# (a)  Create a 2×2 matrix A by row binding vectors using the rbind() command.

vector4 <- c(1,2)
vector5 <- c(2,1)

A <- rbind(vector4, vector5)

# (b)  Nullify matrix A by adding another matrix that you define.


# (c)  Double all the values in the original matrix A by multiplication with another matrix that you define.

B <- matrix(2:2, nrow = 2, ncol = 2)

print(A * B)


# 6-8 Exploration of Datasets in R ----------------------------------------


# >(a) --------------------------------------------------------------------
# Use the help command to get information on
# the built-in dataset AirPassengers.
help(AirPassengers)

# >>(i) -------------------------------------------------------------------
# Plot the dataset using the plot() command.
# What do you see? Describe the resulting plot.
plot(AirPassengers)

# >>(ii) ------------------------------------------------------------------
# Create a histogram using the built-in function hist()
# What do you observe?
hist(AirPassengers)


# >>(iii) -----------------------------------------------------------------
# Check the class() and mode() of the dataset. Are these as expected?
# If you are not sure what the mode and class functions do use the help() function.
class(AirPassengers)

mode(AirPassengers)

# >(b) --------------------------------------------------------------------
# R comes with many historical data sets. One of them is the Titanic dataset.
# Use the help() function to read about the data set.
# Then make a mosaic plot using the mosaicplot() command. What do you observe?
help(Titanic)
mosaicplot(Titanic)

# 6-9 Clustering and Classification on the Iris Data ----------------------

# >(a) --------------------------------------------------------------------
# Load the Iris dataset in R, remove the class attribute.
# Cluster it using k-means with a reasonable choice of k.
help(iris)


irisDB <- iris[-5]

cluster <- kmeans(irisDB, 3, iter.max = 10, nstart = 2)


# >(b) --------------------------------------------------------------------
# Use the clustering result to label the data.
irisDB['Cluster'] <- cluster$cluster


# >(c) --------------------------------------------------------------------
# Create some artificial flower data, that could potentially be Iris flowers.
# Think about how you will do this,
# and what you expect the resulting flowers to be.



# >(d) --------------------------------------------------------------------
# Try the knn-classifier with different values for k,
# and use your generated labeled Iris dataset to classify the artificial query points.

# >(e) --------------------------------------------------------------------
# Try using the original labeled Iris dataset. Does this yield the same result?

# >(f) --------------------------------------------------------------------
# Explain your findings.
