# 6.6 - Vectors in R ------------------------------------------------------

# >(a) --------------------------------------------------------------------
# (a) Create a vector of length 5 containing both positive and negative numbers,
# using the concatenate (c()) command.
vector <- (c(1,5,15,-4,-2))


# >(b) --------------------------------------------------------------------
# (b) Find the mean(), max(), min() of the vector. Then compute the mean of the absolute values.
min(vector)
mean(vector)
max(vector)

# >(c) --------------------------------------------------------------------
# (c) Taking a subset of the vector can be done using the following notation:
# vector[1:2] will take the first two elements of the vector
# (R starts indexing with 1).
vector <-vector+5
print(vector)

# Insert 42 on the third position of the vector you created earlier.
  # Using R.utils: (not working as intended)
  # install.packages("R.utils")
  # library("R.utils")
  # vector <- insert(vector, ats=3 ,values=42 ,useNames=TRUE)
  # print(vector)

#Base
vector[3] <- 42

# >(d) --------------------------------------------------------------------
# (d) Create a new vector and build the sum of the two vectors.
vector2 <- (c(4,1,5,20,-7))
print(vector+vector2)
sum(vector+vector2)

# >(e) --------------------------------------------------------------------
#Create a random vector using the rnorm() function with no additional arguments.

random_V <- rnorm(10)
#Calculate the mean - what do you observe?
mean(random_V)
print(random_V)
#nothing special??? it's close to 1?

#Take the last 5 elements of the vector using the indexing described above.
random_V[-1:-5]


