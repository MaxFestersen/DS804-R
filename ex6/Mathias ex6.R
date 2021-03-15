#### Exercise 6-5 ####


#### Exercise 6-6 ####

#(a) Create a vector of length 5 containing both positive and negative numbers, using the concatenate (c())
#command.

vector <- (c(1,5,15,-4,-2))

#(b) Find the mean(), max(), min() of the vector. Then compute the mean of the absolute values.
min(vector)
mean(vector)
max(vector)

#(c) Taking a subset of the vector can be done using the following notation:
#vector[1:2] will take the first two elements of the vector (R starts indexing with 1).
#Insert 42 on the third position of the vector you created earlier
vector <-vector+5
print(vector)


install.packages("R.utils")
library("R.utils")

vector <- insert(vector, ats=3 ,values=42 ,useNames=TRUE)
print(vector)

#(d) Create a new vector and build the sum of the two vectors.
vector2 <- (c(4,1,5,20,-7,0))
print(vector+vector2)

#(e) Create a random vector using the rnorm() function with no additional arguments.

random_V <- rnorm(10)
#Calculate the mean - what do you observe?
mean(random_V)
print(random_V)
#nothing special??? it's close to 1?

#Take the last 5 elements of the vector using the indexing described above.
random_V[-1:-5]



#Exercise 6-7 Matrices in R


#(a) Create a 2 × 2 matrix A by row binding vectors using the rbind() command.

v1 <- (c(5,1))
v2 <- (c(1,-6))
matrixA<-rbind(v1,v2)

#(b) Nullify matrix A by adding another matrix that you define.
nul <- (c(0,0))
matrixA*nul


#(c) Double all the values in the original matrix A by multiplication with
#another matrix that you define
v3 <- (c(2,2))
v4 <- (c(2,2))
matrixB<-rbind(v3,v4)

matrixA*matrixB

#Exercise 6-8 Exploration of Datasets in R
#(a) Use the help command to get information on the built-in dataset AirPassengers.
help(AirPassengers)

#(i) Plot the dataset using the plot() command. What do you see? Describe the resulting plot.

Air <- AirPassengers
plot(Air)

#(ii) Create a histogram using the built-in function hist() What do you observe?
hist(Air)
#that Frequency decrease as Air Passengers increase

#(iii) Check the class() and mode() of the dataset. Are these as expected? If you are not sure what
#the mode and class functions do use the help() function.

class(Air)
# Class(Air) returns "TS" which is a Time-Series Objects
mode(Air)
#returns "numeric" (integer and double) which is the most frequent type of data I guess?

#(b) R comes with many historical data sets. One of them is the Titanic dataset. Use the help() function to
#read about the data set. Then make a mosaic plot using the mosaicplot() command. What do you observe?
help("Titanic")
     
mosaicplot(Titanic)
#It gives us an overview of the people who died, their gender, and ther Class.. as 1st, 2st.. class.


#Exercise 6-9 Clustering and Classification on the Iris Data (1 point)
#(a) Load the Iris dataset in R, remove the class attribute. Cluster it using k-means with a reasonable choice
#of k.