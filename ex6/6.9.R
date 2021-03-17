# 6-9 Clustering and Classification on the Iris Data ----------------------

# >(a) --------------------------------------------------------------------
# Load the Iris dataset in R, remove the class attribute.
# Cluster it using k-means with a reasonable choice of k.
iris_no_classes <- iris[-5]
cluster <- kmeans(iris_no_classes, 3) # Cluster using kmeans
iris # Observe classes are now missing

# >(b) --------------------------------------------------------------------
# Use the clustering result to label the data.
iris_no_classes['Cluster'] <- cluster$cluster # Add clusters to iris_no_classes
iris_no_classes['species'] <- iris[5] # Add species to iris_no_classes to compare
iris_no_classes


# >(c) --------------------------------------------------------------------
# Create some artificial flower data, that could potentially be Iris flowers.
# Think about how you will do this,
# and what you expect the resulting flowers to be.
print("rnorm can be used to generate normal distributed numbers. IT can however not avoid using negative values, so abs function is used to make values positive.")
print("We expect it to be similar to the iris dataset")
artificial_data <- data.frame(Sepal.Length = abs(rnorm(mean = mean(iris$Sepal.Length), sd = sd(iris$Sepal.Length), 150)),
                              Sepal.width = abs(rnorm(mean = mean(iris$Sepal.Width), sd = sd(iris$Sepal.Width), 150)),
                              Petal.width = abs(rnorm(mean = mean(iris$Petal.Width), sd = sd(iris$Petal.Width), 150)),
                              Petal.length = abs(rnorm(mean = mean(iris$Petal.Length), sd = sd(iris$Petal.Length), 150)),
                              classs = c(rep("virginica", 50), rep("versicolor", 50), rep("setosa", 50)))


# random data based on the mean and sd of iris, thinking about different classes
library(tidyverse)

virginica <- filter(iris, iris$Species == "virginica")
versicolor <- filter(iris, iris$Species == "versicolor")
setosa <- filter(iris, iris$Species == "setosa")

generator <- function(x) {
  m <- mean(x)
  s <- sd(x)
  abs(rnorm(50, m, s))
}

artificial_data_2 <- data.frame(lapply(virginica[-5], generator),
                                Species = rep("virginica")) %>% 
  rbind(data.frame(lapply(versicolor[-5], generator),
                   Species = rep("versicolor"))) %>% 
  rbind(data.frame(lapply(setosa[-5], generator),
                   Species = rep("setosa")))

# >(d) --------------------------------------------------------------------
# Try the knn-classifier with different values for k,
# and use your generated labeled Iris dataset to classify the artificial query points.
library(class) # class has built in knn classifier
?knn

rnum <- sample(rep(1:150))
train <- artificial_data[rnum[1:100],-5] 
test <- artificial_data[rnum[101:150],-5]
cl <- artificial_data[rnum[1:100],5]

classifier_1 <- knn(train, test, cl, k = 3, prob=TRUE) # data based on all of iris
classifier_1

rnum <- sample(rep(1:150))
train <- artificial_data_2[rnum[1:100],-5] 
test <- artificial_data_2[rnum[101:150],-5]
cl <- artificial_data_2[rnum[1:100],5]

classifier_1.1 <- knn(train, test, cl, k = 3, prob=TRUE) # data with thought to different classes

# >(e) --------------------------------------------------------------------
# Try using the original labeled Iris dataset. Does this yield the same result?
rnum <- sample(rep(1:150))
train <- iris[rnum[1:100],-5] 
test <- iris[rnum[101:150],-5]
cl <- iris[rnum[1:100],5]

classifier_2 <- knn(train, test, cl, k = 3, prob=TRUE) # iris dataset
classifier_2 # Not the variing propability

# >(f) --------------------------------------------------------------------
# Explain your findings.
cat(paste(
  "When comparing classifier_1 with classifer_2 it's clear that the probabilities",
  "at which predictions are made, are not as high in classifier_1 as in classifier_2.",
  "This happens because the data used in classifier_1, was generated based on the mean and standard deviation",
  "of all of the iris dataset, not taking into account that there is different classes.",
  "Therefore a knn-classifier has a hard time distinguishing the classes from eachother,",
  "as there is no real difference between them.",
  "When we generated the dataset artificial_data_2, the different classes was taken into account,",
  "by using the mean and standard deviation of each class, to generate a representation of the classes.",
  "Therefore, when comparing classifier_1.1 with classifier_2, there isn't much of a difference",
  "in the probability at which predictions are made.",
  sep="\n"
  ))

