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
                              classs = c(rep("virginica", 40), rep("versicolor", 60), rep("setosa", 50)))
# >(d) --------------------------------------------------------------------
# Try the knn-classifier with different values for k,
# and use your generated labeled Iris dataset to classify the artificial query points.
library(class) # class has built in knn classifier
?knn

train <- rbind(artificial_data[1:25,1:4,1], artificial_data[1:25,1:4,2], artificial_data[1:25,1:4,3])
test <- rbind(artificial_data[26:50,1:4,1], artificial_data[26:50,1:4,2], artificial_data[26:50,1:4,3])
cl <- factor(c(rep("s",25), rep("c",25), rep("v",25)))

classifier_1 <- knn(train, test, cl, k = 3, prob=TRUE)
classifier_1 # note the equal probability

# >(e) --------------------------------------------------------------------
# Try using the original labeled Iris dataset. Does this yield the same result?
train <- rbind(iris3[1:25,,1], iris3[1:25,,2], iris3[1:25,,3])
test <- rbind(iris3[26:50,,1], iris3[26:50,,2], iris3[26:50,,3])
cl <- factor(c(rep("s",25), rep("c",25), rep("v",25)))

classifier_2 <- knn(train, test, cl, k = 3, prob=TRUE)
classifier_2 # Not the variing propability

# >(f) --------------------------------------------------------------------
# Explain your findings.
cat(paste(
  "When we generated the data set artificial_data it was generated based on the",
  "mean and standard deviation of the whole of the iris dataset, therefore a knn-classifier",
  "can't distinguish the classes from eachother as there is no real difference between them.",
  "When looking at classifier_1 each predicition is made with a probability of 0.33",
  "as there are a third of a chance that it's either of the classes.",
  "When looking at classifer_2 each prediction is made with changing probabilities,",
  "as some points are easier to classify than others.",
  sep="\n"
  ))
