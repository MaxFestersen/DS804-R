# Exercise 14 / Classification project ------------------------------------




# Dataset -----------------------------------------------------------------

test <- read.table("datatest.txt", sep = ",")
test2 <- read.table("datatest2.txt", sep = ",")
training <- read.table("datatraining.txt", sep = ",")

training$Occupancy <- factor(training$Occupancy) # converting class label to factor

# Choice of Algorithms ----------------------------------------------------


## Decision Tree ----------------------------------------------------------
library(rpart)
library(rpart.plot)

tree <- rpart(Occupancy ~ Temperature + Humidity + Light + CO2 + HumidityRatio, data = training)
rpart.plot(tree)

## Support Vectors and Margin (SVM)----------------------------------------
library(e1071)

svmfit <- svm(Occupancy ~ ., data = training, kernel = "linear", cost = 10, scale = FALSE)
print(svmfit)



## Neural Network ----------------------------------------------------------
library(neuralnet)
set.seed(12345689) # Men how, vi glemte 7, men det gør ikke noget, for vi har det sjovt.

training$Occupancy <- factor(training$Occupancy)


net <- neuralnet(Occupancy ~ Temperature + Humidity + Light + CO2 + HumidityRatio,
                 data = training,
                 hidden = 2,
                 linear.output = FALSE, 
                 err.fct = 'ce', 
                 likelihood = TRUE)


plot(net)






## Naïve Bayes ------------------------------------------------------------

