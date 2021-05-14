# Exercise 14 / Classification project ------------------------------------

## Libraries
source("../cluster_quality.R") # For checking quality of fit
library(SciViews) # to use boxplots and scatterplots together with pairs function
library(rpart) # Decision Tree
library(rpart.plot) # Visualizing Decision Tree
library(e1071) # Support Vector Machine
library(neuralnet) # Neural Network
library(lubridate)

# Dataset -----------------------------------------------------------------
# We have chosen the Occupancy dataset: http://archive.ics.uci.edu/ml/datasets/Occupancy+Detection+#

test <- read.table("datatest.txt", sep = ",")
test2 <- read.table("datatest2.txt", sep = ",")
training <- read.table("datatraining.txt", sep = ",")

training$Occupancy <- factor(training$Occupancy) # converting class label to factor
test$Occupancy <- factor(test$Occupancy) # converting class label to factor
training$date <- as.numeric(ymd_hms(training$date)) # Converting date to numeric
test$date <- as.numeric(ymd_hms(test$date)) # Converting date to numeric

# Choice of Algorithms ----------------------------------------------------
pairs(training, diag.panel = panel.boxplot)

## Decision Tree ----------------------------------------------------------
set.seed(44444444)
tree <- rpart(Occupancy ~ .,
              method = "class",
              data = training)
rpart.plot(tree) # A bit simple? Lets try with some control

predictions <- predict(tree, test, type = 'class') # predicting unseen test data
cm <- table(test$Occupancy, predictions) # confusion matrix
cluster_report(cm, cap = "Decision Tree") # Quality measures of Decision tree

control <- rpart.control(minsplit = 128, minbucket = 128/2, cp = 0.001) # for adjusting hyperparameters
tree <- rpart(Occupancy ~ Temperature + Humidity + Light + CO2 + HumidityRatio + date,
# tree <- rpart(Occupancy ~ .,
               method = "class",
               data = training,
              control = control)
rpart.plot(tree) # A bit better.

predictions <- predict(tree, test, type = 'class') # predicting unseen test data
cm <- table(test$Occupancy, predictions) # confusion matrix
cluster_report(cm, cap = "Decision Tree") # Quality measures of Decision tree

## Support Vectors and Margin (SVM)----------------------------------------
svmfit <- svm(Occupancy ~ .,
              data = training,
              type = "C-classification",
              kernel = "radial",
              cost = 10,
              gamma = 0.1,
              scale = TRUE)
summary(svmfit)

plot(svmfit, training, CO2 ~ HumidityRatio,
     slice=list(Humidity=3, Light=4, date=5, Temperature = 6))

predictions <- predict(svmfit, test, type = 'class') # predicting unseen test data
cm <- table(test$Occupancy, predictions) # confusion matrix
cluster_report(cm, cap = "Support-Vector-Machine") # Quality measures of SVM

## Neural Network ----------------------------------------------------------
library(neuralnet)
set.seed(12345689) # Men how, vi glemte 7, men det gør ikke noget, for vi har det sjovt.

net <- neuralnet(Occupancy ~ .,
                 data = training,
                 hidden = 2,
                 linear.output = FALSE, 
                 err.fct = 'ce', 
                 likelihood = TRUE)


plot(net)






## Naïve Bayes ------------------------------------------------------------

