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
formating <- function(x) {
  x$Occupancy <- factor(x$Occupancy) # Factor Occupancy
  x$date <- as.numeric(ymd_hms(x$date)) # Change date from char to date, and then to numeric
  return(x)
}
# We have chosen the Occupancy dataset: http://archive.ics.uci.edu/ml/datasets/Occupancy+Detection+#

test <- read.table("datatest.txt", sep = ",")
test2 <- read.table("datatest2.txt", sep = ",")
training <- read.table("datatraining.txt", sep = ",")

test <- formating(test)
test2 <- formating(test2)
training <- formating(training)


# Choice of Algorithms ----------------------------------------------------
pairs(training, diag.panel = panel.boxplot)

## Decision Tree ----------------------------------------------------------
set.seed(44444444)

# Generated using whatever it feels like
tree <- rpart(Occupancy ~ .,
              method = "class",
              data = training)
rpart.plot(tree) # A bit simple?

# predictions and repport
predictions <- predict(tree, test, type = 'class') # predicting unseen test data
cm <- table(test$Occupancy, predictions) # confusion matrix
cluster_report(cm, cap = "Decision Tree") # Quality measures of Decision tree

# Inital results are good though. No Control is really needed.

# Generated with control parameters
control <- rpart.control(minsplit = 32, minbucket = 32/2, cp = 0.001) # for adjusting hyperparameters
#tree <- rpart(Occupancy ~ Temperature + Humidity + Light + CO2 + HumidityRatio + date,
tree.c <- rpart(Occupancy ~ .,
               method = "class",
               data = training,
              control = control)
rpart.plot(tree.c) # A bit better. Acctually the same now...

# predictions and repport
predictions.c <- predict(tree.c, test, type = 'class') # predicting unseen test data
cm.c <- table(test$Occupancy, predictions.c) # confusion matrix
cluster_report(cm.c, cap = "Decision Tree with control") # Quality measures of Decision tree

# Control did not improve results.

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

