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

test <- read.table("datatest.txt", sep = ",")
test2 <- read.table("datatest2.txt", sep = ",")
training <- read.table("datatraining.txt", sep = ",")

training$Occupancy <- factor(training$Occupancy) # converting class label to factor
training$test <- factor(training$test) # converting class label to factor
training$date <- as.numeric(ymd_hms(training$date))


# Choice of Algorithms ----------------------------------------------------
pairs(training[-1], diag.panel = panel.boxplot)

## Decision Tree ----------------------------------------------------------
tree <- rpart(Occupancy ~ ., 
              data = training, 
              method = 'class') # No changes to control element
rpart.plot(tree) # plotting Decision tree


predictions <- predict(tree, test, type = 'class') # predicting unseen test data
cm <- table(test$Occupancy, predictions) # confusion matrix
cluster_report(cm, cap = "Decision Tree") # Quality measures of Decision tree

## Support Vectors and Margin (SVM)----------------------------------------
svmfit <- svm(Occupancy ~ ., data = training, kernel = "linear", cost = 10, scale = FALSE)
print(svmfit)


## Neural Network ----------------------------------------------------------
set.seed(42069701051146) # 420 69 yolo swag



net <- neuralnet(Occupancy ~ Temperature + Humidity + Light + CO2 + HumidityRatio,
                 data = training,
                 hidden = 2,
                 linear.output = FALSE, 
                 err.fct = 'ce', 
                 likelihood = TRUE)


plot(net)






## Naïve Bayes ------------------------------------------------------------

