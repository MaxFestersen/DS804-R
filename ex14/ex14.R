# Exercise 14 / Classification project ------------------------------------

## Libraries
source("../cluster_quality.R") # For checking quality of fit
library(SciViews) # to use boxplots and scatterplots together with pairs function
library(rpart) # Decision Tree
library(rpart.plot) # Visualizing Decision Tree
library(e1071) # Support Vector Machine
library(neuralnet) # Neural Network
library(lubridate)
library(nnet)
library(caret)

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
set.seed(44444444)
control <- rpart.control(minsplit = 8, minbucket = 8/2, cp = 0.001) # for adjusting hyperparameters
tree <- rpart(Occupancy ~ .,
               method = "class",
               data = training,
              control = control)
rpart.plot(tree)

predictions <- predict(tree, test, type = 'class') # predicting unseen test data
cm <- table(test$Occupancy, predictions) # confusion matrix
cluster_report(cm, cap = "Decision Tree") # Quality measures of Decision tree

## Support Vectors and Margin (SVM)----------------------------------------
svmfit <- svm(Occupancy ~ ., data = training, kernel = "linear", cost = 10, scale = FALSE)
print(svmfit)


## Neural Network ----------------------------------------------------------
library(neuralnet)
set.seed(12345689) # Men how, vi glemte 7, men det gør ikke noget, for vi har det sjovt.

print(dim(training)); print(dim(test))

netmodel <- neuralnet(Occupancy ~ Temperature + Humidity + Light + CO2 + HumidityRatio,
                 data = training,
                 hidden = 2,
                 linear.output = FALSE, 
                 err.fct = 'ce', 
                 likelihood = TRUE)

plot(netmodel)



#another NNET method
#training parameters
train_params <- trainControl(method = "repeatedcv", number = 2, repeats=1)

#train model
nnet_model <- train(Occupancy ~ Temperature + Humidity + Light + CO2 + HumidityRatio,
                    training,
                    method = "nnet",
                    trControl= train_params,
                    preProcess=c("scale","center")
)

#Baseline Accuracy
prop.table(table(training$Occupancy))


# Predictions on the training set
nnet_predictions_train <-predict(nnet_model, training)

# Confusion matrix on training data
table(training$Occupancy, nnet_predictions_train)
(278+125)/nrow(training)                    


#Predictions on the test set
nnet_predictions_test <-predict(nnet_model, test)

# Confusion matrix on test set
table(test$Occupancy, nnet_predictions_test)
157/nrow(test) 

## Naïve Bayes ------------------------------------------------------------

