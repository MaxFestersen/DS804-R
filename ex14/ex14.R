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
library(tidyverse)
library(caTools)
library(usefun) # Used for pretty print

# Dataset -----------------------------------------------------------------
# We have chosen the Occupancy dataset: http://archive.ics.uci.edu/ml/datasets/Occupancy+Detection+#

# > Loading data ----
test <- read.table("datatest.txt", sep = ",")
test2 <- read.table("datatest2.txt", sep = ",")
training <- read.table("datatraining.txt", sep = ",")

# > Formatting data -------------------------------------------------------
# Date might be an issue, as we will never test the results are based on a specific period
formating <- function(x) {
  x$Occupancy <- factor(x$Occupancy) # Factor Occupancy
  x <- x %>%
    rowwise() %>% # Group by each row (to use functions on row level)
    mutate(time = strsplit(date, " ")[[1]][2]) %>% # Split date, and use the time part
    mutate(date = strsplit(date, " ")[[1]][1]) %>% # Split date, and remove time part
    ungroup() # Remove rowwise
  x$date <- ymd(x$date) # Change date from char to time format
  x$time <- hms(x$time) # change time from char to time format
  return(x)
}

test <- formating(test)
test2 <- formating(test2)
training <- formating(training)


# Choice of Algorithms ----------------------------------------------------
pairs(training[-1], diag.panel = panel.boxplot)


## Decision Tree ----------------------------------------------------------
set.seed(44444444)


# > Base test (no control) ------------------------------------------------
# Generated using whatever it feels like
# >> Tree
tree <- rpart(Occupancy ~ .,
              method = "class",
              data = training)
rpart.plot(tree) # A bit simple?

pretty_print_string("The tree looks a bit simple. So let's test the precision.")

# >> predictions and repport
predictions <- predict(tree, test, type = 'class') # predicting unseen test data
cm <- table(test$Occupancy, predictions) # confusion matrix
cluster_report(cm, cap = "Decision Tree") # Quality measures of Decision tree

pretty_print_string("The accuracy is really high with ~97%. What if we add some control parameters?")


# > With some control parameters ------------------------------------------
# >> Control parameter
control <- rpart.control(minsplit = 32, minbucket = 32/2, cp = 0.001) # for adjusting hyperparameters

# >> Tree
tree.c <- rpart(Occupancy ~ .,
               method = "class",
               data = training,
              control = control)
rpart.plot(tree.c)

pretty_print_string("The tree looks a bit better Visually, but did the results improve?")

# >> predictions and repport
predictions.c <- predict(tree.c, test, type = 'class') # predicting unseen test data
cm.c <- table(test$Occupancy, predictions.c) # confusion matrix
cluster_report(cm.c, cap = "Decision Tree with control") # Quality measures of Decision tree

pretty_print_string("The results got way worse with ~79% accuracy. Maybe we choose poorly. Lets test many minpslit values, to see if we can improve.")


# > Testing minsplit values -----------------------------------------------
accuracy.test <- function(minsplit, training, test){
  control <- rpart.control(minsplit = minsplit, minbucket = minsplit/2, cp = 0.001) # for adjusting hyperparameters
  tree <- rpart(Occupancy ~ .,
                method = "class",
                data = training,
                control = control)
  predictions <- predict(tree, test, type = 'class') # predicting unseen test data
  cm <- table(test$Occupancy, predictions) # confusion matrix
  return(sum(diag(cm)) / sum(cm))
}

# >> Looping results
accuracy.arr <- c()
for (i in 1:1001) {
  accuracy.arr <- c(accuracy.arr, accuracy.test(i, training, test))
}
accuracy.arr
max(accuracy.arr) # Best value

pretty_print_string("The result can be matched but not improved.")



# > Tree without light ----------------------------------------------------
pretty_print_string("Light seems to be a dominant attribute (~97% accuracy using only Light). So can we predict a result without it?")


# >> Formatting data - remove light ---------------------------------------
remove.light <- function(x) {
  x <- select(x, -Light)
}



# >> No light - no control ---------------------------------
test.f <- remove.light(test) # f as in filtered
test2.f <- remove.light(test2)
training.f <- remove.light(training)

# >>> Tree
tree.f <- rpart(Occupancy ~ .,
              method = "class",
              data = training.f)
rpart.plot(tree.f)

pretty_print_string("The tree looks a bit more complex now. Let's see the accuracy.")

# >>> predictions and repport
predictions.f <- predict(tree.f, test.f, type = 'class') # predicting unseen test data
cm.f <- table(test$Occupancy, predictions.f) # confusion matrix
cluster_report(cm.f, cap = "Decision Tree without light") # Quality measures of Decision tree

pretty_print_string("The accuracy is way worse with ~69% accuracy. Maybe this time, control parameters could help.")


# >> No light control loop -----------------------------------------------
accuracy.f.arr <- c()
for (i in 1:150) {
  accuracy.f.arr <- c(accuracy.f.arr, accuracy.test(i, training.f, test.f))
}
accuracy.f.arr
which(max(accuracy.f.arr) == accuracy.f.arr) # best results in list

pretty_print_string("Minsplit of 92-121 gives the best predictions in the set range.")

# >>> No light m=92 -----------------------------------------------------
control.c.f.92 <- rpart.control(minsplit = 92, minbucket = 92/2, cp = 0.001) # for adjusting hyperparameters
# >>>> Tree
tree.c.f.92 <- rpart(Occupancy ~ .,
                  method = "class",
                  data = training.f,
                  control = control.c.f.92)
rpart.plot(tree.c.f.92)

pretty_print_string("The tree is also somewhat more complex.")

# >>>> predictions and repport
predictions.c.f.92 <- predict(tree.c.f.92, test.f, type = 'class') # predicting unseen test data
cm.c.f.92 <- table(test$Occupancy, predictions.c.f.92) # confusion matrix
cluster_report(cm.c.f.92, cap = "Decision Tree without light and minsplit = 92") # Quality measures of Decision tree

pretty_print_string("The result did improve, but only by a bit. It seems the minslpit results could improve at higher values. So let's try that.")


# >> No light control loop 2 -------------------------------------------
for (i in 1:1000) {
  accuracy.f.arr <- c(accuracy.f.arr, accuracy.test(i, training.f, test.f))
}

accuracy.f.arr
which(max(accuracy.f.arr) == accuracy.f.arr)

pretty_print_string("Minsplit between 776 and 1071 give better values, but many values between do not.")



# >>> No light m=776 --------------------------------------------------
control.c.f.776 <- rpart.control(minsplit = 776, minbucket = 776/2, cp = 0.001) # for adjusting hyperparameters
# >>>> Tree
tree.c.f.776 <- rpart(Occupancy ~ .,
                method = "class",
                data = training.f,
                control = control.c.f.776)
rpart.plot(tree.c.f.776)

pretty_print_string("The model is very simple now, using only C02 attribute.")


# >>>> predictions and repport
predictions.c.f.776 <- predict(tree.c.f.776, test.f, type = 'class') # predicting unseen test data
cm.c.f.776 <- table(test$Occupancy, predictions.c.f.776) # confusion matrix
cluster_report(cm.c.f.776, cap = "Decision Tree without light and minsplit = 776") # Quality measures of Decision tree

pretty_print_string("Now C02 seems to be the best identifier, and can produce ~87% accuracy alone.")

pretty_print_string("The results are always good. But it seems that one attribute is allways dominant. Which could hint at the other paratmeters havinng less correlation or a too small test set to see futher splits")


# > New data (test 2) -------------------------------------------------------
pretty_print_string("To test prvious claim, lets try the same tests, on another test set. We try the same best paramters.")

# >> Test2 base -----------------------------------------------------------
predictions <- predict(tree, test2, type = 'class') # predicting unseen test data
cm <- table(test2$Occupancy, predictions) # confusion matrix
cluster_report(cm, cap = "Decision Tree") # Quality measures of Decision tree

pretty_print_string("The accuracy is really high with ~99% (~97% before). It's somewhat better than before.")


# >> Test 2 No light base -------------------------------------------------
predictions.f <- predict(tree.f, test2.f, type = 'class') # predicting unseen test data
cm.f <- table(test2$Occupancy, predictions.f) # confusion matrix
cluster_report(cm.f, cap = "Decision Tree without light") # Quality measures of Decision tree

pretty_print_string("The accuracy is better. ~79% accuracy. Beeting testset 1 at ~69% accuracy.")


# >> Test 2 No light m=776 ------------------------------------------------
predictions.c.f.776 <- predict(tree.c.f.776, test2.f, type = 'class') # predicting unseen test data
cm.c.f.776 <- table(test2$Occupancy, predictions.c.f.776) # confusion matrix
cluster_report(cm.c.f.776, cap = "Decision Tree without light and minsplit = 776") # Quality measures of Decision tree

pretty_print_string("The accuracy is about the same as base on testset 2 (~79%). But not as good as the 89% testset 1 reached.")

pretty_print_string("The results are about the same as before. It would seem the decision tree is well optimized for this kind of data now.")

pretty_print_string("Light and C02 are the best parameters. Correlation matrix will be applied later.")


## Support Vectors and Margin (SVM)----------------------------------------
training$time <- as.numeric(training$time)
test$time <- as.numeric(test$time)

svmfit <- svm(Occupancy ~ Temperature + Humidity + Light + CO2 + HumidityRatio + time,
              data = training,
              type = "C-classification",
              kernel = "radial",
              cost = 7,
              gamma = 0.05,
              scale = TRUE)
summary(svmfit)
plot(svmfit, training, CO2 ~ HumidityRatio,
     slice=list(Humidity=3, Light=4, time=5, Temperature = 6))
predictions <- predict(svmfit, test, type = 'class') # predicting unseen test data
cm <- table(test$Occupancy, predictions) # confusion matrix
cluster_report(cm, cap = "Support-Vector-Machine") # Quality measures of SVM


predictions <- predict(svmfit, test2, type = 'class') # predicting unseen test data
cm <- table(test2$Occupancy, predictions) # confusion matrix
cluster_report(cm, cap = "Support-Vector-Machine") # Quality measures of SVM


# 96 % accuracy on test and 90% accuracy on test2
cat(paste("Instead of using the variable date, we formatted it to be two variables date and time",
          "of which we use the time variable, as it is most likely to be generalizable upon a new dataset",
          "sampled at a different point in time.",
          "The SVM classifier was optimized to perform with a 96% accuracy on test-set 1 and 90% accuracy",
          "on test-set 2.", sep = "/n"))

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



# another method
netmodel <- neuralnet(Occupancy ~ Temperature + Humidity + Light + CO2 + HumidityRatio,
                      data = training,
                      hidden = 3,)

#plotting the netmodel
print(netmodel)
plot(netmodel)


x <- mutate(training, date = as.numeric(date))

#taking a test sample from the training data
test_sample <- x[sample(nrow(x), size = 400, replace = FALSE), ]
test_sample <- test_sample[1:7]

#taking a test sample from the test data
test_sample <- test2[sample(nrow(test2), size = 2665, replace = FALSE), ]
test_sample <- test_sample[1:7]

#saves result
net.results <- neuralnet::compute(netmodel, test_sample)
ls(net.results)
print(net.results$net.result)

# display a better version of the results
cleanoutput <- cbind(test_sample,sqrt(test_sample),
                     as.data.frame(net.results$net.result))
colnames(cleanoutput) <- c("Temperature","Humidity","Light","CO2", "HumidityRatio","Occupancy",
                           "expected Temperature","expected Humidity","expected Light","expected CO2","expected HumidityRatio","expected Occupancy",
                           "Neural Net Output")
print(cleanoutput)

actual_vs_predicted <-select(cleanoutput, "Occupancy","expected Occupancy")
table1 <-table(actual_vs_predicted)
#confusion matirx
print(table1)

#overall accuracy
print(sum(diag(table1))/sum(table1))

#incorrect classification
print(1-sum(diag(table1))/sum(table1))


## Naïve Bayes ------------------------------------------------------------

set.seed(120)  # Setting Seed
classifier_cl <- naiveBayes(Occupancy ~ ., data = training)
classifier_cl

# Predicting on test data
y_pred <- predict(classifier_cl, newdata = test)

# Confusion Matrix
cm <- table(test$Occupancy, y_pred)
cm

# Model Evauation
confusionMatrix(cm)
