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
    ungroup() %>%  # Remove rowwise
  x$date <- ymd(x$date) # Change date from char to time format
  x$time <- hms(x$time) # change time from char to time format
  x$weekday <- factor(weekdays(x$date))
  x <- rowwise(x) %>% 
    mutate(weekdayNum = as.numeric(ifelse(
    weekday == "mandag",
    1,
    ifelse(weekday == "tirsdag",
           2,
           ifelse(weekday == "onsdag",
                  3,
                  ifelse(weekday == "torsdag",
                         4,
                         ifelse(weekday == "fredag",
                                5,
                                ifelse(weekday == "lørdag",
                                       6,
                                       7
  ))))))))
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

test.f <- remove.light(test) # f as in filtered
test2.f <- remove.light(test2)
training.f <- remove.light(training)

# >> No light - no control ---------------------------------
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
accuracy.f.arr <- c()
for (i in 1:1000) {
  accuracy.f.arr <- c(accuracy.f.arr, accuracy.test(i, training.f, test.f))
}

accuracy.f.arr
which(max(accuracy.f.arr) == accuracy.f.arr)

pretty_print_string("Minsplit between 626 and 921 give better values.")


# >>> No light m=626 --------------------------------------------------
control.c.f.626 <- rpart.control(minsplit = 626, minbucket = 626/2, cp = 0.001) # for adjusting hyperparameters

# >>>> Tree
tree.c.f.626 <- rpart(Occupancy ~ .,
                method = "class",
                data = training.f,
                control = control.c.f.626)
rpart.plot(tree.c.f.626)

pretty_print_string("The model is very simple now, using only C02 attribute.")


# >>>> predictions and repport
predictions.c.f.626 <- predict(tree.c.f.626, test.f, type = 'class') # predicting unseen test data
cm.c.f.626 <- table(test$Occupancy, predictions.c.f.626) # confusion matrix
cluster_report(cm.c.f.626, cap = "Decision Tree without light and minsplit = 626") # Quality measures of Decision tree

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


# >> Test 2 No light m=626 ------------------------------------------------
predictions.c.f.626 <- predict(tree.c.f.626, test2.f, type = 'class') # predicting unseen test data
cm.c.f.626 <- table(test2$Occupancy, predictions.c.f.626) # confusion matrix
cluster_report(cm.c.f.626, cap = "Decision Tree without light and minsplit = 626") # Quality measures of Decision tree

pretty_print_string("The accuracy is about the same as base on testset 2 (~79%). But not as good as the 89% testset 1 reached.")

pretty_print_string("The results are about the same as before. It would seem the decision tree is well optimized for this kind of data now.")

pretty_print_string("Light and C02 are the best parameters. Correlation matrix will be applied later.")


# > No C02 ----------------------------------------------------------------
# >> Formatting data - remove C02 ---------------------------------------
remove.cO2 <- function(x) {
  x <- select(x, -CO2)
}

test.f2 <- remove.cO2(test.f) # f as in filtered
test2.f2 <- remove.cO2(test2.f)
training.f2 <- remove.cO2(training.f)

# >> No CO2 - no control ---------------------------------
# >>> Tree
tree.f2 <- rpart(Occupancy ~ .,
                method = "class",
                data = training.f2)
rpart.plot(tree.f2)

pretty_print_string("The tree looks complex now. Let's see the accuracy.")

# >>> predictions and repport
predictions.f2 <- predict(tree.f2, test.f2, type = 'class') # predicting unseen test data
cm.f2 <- table(test$Occupancy, predictions.f2) # confusion matrix
cluster_report(cm.f2, cap = "T1: Decision Tree without C02") # Quality measures of Decision tree

predictions.f2.2 <- predict(tree.f2, test2.f2, type = 'class') # predicting unseen test data
cm.f2.2 <- table(test2$Occupancy, predictions.f2.2) # confusion matrix
cluster_report(cm.f2.2, cap = "T2: Decision Tree without C02") # Quality measures of Decision tree

pretty_print_string("The accuracy is worse with ~64% accuracy on testset 1 and ~79% on testset 2.")


# >> No CO2 control loop -----------------------------------------------
accuracy.f.arr <- c()
for (i in 1:150) {
  accuracy.f.arr <- c(accuracy.f.arr, accuracy.test(i, training.f2, test.f2))
}
accuracy.f.arr
which(max(accuracy.f.arr) == accuracy.f.arr) # best results in list

pretty_print_string("Minsplit of 60-61 gives the best predictions in the set range.")


# >>> No CO2 m=60 -----------------------------------------------------
control.c.f2.60 <- rpart.control(minsplit = 60, minbucket = 60/2, cp = 0.001) # for adjusting hyperparameters

# >>>> Tree
tree.c.f2.60 <- rpart(Occupancy ~ .,
                     method = "class",
                     data = training.f2,
                     control = control.c.f2.60)
rpart.plot(tree.c.f2.60)

pretty_print_string("The tree is more complex.")

# >>>> predictions and repport
predictions.c.f2.60 <- predict(tree.c.f2.60, test.f2, type = 'class') # predicting unseen test data
cm.c.f2.60 <- table(test$Occupancy, predictions.c.f2.60) # confusion matrix
cluster_report(cm.c.f2.60, cap = "T1: Decision Tree without C02 and minsplit = 60") # Quality measures of Decision tree

predictions.c.f2.60.2 <- predict(tree.c.f2.60, test2.f2, type = 'class') # predicting unseen test data
cm.c.f2.60.2 <- table(test2$Occupancy, predictions.c.f2.60.2) # confusion matrix
cluster_report(cm.c.f2.60.2, cap = "T2: Decision Tree without C02 and minsplit = 60") # Quality measures of Decision tree

pretty_print_string("The result did improve with control, to ~64% on testset 1 and ~79% on testset 2. It seems the minslpit results could improve at higher values. So let's try that.")


# >> No CO2 control loop 2 -------------------------------------------
accuracy.f.arr <- c()
for (i in 1:1500) {
  accuracy.f.arr <- c(accuracy.f.arr, accuracy.test(i, training.f2, test.f2))
}

accuracy.f.arr
which(max(accuracy.f.arr) == accuracy.f.arr)

pretty_print_string("Minsplit between 721 and 1323 give better values.")


# >>> No CO2 m=724 --------------------------------------------------
control.c.f2.724 <- rpart.control(minsplit = 724, minbucket = 724/2, cp = 0.001) # for adjusting hyperparameters
# >>>> Tree
tree.c.f2.724 <- rpart(Occupancy ~ .,
                      method = "class",
                      data = training.f2,
                      control = control.c.f2.724)
rpart.plot(tree.c.f2.724)

pretty_print_string("The model is very simple now, using different attributes.")


# >>>> predictions and repport
predictions.c.f2.724 <- predict(tree.c.f2.724, test.f2, type = 'class') # predicting unseen test data
cm.c.f2.724 <- table(test$Occupancy, predictions.c.f2.724) # confusion matrix
cluster_report(cm.c.f2.724, cap = "T1: Decision Tree without CO2 and minsplit = 724") # Quality measures of Decision tree

predictions.c.f2.724.2 <- predict(tree.c.f2.724, test2.f2, type = 'class') # predicting unseen test data
cm.c.f2.724.2 <- table(test2$Occupancy, predictions.c.f2.724.2) # confusion matrix
cluster_report(cm.c.f2.724.2, cap = "T2: Decision Tree without CO2 and minsplit = 724") # Quality measures of Decision tree


pretty_print_string("Accuracy improved to ~80% on testset 1 and ~89% on testset 2.")

pretty_print_string("Without light and c02, the most dominant attributes, a somewhat high accuracy can still be reached with the decision tree.")


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
          "on test-set 2.", sep = "\n"))

### Picking variables with a high correlation
training_2 <- training
training_2$Occupancy <- as.numeric(training_2$Occupancy)
training_2$date <- as.numeric(training_2$date)

cor(training_2)

cat(paste("Light, CO2 and Temparature are all correlated with Occupancy",
          "with a coefficient above 0.50. Therefore we choose these for a potentially better model.", sep = "\n"))

svmfit <- svm(Occupancy ~ Temperature + Light + CO2,
              data = training,
              type = "C-classification",
              kernel = "radial",
              cost = 7,
              gamma = 0.05,
              scale = TRUE)

summary(svmfit)

plot(svmfit, training, CO2 ~ Temperature,
     slice=list(Light=3))

predictions <- predict(svmfit, test, type = 'class') # predicting unseen test data
cm <- table(test$Occupancy, predictions) # confusion matrix
cluster_report(cm, cap = "Support-Vector-Machine") # Quality measures of SVM

predictions <- predict(svmfit, test2, type = 'class') # predicting unseen test data
cm <- table(test2$Occupancy, predictions) # confusion matrix
cluster_report(cm, cap = "Support-Vector-Machine") # Quality measures of SVM


# 96 % accuracy on test and 90% accuracy on test2
cat(paste("The accuracy was improved both on test-set 1 and 2, mostly when looking at test-set 2.",
          "Choosing the variables that are most correlated with the response variable, helps build a better model", sep = "\n"))

## Neural Network ----------------------------------------------------------
library(neuralnet)
set.seed(12345689) # Men how, vi glemte 7, men det gør ikke noget, for vi har det sjovt.

print(dim(training)); print(dim(test))

netmodel <- neuralnet(Occupancy ~ weekdayNum+ Temperature + Humidity+ CO2+HumidityRatio,
                 data = training,
                 hidden = 2,
                 linear.output = FALSE, 
                 err.fct = 'ce', 
                 likelihood = TRUE,
                 threshold=0.1,)

#plotting the netmodel
print(netmodel)
plot(netmodel)
test_sample <- test2[sample(nrow(test2), size = 8143, replace = FALSE), ]

final_output=cbind (test_sample, training, 
                    as.data.frame(netmodel$net.result) )
colnames(final_output) = c("Date","Temperature","Humidity","Light","CO2", "HumidityRatio","Occupancy", "time", "weekday","weekday/Num",
                           "expected date","expected Temperature","expected Humidity","expected Light","expected CO2","expected HumidityRatio","expected Occupancy", "expected time", "expected weekday","expected weekday/Num",
                           "Neural Net Output")

actual_vs_predicted <-select(final_output, "Occupancy","expected Occupancy")
table1 <- table(actual_vs_predicted)
print(table1)
#overall accuracy for Occupancy
print(sum(diag(table1))/sum(table1))

0.6656024
0.6651111

#incorrect classification
print(1-sum(diag(table1))/sum(table1))


actual_vs_predicted <-select(final_output, "weekday","expected weekday")
table1 <- table(actual_vs_predicted)
print(table1)

#overall accuracy for Occupancy
print(sum(diag(table1))/sum(table1))

#incorrect classification
print(1-sum(diag(table1))/sum(table1))



## Naïve Bayes ------------------------------------------------------------

set.seed(120)  # Setting Seed
classifier_cl <- naiveBayes(Occupancy ~ ., data = training)
classifier_cl

# Predicting on test data
y_pred <- predict(classifier_cl, newdata = test2)

# Confusion Matrix
cm <- table(test2$Occupancy, y_pred)
cm

# Model Evauation
confusionMatrix(cm)

# Naïve Bayes witout light and date

test_sample <- test2.f
test_sample <- test_sample[2:6]

set.seed(120)  # Setting Seed
classifier_cl <- naiveBayes(Occupancy ~., data = training)
classifier_cl

# Predicting on test data
y_pred <- predict(classifier_cl, newdata = test2)

# Confusion Matrix
cm <- table(test2$Occupancy, y_pred)
cm

# Model Evauation
confusionMatrix(cm)


plot(y_pred)
plot(cm)
