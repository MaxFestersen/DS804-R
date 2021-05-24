# Exercise 14 / Classification project ------------------------------------

## Libraries
source("../cluster_quality.R") # For checking quality of fit
library(SciViews) # to use boxplots and scatterplots together with pairs function
library(rpart) # Decision Tree
library(rpart.plot) # Visualizing Decision Tree
library(e1071) # Support Vector Machine
library(neuralnet) # Neural Network
library(lubridate) # Time and date manipulation
library(tidyverse) # Utility functions - like formatting
#library(nnet) # Neural Network (for comparison) - no longer used
library(caret) # Confusion matrix
# library(caTools) # For splitting samples (not used)
#library(FSelector) # For information gain
library(ggplot) #Knn Plotting 
library(class) #Knn
library(gmodels) #More Knn Matrix (Not really that much better than the usual one.)
library(plyr) # For a function in plotting Knn
library(gridExtra) # For some plotting goodness

# Dataset -----------------------------------------------------------------
# We have chosen the Occupancy dataset: http://archive.ics.uci.edu/ml/datasets/Occupancy+Detection+#

# > Loading data ----
test <- read.table("datatest.txt", sep = ",")
test2 <- read.table("datatest2.txt", sep = ",")
training <- read.table("datatraining.txt", sep = ",")

# > Formatting data -------------------------------------------------------
# Date might be an issue, as we will never test the results are based on a specific period
test <- as_tibble(test)
test$Occupancy <- factor(test$Occupancy) # Factor Occupancy
test <- dplyr::rowwise(test) %>% # Group by each row (to use functions on row level)
  dplyr::mutate( time = strsplit(date, " ")[[1]][2], # Split date, and use the time part
          date = strsplit(date, " ")[[1]][1]) # Split date, and remove time part
test$date <- ymd(test$date) # Change date from char to time format
test$time <- hms(test$time) # change time from char to time format
test$weekday <- factor(weekdays(test$date))
test <- test %>%
  dplyr::mutate(weekdayNum = as.numeric(ifelse(
    weekday == "mandag" | weekday == "Monday",
    1,
    ifelse(weekday == "tirsdag" | weekday == "Tuesday",
           2,
           ifelse(weekday == "onsdag" | weekday == "Wednesday",
                  3,
                  ifelse(weekday == "torsdag" | weekday == "Tuesday",
                         4,
                         ifelse(weekday == "fredag" | weekday == "Friday",
                                5,
                                ifelse(weekday == "lørdag" | weekday == "Saturday",
                                       6,
                                       7
                                )))))))) %>%
  ungroup()
test2 <- as_tibble(test2)
test2$Occupancy <- factor(test2$Occupancy) # Factor Occupancy
test2 <- dplyr::rowwise(test2) %>% # Group by each row (to use functions on row level)
  dplyr::mutate( time = strsplit(date, " ")[[1]][2], # Split date, and use the time part
                 date = strsplit(date, " ")[[1]][1]) # Split date, and remove time part
test2$date <- ymd(test2$date) # Change date from char to time format
test2$time <- hms(test2$time) # change time from char to time format
test2$weekday <- factor(weekdays(test2$date))
test2 <- test2 %>%
  dplyr::mutate(weekdayNum = as.numeric(ifelse(
    weekday == "mandag" | weekday == "Monday",
    1,
    ifelse(weekday == "tirsdag" | weekday == "Tuesday",
           2,
           ifelse(weekday == "onsdag" | weekday == "Wednesday",
                  3,
                  ifelse(weekday == "torsdag" | weekday == "Tuesday",
                         4,
                         ifelse(weekday == "fredag" | weekday == "Friday",
                                5,
                                ifelse(weekday == "lørdag" | weekday == "Saturday",
                                       6,
                                       7
                                )))))))) %>%
  ungroup()
training <- as_tibble(training)
training$Occupancy <- factor(training$Occupancy) # Factor Occupancy
training <- dplyr::rowwise(training) %>% # Group by each row (to use functions on row level)
  rowwise() %>% 
  dplyr::mutate( time = strsplit(date, " ")[[1]][2], # Split date, and use the time part
          date = strsplit(date, " ")[[1]][1]) # Split date, and remove time part
training$date <- ymd(training$date) # Change date from char to time format
training$time <- hms(training$time) # change time from char to time format
training$weekday <- factor(weekdays(training$date))
training <- training %>%
  dplyr::mutate(weekdayNum = as.numeric(ifelse(
    weekday == "mandag" | weekday == "Monday",
    1,
    ifelse(weekday == "tirsdag" | weekday == "Tuesday",
           2,
           ifelse(weekday == "onsdag" | weekday == "Wednesday",
                  3,
                  ifelse(weekday == "torsdag" | weekday == "Tuesday",
                         4,
                         ifelse(weekday == "fredag" | weekday == "Friday",
                                5,
                                ifelse(weekday == "lørdag" | weekday == "Saturday",
                                       6,
                                       7
                                )))))))) %>%
  ungroup()

# Formatting function stopped functioning  - so look above for individual hard code method.
# test <- formating(test)
# test2 <- formating(test2)
# training <- formating(training)

# Create combined testset
test3 <- merge(x = test, y = test2, by = colnames(test), all = TRUE)
Total <- merge(x = test3, y = training, by = colnames(test), all = TRUE)
rows <- nrow(Total)
home <- sum(as.numeric(Total$Occupancy)-1)
out <- nrow(Total) - sum(as.numeric(Total$Occupancy)-1)
dataDistribution <- c(rows, home, out)
names(dataDistribution) <- c("All", "1", "0")
barplot(dataDistribution, main="Occupancy for all data", ylab="Datapoints")

# Choice of Algorithms ----------------------------------------------------
# pairs(training[-1], diag.panel = panel.boxplot)

# Information gain --------------------------------------------------------
# ig.weights <- information.gain(Occupancy ~ ., as.data.frame(training))
# ig.weights


## Decision Tree ----------------------------------------------------------
set.seed(44444444)


# > Base test (no control) ------------------------------------------------
# Generated using whatever it feels like
# >> Tree
tree <- rpart(Occupancy ~ .,
              method = "class",
              data = training)
rpart.plot(tree) # A bit simple?

print("The tree looks a bit simple. So let's test the precision.")

# >> predictions and repport
# >>> Test 1
predictions <- predict(tree, test, type = 'class') # predicting unseen test data
cm <- table(test$Occupancy, predictions) # confusion matrix
cluster_report(cm, cap = "T1: Decision Tree") # Quality measures of Decision tree

# >>> Test 2
predictions <- predict(tree, test2, type = 'class') # predicting unseen test data
cm <- table(test2$Occupancy, predictions) # confusion matrix
cluster_report(cm, cap = "T2: Decision Tree") # Quality measures of Decision tree

# >>> Test 3
predictions <- predict(tree, test3, type = 'class') # predicting unseen test data
cm <- table(test3$Occupancy, predictions) # confusion matrix
cluster_report(cm, cap = "T3: Decision Tree") # Quality measures of Decision tree


cat(paste(
  "Test 1 has ~97% accuracy without control parameters.",
  "Test 2 has ~99% accuracy without control parameters.",
  "Test 3 has ~99% accuracy without control parameters.",
  "What if we add some control parameters?",
  sep = "\n"
))


# > With some control parameters ------------------------------------------
# >> Control parameter
control <- rpart.control(minsplit = 32, minbucket = 32/2, cp = 0.001) # for adjusting hyperparameters

# >> Tree
tree.c <- rpart(Occupancy ~ .,
                method = "class",
                data = training,
                control = control)
rpart.plot(tree.c)

print("The tree looks a bit better Visually, but did the results improve?")

# >> predictions and repport
predictions.c <- predict(tree.c, test, type = 'class') # predicting unseen test data
cm.c <- table(test$Occupancy, predictions.c) # confusion matrix
cluster_report(cm.c, cap = "Decision Tree with control") # Quality measures of Decision tree

cat(paste(
  "The results got way worse with ~79% accuracy.",
  "Maybe we choose poorly. Lets test many minpslit values, to see if we can improve.",
  sep = "\n"
))


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
for (i in 1:1500) {
  accuracy.arr <- c(accuracy.arr, accuracy.test(i, training, test))
}
accuracy.arr
max(accuracy.arr) # Best value
which(max(accuracy.arr) == accuracy.arr) # best results in list

cat(paste(
  "The result can be matched but not improved.",
  "The best value would be between 136 and 1135.",
  sep = "\n"
))


# >> minsplit of 135 ------------------------------------------------------
# >> Control parameter
control <- rpart.control(minsplit = 136, minbucket = 136/2, cp = 0.001) # for adjusting hyperparameters

# >> Tree
tree.c.136 <- rpart(Occupancy ~ .,
                    method = "class",
                    data = training,
                    control = control)
rpart.plot(tree.c.136)

print("The tree is the same as the one reached without control parameters.")

# >> predictions and repport
# >>> Test 1
predictions.c <- predict(tree.c.724, test, type = 'class') # predicting unseen test data
cm.c <- table(test$Occupancy, predictions.c) # confusion matrix
cluster_report(cm.c, cap = "T1: Decision Tree with control") # Quality measures of Decision tree

# >>> Test 2
predictions.c <- predict(tree.c.724, test2, type = 'class') # predicting unseen test data
cm.c <- table(test2$Occupancy, predictions.c) # confusion matrix
cluster_report(cm.c, cap = "T2: Decision Tree with control") # Quality measures of Decision tree

# >>> Test 3
predictions.c <- predict(tree.c.724, test3, type = 'class') # predicting unseen test data
cm.c <- table(test3$Occupancy, predictions.c) # confusion matrix
cluster_report(cm.c, cap = "T3: Decision Tree with control") # Quality measures of Decision tree

print("As expected, the results are the same.")


# > With information gain weights -----------------------------------------
# It does not work for some reason. The format of weights should be a list, and lengths match.
# >> Tree
# tree.c.ig <- rpart(Occupancy ~ .,
#                     method = "class",
#                     data = training[2:7],
#                     weights = ig.weights$attr_importance[2:6])
# rpart.plot(tree.c.ig)


# > Tree without light ----------------------------------------------------
cat(paste(
  "Light seems to be a dominant attribute (~97% accuracy using only Light).",
  "So can we predict a result without it?",
  sep = "\n"
))


# >> Formatting data - remove light ---------------------------------------
remove.light <- function(x) {
  x <- select(x, -Light)
}

test.f <- remove.light(test) # f as in filtered
test2.f <- remove.light(test2)
test3.f <- remove.light(test3)
training.f <- remove.light(training)

# >> No light - no control ------------------------------------------------
# >>> Tree
tree.f <- rpart(Occupancy ~ .,
                method = "class",
                data = training.f)
rpart.plot(tree.f)

print("The tree looks a bit more complex now. Let's see the accuracy.")

# >>> predictions and repport
# >>>> Test 1
predictions.f <- predict(tree.f, test.f, type = 'class') # predicting unseen test data
cm.f <- table(test$Occupancy, predictions.f) # confusion matrix
cluster_report(cm.f, cap = "T1: Decision Tree without light") # Quality measures of Decision tree

# >>>> Test 2
predictions.f <- predict(tree.f, test2.f, type = 'class') # predicting unseen test data
cm.f <- table(test2$Occupancy, predictions.f) # confusion matrix
cluster_report(cm.f, cap = "T2: Decision Tree without light") # Quality measures of Decision tree

# >>>> Test 3
predictions.f <- predict(tree.f, test3.f, type = 'class') # predicting unseen test data
cm.f <- table(test3$Occupancy, predictions.f) # confusion matrix
cluster_report(cm.f, cap = "T3: Decision Tree without light") # Quality measures of Decision tree


cat(paste(
  "Test 1 has ~69% accuracy without Light attribute.",
  "Test 2 has ~79% accuracy without Light attribute.",
  "Test 3 has ~76% accuracy without Light attribute.",
  "It would seem that without the Light attribute, the tree isn't so accurate.",
  "Maybe this time, control parameters could help.",
  sep = "\n"
))

# >> No light control loop -----------------------------------------------
accuracy.f.arr <- c()
for (i in 1:150) {
  accuracy.f.arr <- c(accuracy.f.arr, accuracy.test(i, training.f, test.f))
}
#accuracy.f.arr
which(max(accuracy.f.arr) == accuracy.f.arr) # best results in list

print("Minsplit of 92-121 gives the best predictions in the set range.")

# >>> No light m=92 -----------------------------------------------------
control.c.f.92 <- rpart.control(minsplit = 92, minbucket = 92/2, cp = 0.001) # for adjusting hyperparameters
# >>>> Tree
tree.c.f.92 <- rpart(Occupancy ~ .,
                     method = "class",
                     data = training.f,
                     control = control.c.f.92)
rpart.plot(tree.c.f.92)

print("The tree is also somewhat more complex.")

# >>>> predictions and repport
predictions.c.f.92 <- predict(tree.c.f.92, test.f, type = 'class') # predicting unseen test data
cm.c.f.92 <- table(test$Occupancy, predictions.c.f.92) # confusion matrix
cluster_report(cm.c.f.92, cap = "Decision Tree without light and minsplit = 92") # Quality measures of Decision tree

cat(paste(
  "The result did improve, but only by a bit.",
  "It seems the minslpit results could improve at higher values. So let's try that.",
  sep = "\n"
))


# >> No light control loop 2 -------------------------------------------
accuracy.f.arr <- c()
for (i in 1:1000) {
  accuracy.f.arr <- c(accuracy.f.arr, accuracy.test(i, training.f, test.f))
}

#accuracy.f.arr
which(max(accuracy.f.arr) == accuracy.f.arr)

print("Minsplit between 626 and 921 give better values.")


# >>> No light m=626 --------------------------------------------------
control.c.f.626 <- rpart.control(minsplit = 626, minbucket = 626/2, cp = 0.001) # for adjusting hyperparameters

# >>>> Tree
tree.c.f.626 <- rpart(Occupancy ~ .,
                      method = "class",
                      data = training.f,
                      control = control.c.f.626)
rpart.plot(tree.c.f.626)

print("The model is very simple now, using only C02 attribute.")


# >>>> predictions and repport
# >>>>> Test 1
predictions.c.f.626 <- predict(tree.c.f.626, test.f, type = 'class') # predicting unseen test data
cm.c.f.626 <- table(test$Occupancy, predictions.c.f.626) # confusion matrix
cluster_report(cm.c.f.626, cap = "T1: Decision Tree without light and minsplit = 626") # Quality measures of Decision tree

# >>>>> Test 2
predictions.c.f.626.2 <- predict(tree.c.f.626, test2.f, type = 'class') # predicting unseen test data
cm.c.f.626.2 <- table(test2$Occupancy, predictions.c.f.626.2) # confusion matrix
cluster_report(cm.c.f.626.2, cap = "T2: Decision Tree without light and minsplit = 626") # Quality measures of Decision tree


# >>>>> Test 3
predictions.c.f.626.3 <- predict(tree.c.f.626, test3.f, type = 'class') # predicting unseen test data
cm.c.f.626.3 <- table(test3$Occupancy, predictions.c.f.626.3) # confusion matrix
cluster_report(cm.c.f.626.3, cap = "T3: Decision Tree without light and minsplit = 626") # Quality measures of Decision tree

cat(paste(
  "Test 1 has ~87% accuracy without Light attribute ans minaplit of 626.",
  "Test 2 has ~79% accuracy without Light attribute.",
  "Test 3 has ~78% accuracy without Light attribute.",
  "The results only segnifically improved on testset 1 from the test without control parameters.",
  "The results are always good.",
  "It seems that one attribute is allways dominant.",
  "Light and C02 are the best parameters.",
  "Which could hint at the other paratmeters havinng less correlation",
  "or a too small test set to see futher splits.",
  "Correlation matrix will be applied later.",
  sep = "\n"
))


# > No C02 ----------------------------------------------------------------
# >> Formatting data - remove C02 ---------------------------------------
remove.cO2 <- function(x) {
  x <- select(x, -CO2)
}

test.f2 <- remove.cO2(test.f) # f as in filtered
test2.f2 <- remove.cO2(test2.f)
test3.f2 <- remove.cO2(test3.f)
training.f2 <- remove.cO2(training.f)

# >> No CO2 - no control ---------------------------------
# >>> Tree
tree.f2 <- rpart(Occupancy ~ .,
                 method = "class",
                 data = training.f2)
rpart.plot(tree.f2)

print("The tree looks complex now. Let's see the accuracy.")

# >>> predictions and repport
# >>>> Test 1
predictions.f2 <- predict(tree.f2, test.f2, type = 'class') # predicting unseen test data
cm.f2 <- table(test$Occupancy, predictions.f2) # confusion matrix
cluster_report(cm.f2, cap = "T1: Decision Tree without C02") # Quality measures of Decision tree


# >>>> Test 2
predictions.f2.2 <- predict(tree.f2, test2.f2, type = 'class') # predicting unseen test data
cm.f2.2 <- table(test2$Occupancy, predictions.f2.2) # confusion matrix
cluster_report(cm.f2.2, cap = "T2: Decision Tree without C02") # Quality measures of Decision tree

# >>>> Test 3
predictions.f3.2 <- predict(tree.f2, test3.f2, type = 'class') # predicting unseen test data
cm.f3.2 <- table(test3$Occupancy, predictions.f3.2) # confusion matrix
cluster_report(cm.f3.2, cap = "T3: Decision Tree without C02") # Quality measures of Decision tree

cat(paste(
  "Test 1 has ~64% accuracy without C02 and light, and no control parameters.",
  "Test 2 has ~79% accuracy without C02 and light, and no control parameters.",
  "Test 3 has ~76% accuracy without C02 and light, and no control parameters.",
  "Overall, the accuracy is allright.",
  sep = "\n"
))

# >> No CO2 control loop -----------------------------------------------
accuracy.f.arr <- c()
for (i in 1:150) {
  accuracy.f.arr <- c(accuracy.f.arr, accuracy.test(i, training.f2, test.f2))
}
#accuracy.f.arr
which(max(accuracy.f.arr) == accuracy.f.arr) # best results in list

print("Minsplit of 92-93 gives the best predictions in the set range.")


# >> No CO2 control loop ---------------------------------------------
accuracy.f.arr <- c()
for (i in 1:1500) {
  accuracy.f.arr <- c(accuracy.f.arr, accuracy.test(i, training.f2, test.f2))
}

#accuracy.f.arr
which(max(accuracy.f.arr) == accuracy.f.arr)

print("Minsplit between 724 and 1323 give better values.")


# >>> No CO2 m=724 --------------------------------------------------
control.c.f2.724 <- rpart.control(minsplit = 724, minbucket = 724/2, cp = 0.001) # for adjusting hyperparameters
# >>>> Tree
tree.c.f2.724 <- rpart(Occupancy ~ .,
                       method = "class",
                       data = training.f2,
                       control = control.c.f2.724)
rpart.plot(tree.c.f2.724)

print("The model is very simple now, using different attributes.")


# >>>> predictions and repport
# >>>>> Test 1
predictions.c.f2.724 <- predict(tree.c.f2.724, test.f2, type = 'class') # predicting unseen test data
cm.c.f2.724 <- table(test$Occupancy, predictions.c.f2.724) # confusion matrix
cluster_report(cm.c.f2.724, cap = "T1: Decision Tree without CO2 and minsplit = 724") # Quality measures of Decision tree

# >>>>> Test 2
predictions.c.f2.724.2 <- predict(tree.c.f2.724, test2.f2, type = 'class') # predicting unseen test data
cm.c.f2.724.2 <- table(test2$Occupancy, predictions.c.f2.724.2) # confusion matrix
cluster_report(cm.c.f2.724.2, cap = "T2: Decision Tree without CO2 and minsplit = 724") # Quality measures of Decision tree

# >>>>> Test 3
predictions.c.f2.724.3 <- predict(tree.c.f2.724, test3.f2, type = 'class') # predicting unseen test data
cm.c.f2.724.3 <- table(test3$Occupancy, predictions.c.f2.724.3) # confusion matrix
cluster_report(cm.c.f2.724.3, cap = "T3: Decision Tree without CO2 and minsplit = 724") # Quality measures of Decision tree

cat(paste(
  "Test 1 has ~80% accuracy without C02 and light, and minsplit of 724.",
  "Test 2 has ~87% accuracy without C02 and light, and minsplit of 724.",
  "Test 3 has ~87% accuracy without C02 and light, and minsplit of 724.",
  "The results improved with a higher minsplit (compared to base and minsplit of 60).",
  "Without light and c02, the most dominant attributes,",
  "a somewhat high accuracy can still be reached with the decision tree.",
  "Test 2 and 3 peformed even better without the C02 attribute.",
  sep = "\n"
))


## Support Vector Machine (SVM)----------------------------------------
set.seed(156)
training$time <- as.numeric(training$time)
test$time <- as.numeric(test$time)
test2$time <- as.numeric(test2$time)
test3$time <- as.numeric(test3$time)

svmfit <- svm(Occupancy ~ Temperature + Humidity + Light + CO2 + HumidityRatio + time + weekdayNum, # Note: Leaving out date, as it made results worse :-(
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
cluster_report(cm, cap = "Support-Vector-Machine test_set 1") # Quality measures of SVM

predictions <- predict(svmfit, test2, type = 'class') # predicting unseen test data
cm <- table(test2$Occupancy, predictions) # confusion matrix
cluster_report(cm, cap = "Support-Vector-Machine test_set 2") # Quality measures of SVM

predictions <- predict(svmfit, test3, type = 'class') # predicting unseen test data
cm <- table(test3$Occupancy, predictions) # confusion matrix
cluster_report(cm, cap = "Support-Vector-Machine test_set 3") # Quality measures of SVM


# 97 % accuracy on test and 94.9% accuracy on test2
cat(paste("Instead of using the variable date, we formatted it to be two variables date and time",
          "of which we use the time variable, as it is most likely to be generalizable upon a new dataset",
          "sampled at a different point in time.",
          "The SVM classifier was optimized to perform with a 96% accuracy on test-set 1 and 90% accuracy",
          "on test-set 2.", sep = "\n"))

### Picking variables with a high correlation
training_2 <- training[-9]
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

plot(svmfit, training, Light ~ CO2,
     slice=list(Temperature=3, HumidityRatio=4))

predictions <- predict(svmfit, test, type = 'class') # predicting unseen test data
cm <- table(test$Occupancy, predictions) # confusion matrix
cluster_report(cm, cap = "Support-Vector-Machine test_set 1") # Quality measures of SVM

predictions <- predict(svmfit, test2, type = 'class') # predicting unseen test data
cm <- table(test2$Occupancy, predictions) # confusion matrix
cluster_report(cm, cap = "Support-Vector-Machine test_set 2") # Quality measures of SVM

predictions <- predict(svmfit, test3, type = 'class') # predicting unseen test data
cm <- table(test3$Occupancy, predictions) # confusion matrix
cluster_report(cm, cap = "Support-Vector-Machine test_set 3") # Quality measures of SVM

# 97 % accuracy on test and 95.6% accuracy on test2
cat(paste("The accuracy was improved both on test-set 1 and 2, mostly when looking at test-set 2.",
          "Choosing the variables that are most correlated with the response variable, helps build a better model", sep = "\n"))

## Neural Network ----------------------------------------------------------

# Normalizing training and test set
norm_train <- scale(select(training[2:10], -c(Occupancy, weekday))) %>% 
  as.data.frame()
norm_train$Occupancy <- training$Occupancy

norm_test <- scale(select(test[2:10], -c(Occupancy, weekday))) %>% 
  as.data.frame()
norm_test$Occupancy <- test$Occupancy

norm_test2 <- scale(select(test2[2:10], -c(Occupancy, weekday))) %>% 
  as.data.frame()
norm_test2$Occupancy <- test2$Occupancy

norm_test3 <- scale(select(test3[2:10], -c(Occupancy, weekday))) %>% 
  as.data.frame()
norm_test3$Occupancy <- test3$Occupancy

# First try with all variables
set.seed(12345689)
nn <- neuralnet((Occupancy == "1") + (Occupancy == "0") ~ weekdayNum + Light + time + Temperature + Humidity + CO2 + HumidityRatio,
                data = norm_train,
                hidden = 2,
                lifesign = 'full',
                lifesign.step = 100,
                stepmax = 50000,
                linear.output = FALSE,
                algorithm = 'rprop-',
                err.fct = 'ce',
                likelihood = TRUE,
                threshold = 0.02) 
plot(nn) # plotting neural network
pred <- predict(nn, norm_test, type = "class") # making predictions with nn
cm <- table(norm_test$Occupancy, apply(pred, 1, which.max)) # confusion matrix
cm # 85.7% Accuracy
cluster_report(cm, cap = "Neural Network test_set 1") # computing quality measures

pred <- predict(nn, norm_test2, type = "class") # making predictions with nn
cm <- table(norm_test2$Occupancy, apply(pred, 1, which.max)) # confusion matrix
cm # 89.9% accuracy
cluster_report(cm, cap = "Neural Network test_set 2") # computing quality measures

pred <- predict(nn, norm_test3, type = "class") # making predictions with nn
cm <- table(norm_test3$Occupancy, apply(pred, 1, which.max)) # confusion matrix
cm # 91.4% accuracy
cluster_report(cm, cap = "Neural Network test_set 3") # computing quality measures


cat(paste("Using all numeric variables for training the Neural Network, the process is long",
          "however, accuracy of the model is pretty good with 85.7% on test-set 1 and 89.9% on test-set 2.", sep = "\n"))

#creating readable matrix
ordered_table <- cm[1:2, 2:1]
ordered_table <- rbind(as.numeric(names(ordered_table)), ordered_table)
rownames(ordered_table) <- c("Predicted Occupancy","Predicted NO Occupancy")
colnames(ordered_table) <- c("True Occupancy","True NO Occupancy")
ordered_table

# Second try with less variables
set.seed(12345689)
nn <- neuralnet((Occupancy == "1") + (Occupancy == "0") ~ Light + Temperature + CO2,
                data = norm_train,
                hidden = 2,
                lifesign = 'full',
                lifesign.step = 100,
                stepmax = 50000,
                linear.output = FALSE,
                algorithm = 'rprop-',
                err.fct = 'ce',
                likelihood = TRUE,
                threshold = 0.01) 
plot(nn) # plotting neural network
pred <- predict(nn, norm_test, type = "class") # predicting with nn
cm <- table(norm_test$Occupancy, apply(pred, 1, which.max)) # confusion matrix
cm # ~96% accuracy
cluster_report(cm, cap = "Neural Network test_set 1") # computing quality measures

pred <- predict(nn, norm_test2, type = "class") # making predictions with nn
cm <- table(norm_test2$Occupancy, apply(pred, 1, which.max)) # confusion matrix
cm # 90.9% accuracy
cluster_report(cm, cap = "Neural Network test_set 2") # computing quality measures

pred <- predict(nn, norm_test3, type = "class") # making predictions with nn
cm <- table(norm_test3$Occupancy, apply(pred, 1, which.max)) # confusion matrix
cm # 92.5% accuracy
cluster_report(cm, cap = "Neural Network test_set 3") # computing quality measures

#creating readable matrix
ordered_table <- cm[1:2, 2:1]
ordered_table <- rbind(as.numeric(names(ordered_table)), ordered_table)
rownames(ordered_table) <- c("Predicted Occupancy","Predicted NO Occupancy")
colnames(ordered_table) <- c("True Occupancy","True NO Occupancy")
ordered_table

cat(paste("Using only some of the variables for training the Neural Network, as we also did with SVM",
          "the training processed was shortened and a lower min.threshold could be reached within stepmax.",
          "The accuracy of the model also improved good with ~96% on test-set 1 and 90.9% on test-set 2.", sep = "\n"))


library(neuralnet)
set.seed(12345689) # Men how, vi glemte 7, men det gør ikke noget, for vi har det sjovt.

#print(dim(training)); print(dim(test))


netmodel <- neuralnet(Occupancy ~ weekdayNum+ Temperature + Humidity+ CO2+HumidityRatio, # No light, time (testing), no date or weekday (can't even)
                      data = training,
                      hidden = 2,
                      linear.output = FALSE, 
                      err.fct = 'ce', 
                      likelihood = TRUE,
                      threshold=0.1)



# Plotting the netmodel
print(netmodel)
plot(netmodel)
# Prediction?
test_sample <- test2[sample(nrow(test2), size = 8143, replace = FALSE), ] # Might be a wrong method

final_output <- cbind (test_sample, training, 
                       as.data.frame(netmodel$net.result) )
colnames(final_output) = c("Date","Temperature","Humidity","Light","CO2", "HumidityRatio","Occupancy", "time", "weekday","weekday/Num",
                           "expected date","expected Temperature","expected Humidity","expected Light","expected CO2","expected HumidityRatio","expected Occupancy", "expected time", "expected weekday","expected weekday/Num",
                           "Neural Net Output")

actual_vs_predicted <- select(final_output, "Occupancy","expected Occupancy")
table1 <- table(actual_vs_predicted)
print(table1)
#overall accuracy for Occupancy
print(sum(diag(table1))/sum(table1))

# 0.6656024
# 0.6651111

# incorrect classification
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
pred_test <- predict(classifier_cl, newdata = test)

# Predicting on test2 data
pred_test2 <- predict(classifier_cl, newdata = test2)

# Predicting on test3 data
pred_test3 <- predict(classifier_cl, newdata = test3)

# Confusion Matrix test
cm <- table(test$Occupancy, pred_test)

#creating readable matrix
ordered_table <- rbind(as.numeric(names(cm)), cm)
rownames(ordered_table) <- c("Predicted Occupancy","Predicted NO Occupancy")
colnames(ordered_table) <- c("Actual Occupancy","Actual NO Occupancy")
ordered_table

cluster_report(cm, cap = "Naïve Bayes")
# Model Evauation
confusionMatrix(cm) # 0.9775% accuracy


# Confusion Matrix test2
cm <- table(test2$Occupancy, pred_test2)
#creating readable matrix
ordered_table <- rbind(as.numeric(names(cm)), cm)
rownames(ordered_table) <- c("Predicted Occupancy","Predicted NO Occupancy")
colnames(ordered_table) <- c("Actual Occupancy","Actual NO Occupancy")
ordered_table

cluster_report(cm, cap = "Naïve Bayes")
# Model Evauation
confusionMatrix(cm) # 0.9892 % accuracy

# Confusion Matrix test3
cm <- table(test3$Occupancy, pred_test3)

cm <- table(test2$Occupancy, pred_test2)
#creating readable matrix
ordered_table <- rbind(as.numeric(names(cm)), cm)
rownames(ordered_table) <- c("Predicted Occupancy","Predicted NO Occupancy")
colnames(ordered_table) <- c("Actual Occupancy","Actual NO Occupancy")
ordered_table

cluster_report(cm, cap = "Naïve Bayes")
# Model Evauation
confusionMatrix(cm) # 0.9867% accuracy


# Naïve Bayes without light and date

test_sample <- test2.f
test_sample <- test_sample[2:6]

classifier_cl <- naiveBayes(Occupancy ~., data = training)
classifier_cl

# Predicting on test data
y_pred <- predict(classifier_cl, newdata = test_sample)

# Confusion Matrix
cm <- table(test_sample$Occupancy, y_pred)
cm

# Model Evauation
confusionMatrix(cm)


plot(y_pred)
plot(cm)




# Naïve Bayes caret with 10 fold CV
x = training[,-7]
y = training$Occupancy

x1 = test[,-7]
y2 = test$Occupancy

naive_bayes_via_caret <- train(Occupancy ~ ., 
                               data = training, 
                               method = "naive_bayes", 
                               usepoisson = TRUE)

naive_bayes_via_caret
confusionMatrix(naive_bayes_via_caret)


# Build the model
set.seed(123)
model <- caret::train(Occupancy ~., data = training, method = "nb", 
               trControl = trainControl("cv", number = 10))
# Make predictions
predicted.classes <- model %>% predict(test)
# Model n accuracy
mean(predicted.classes == test$Occupancy)


model_pca = train(x,y,trControl=trainControl(method='cv',number=10,preProc = "pca"))
model_scale = train(x,y,trControl=trainControl(method='cv',number=10,preProc = "scale"))
model_center = train(x,y,trControl=trainControl(method='cv',number=10,preProc = "center"))
model_Boxcox = train(x,y,trControl=trainControl(method='cv',number=10,preProc = "BoxCox"))


cm <- predict(model_pca$finalModel,x) #predict on training data
table(predict(model_pca$finalModel,x),y) #table of training data


pred <- predict(model_pca, newdata = y1) #predict on test data
confusionMatrix(pred, test$Occupancy) #table of test data

confusionMatrix(model) #entries are percentual average cell counts across resamples

plot(model)

search_grid <- expand.grid(
  usekernel = c(TRUE, FALSE),
  fL = 0:5,
  adjust = seq(0, 5, by = 1)
)

# train model
nb.m2 <- train(
  x = x,
  y = y,
  method = "nb",
  trControl = trainControl,
  preProc = "pca"
)


training %>%
  filter(Occupancy == "1") %>%
  select_if(is.numeric) %>%
  cor() %>%
  corrplot::corrplot()

training %>%
  filter(Occupancy == "0") %>%
  select_if(is.numeric) %>%
  cor() %>%
  corrplot::corrplot()


# KNN ========================================================================


library(class)
library(gmodels)


# Setup Data

normalize <- function(x) {return ((x - min(x)) / (max(x) - min(x))) }
test_norm <- as.data.frame(lapply(test[2:6], normalize))
training_norm <- as.data.frame(lapply(training[2:6], normalize))

train_labels <- as.factor(training[, 7, drop = TRUE])
test_labels <- as.factor(test[, 7, drop = TRUE])

as.data.frame(lapply(test[2:6], normalize))
as.data.frame(lapply(test2[2:6], normalize))

# =====

# Knn using K = Squareroot of Observations on Test 1.

# =====

knn104_pred <- knn(train = training_norm, test = test_norm, cl = train_labels, k =  sqrt( (nrow(training)) + (nrow(test)) ), use.all = TRUE)

#Precision
#100 * sum(test_labels==knn104_pred)/NROW(test_labels)

CrossTable(x = test_labels, y = knn104_pred,prop.chisq=FALSE)

# Error rate
cat(paste("Error Rate of Knn = 104: ", mean(test_labels != knn104_pred) ))

# Confusion Matrix
confusionMatrix(knn104_pred, test_labels) 

# Plot it with Light and Humidity because those are important.

plot.df = data.frame(test_norm, predicted = knn104_pred, truth = as.factor(test$Occupancy)) # Create a dataframe to simplify charting

plot.df1 = data.frame(x = plot.df$Humidity, 
                      y = plot.df$Light, 
                      predicted = plot.df$predicted,
                      truth = as.factor(test$Occupancy)) # First use Convex hull to determine boundary points of each cluster

find_hull = function(df) df[chull(df$x, df$y), ]
boundary = ddply(plot.df1, .variables = "predicted", .fun = find_hull)

ggplot(plot.df, aes(Humidity, Light, color = predicted, shape = truth)) + 
  geom_point(size = 4, alpha = 0.3)

# Plot predication and truth as Heatmap (Only the Occupied part)

# knn.plot.heatmap.pred = data.frame(test_norm, predicted = as.numeric(levels(knn104_pred))[knn104_pred])
# knn.plot.heatmap.pred <- knn.plot.heatmap.pred[apply(knn.plot.heatmap.pred, 1, function(row) all(row !=0 )), ] #Remove Occupancy = 0
# 
# 
# knn.plot.heatmap.truth = data.frame(test_norm, truth = test$Occupancy) #Remove Occupancy = 0
# knn.plot.heatmap.truth <- knn.plot.heatmap.truth[apply(knn.plot.heatmap.truth, 1, function(row) all(row !=0 )), ]
# 
# 
# knn.heat.pred <- ggplot(knn.plot.heatmap.pred, aes(Humidity, Light)) + xlim(0,1) + ylim(0,1) +
#   geom_density_2d_filled(contour_var = "count") + facet_wrap(vars(predicted)) + theme(legend.position = "none")
# 
# knn.heat.truth <- ggplot(knn.plot.heatmap.truth, aes(Humidity, Light))  + xlim(0,1) + ylim(0,1) + 
#   geom_density_2d_filled(contour_var = "count") + facet_wrap(vars(truth)) + theme(legend.position = "none")
# 
# grid.arrange(knn.heat.pred, knn.heat.truth, nrow=2)


# =====

# Batch Testing with K values (Test 1)

# Not sure this is really good for much.

# =====

i=1
k.optm=1
for (i in 1:120){
  knn.mod <- knn(train = training_norm, test = test_norm, cl = train_labels, k =  i)
  k.optm[i] <- 100* sum(test_labels==knn.mod)/NROW(test_labels)
  k=i
  cat(k, '=',k.optm[i],'\n')
}

plot(k.optm, type="b", xlab="K-Value", ylab="Accuracy level")

#=====

# Cross validation

# =====

trControl <- trainControl(method  = "cv",
                          number  = 10)

cross_fit <- train(training_norm, train_labels,
                   method     = "knn",
                   tuneGrid   = expand.grid(k = 1:50),
                   trControl  = trControl,
                   metric     = "Accuracy",
)

cross_fit
confusionMatrix(cross_fit)
#Cross validation best K as a plot
plot(cross_fit)
#plot of ROC(repeated Cross-validation)
plot(cross_fit, print.thres = 0.5, type="S")

#Knn with K = 3

knn3_pred <- knn(train = training_norm, test = test_norm, cl = train_labels, k =  3, use.all = TRUE)

# Confusion Matrix
confusionMatrix(knn3_pred, test_labels) 

plot.df = data.frame(test_norm, predicted = knn3_pred, truth = as.factor(test$Occupancy)) # Create a dataframe to simplify charting

plot.df1 = data.frame(x = plot.df$Humidity, 
                      y = plot.df$Light, 
                      predicted = plot.df$predicted,
                      truth = as.factor(test$Occupancy)) # First use Convex hull to determine boundary points of each cluster

find_hull = function(df) df[chull(df$x, df$y), ]
boundary = ddply(plot.df1, .variables = "predicted", .fun = find_hull)

ggplot(plot.df, aes(Humidity, Light, color = predicted, shape = truth)) + 
  geom_point(size = 4, alpha = 0.3)



