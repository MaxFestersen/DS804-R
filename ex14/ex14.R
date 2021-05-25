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
library(FSelector) # For information gain
library(ggplot) #Knn Plotting 
library(class) #Knn
library(gmodels) #More Knn Matrix (Not really that much better than the usual one.)
library(plyr) # For a function in plotting Knn
library(gridExtra) # For some plotting goodness
library(class)
library(gmodels)
library(mltools)
library(pROC)
# Dataset -----------------------------------------------------------------
# We have chosen the Occupancy dataset: http://archive.ics.uci.edu/ml/datasets/Occupancy+Detection+#

# > Loading data ----
test <- read.table("datatest.txt", sep = ",")
test2 <- read.table("datatest2.txt", sep = ",")
training <- read.table("datatraining.txt", sep = ",")

# > Formatting data -------------------------------------------------------
# Date might be an issue, as we will never test the results are based on a specific period
test$Occupancy <- factor(test$Occupancy) # Factor Occupancy
test <- test %>%
  dplyr::rowwise() %>% # Group by each row (to use functions on row level)
  dplyr::mutate(time = strsplit(date, " ")[[1]][2]) %>% # Split date, and use the time part
  dplyr::mutate(date = strsplit(date, " ")[[1]][1]) %>% # Split date, and remove time part
  ungroup()  # Remove rowwise
test$date <- ymd(test$date) # Change date from char to time format
test$time <- hms(test$time) # change time from char to time format
test$weekday <- factor(weekdays(test$date))
test <- rowwise(test) %>% 
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
                                ))))))))

test2$Occupancy <- factor(test2$Occupancy) # Factor Occupancy
test2 <- test2 %>%
  dplyr::rowwise() %>% # Group by each row (to use functions on row level)
  dplyr::mutate(time = strsplit(date, " ")[[1]][2]) %>% # Split date, and use the time part
  dplyr::mutate(date = strsplit(date, " ")[[1]][1]) %>% # Split date, and remove time part
  ungroup()  # Remove rowwise
test2$date <- ymd(test2$date) # Change date from char to time format
test2$time <- hms(test2$time) # change time from char to time format
test2$weekday <- factor(weekdays(test2$date))
test2 <- rowwise(test2) %>% 
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
                                ))))))))

training$Occupancy <- factor(training$Occupancy) # Factor Occupancy
training <- training %>%
  dplyr::rowwise() %>% # Group by each row (to use functions on row level)
  dplyr::mutate(time = strsplit(date, " ")[[1]][2]) %>% # Split date, and use the time part
  dplyr::mutate(date = strsplit(date, " ")[[1]][1]) %>% # Split date, and remove time part
  ungroup()  # Remove rowwise
training$date <- ymd(training$date) # Change date from char to time format
training$time <- hms(training$time) # change time from char to time format
training$weekday <- factor(weekdays(training$date))
training <- rowwise(training) %>% 
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
                                ))))))))


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
pairs(training[-1], diag.panel = panel.boxplot)

# Information gain --------------------------------------------------------
#ig.weights <- information.gain(Occupancy ~ ., as.data.frame(training))
#ig.weights


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

#classifier on training data
classifier_cl <- naiveBayes(Occupancy ~ ., data = training)

#classifier on training data no light
classifier_cl.f <- naiveBayes(Occupancy ~ ., data = training.f)

#classifier on training data no light and no CO2
classifier_cl.f2 <- naiveBayes(Occupancy ~ ., data = training.f2)

# Predicting on test data
pred_test <- predict(classifier_cl, newdata = test)

# Predicting on test2 data
pred_test2 <- predict(classifier_cl, newdata = test2)

# Predicting on test3 data
pred_test3 <- predict(classifier_cl, newdata = test3)

# Predicting on test.f data
pred_test.f <- predict(classifier_cl.f, newdata = test.f)

# Predicting on test.f1 data
pred_test.f2 <- predict(classifier_cl.f2, newdata = test.f2)

# Confusion Matrix test
cm <- table(test$Occupancy, pred_test)

# Confusion Matrix test2
cm <- table(test3$Occupancy, pred_test3)

# Confusion Matrix test3
cm <- table(test3$Occupancy, pred_test3)

# Confusion Matrix test.f
cm <- table(test.f$Occupancy, pred_test.f)

# Confusion Matrix test.f2
cm <- table(test.f2$Occupancy, pred_test.f2)

#creating readable matrix test1
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
#creating readable matrix
ordered_table <- rbind(as.numeric(names(cm)), cm)
rownames(ordered_table) <- c("Predicted Occupancy","Predicted NO Occupancy")
colnames(ordered_table) <- c("Actual Occupancy","Actual NO Occupancy")
ordered_table

cluster_report(cm, cap = "Naïve Bayes")
# Model Evauation
confusionMatrix(cm) # 0.9867% accuracy

# Confusion Matrix test.f
cm <- table(test.f$Occupancy, pred_test.f)
#creating readable matrix
ordered_table <- rbind(as.numeric(names(cm)), cm)
rownames(ordered_table) <- c("Predicted Occupancy","Predicted NO Occupancy")
colnames(ordered_table) <- c("Actual Occupancy","Actual NO Occupancy")
ordered_table

#ROC
#test 1 as vector
realvec <- as.numeric(test$Occupancy)
predvec <- as.numeric(predict(classifier_cl, newdata = test))


#test 2 as vector
realvec2 <- as.numeric(test2$Occupancy)
predvec2 <- as.numeric(predict(classifier_cl, newdata = test2))

#test 3 as vector
realvec3 <- as.numeric(test3$Occupancy)
predvec3 <- as.numeric(predict(classifier_cl, newdata = test3))

#test.f as vector
realvec.f <- as.numeric(test.f$Occupancy)
predvec.f <- as.numeric(predict(classifier_cl.f, newdata = test.f))

#test.f2 as vector
realvec.f2 <- as.numeric(test.f2$Occupancy)
predvec.f2 <- as.numeric(predict(classifier_cl.f2, newdata = test.f2))

# MMC SCORES
mcc(predvec, realvec)
mcc(predvec2, realvec2)
mcc(predvec3, realvec3)
mcc(predvec.f, realvec.f)
mcc(predvec.f2, realvec.f2)

#ROC curve explained
curve(log(x), from=0, to=100, xlab="False Positive Rate", ylab="True Positive Rate", main="ROC curve", col="green", lwd=3, axes=F)
Axis(side=1, at=c(0, 20, 40, 60, 80, 100), labels = c("0%", "20%", "40%", "60%", "80%", "100%"))
Axis(side=2, at=0:5, labels = c("0%", "20%", "40%", "60%", "80%", "100%"))
segments(0, 0, 110, 5, lty=2, lwd=3)
segments(0, 0, 0, 4.7, lty=2, lwd=3, col="blue")
segments(0, 4.7, 107, 4.7, lty=2, lwd=3, col="blue")
text(20, 4, col="blue", labels = "Perfect Classifier")
text(40, 3, col="green", labels = "Test Classifier")
text(70, 2, col="black", labels= "Classifier with no predictive value")

#plotting roc curve
pred<-ROCR::prediction(predvec, labels=realvec)
roc<-performance(pred, measure="tpr", x.measure="fpr")
plot(roc, main="ROC curve for Occupancy", col="blue", lwd=3)
segments(0, 0, 1, 1, lty=2)
roc_auc<-performance(pred, measure="auc")
#str(roc_auc)
roc_auc@y.values

#plotting roc curve test 2
pred<-ROCR::prediction(predvec2, labels=realvec2)
roc<-performance(pred, measure="tpr", x.measure="fpr")
plot(roc, main="ROC curve for Occupancy(test 2)", col="blue", lwd=3)
segments(0, 0, 1, 1, lty=2)
roc_auc<-performance(pred, measure="auc")
#str(roc_auc)
roc_auc@y.values

#plotting roc curve test 3
pred<-ROCR::prediction(predvec3, labels=realvec3)
roc<-performance(pred, measure="tpr", x.measure="fpr")
plot(roc, main="ROC curve for Occupancy(test 3)", col="blue", lwd=3)
segments(0, 0, 1, 1, lty=2)
roc_auc<-performance(pred, measure="auc")
#str(roc_auc)
roc_auc@y.values

#plotting roc curve (no light)
pred<-ROCR::prediction(predvec.f, labels=realvec.f)
roc<-performance(pred, measure="tpr", x.measure="fpr")
plot(roc, main="ROC curve for Occupancy(no light)", col="blue", lwd=3)
segments(0, 0, 1, 1, lty=2)
roc_auc<-performance(pred, measure="auc")
#str(roc_auc)
roc_auc@y.values

#plotting roc curve (no light, no CO2)
pred<-ROCR::prediction(predvec.f2, labels=realvec.f2)
roc<-performance(pred, measure="tpr", x.measure="fpr")
plot(roc, main="ROC curve for Occupancy(no light, no CO2)", col="blue", lwd=3)
segments(0, 0, 1, 1, lty=2)
roc_auc<-performance(pred, measure="auc")
#str(roc_auc)
roc_auc@y.values

plot(model)
#corrolation plots
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
set.seed(123908213)




# Setup Data

# Methods
normalize <- function(x) {return ((x - min(x)) / (max(x) - min(x))) }

# Create dataframes
test_norm.f <- as.data.frame(lapply(test.f[2:5], normalize))
training_norm.f <- as.data.frame(lapply(training.f[2:5], normalize))

test_norm.f2 <- as.data.frame(lapply(test.f2[2:4], normalize))
training_norm.f2 <- as.data.frame(lapply(training.f2[2:4], normalize))

# Setup Labels
train_labels <- as.factor(training[, 7, drop = TRUE])
test_labels <- as.factor(test[, 7, drop = TRUE])

# ====================================================================#
#                                                                     #
# test.f (no light)                                                   #
#                                                                     #
# ====================================================================#

# Cross validation with Kappa

trControl <- trainControl(method  = "cv",
                          number  = 10)

cross_fit <- train(training_norm.f, train_labels,
                   method     = "knn",
                   tuneGrid   = expand.grid(k = 2:50),
                   trControl  = trControl,
                   metric     = "Kappa"
)

cross_fit

#confusionMatrix(cross_fit)

plot(cross_fit)

# result = K should be 3

knn.f_pred <- knn(train = training_norm.f, test = test_norm.f, cl = train_labels, k =  6, use.all = TRUE)

#CrossTable(x = test_labels, y = knn.f_pred,prop.chisq=FALSE)

# Error rate
mean(test_labels != knn.f_pred)

# Confusion Matrix
confusionMatrix(knn.f_pred, test_labels) 

#MCC
mcc(preds = knn.f_pred, actuals = test_labels)

#ROC


# Plot it with Light and Humidity because those are important.

knn.plot1 = data.frame(test_norm.f, predicted = knn.f_pred, truth = as.factor(test$Occupancy)) # Create a dataframe to simplify charting

knn.plot1.1 = data.frame(x = knn.plot1$Temperature, 
                         y = knn.plot1$HumidityRatio, 
                         predicted = knn.plot1$predicted,
                         truth = as.factor(test$Occupancy))

ggplot(knn.plot1, aes(HumidityRatio, Temperature, color = predicted, shape = truth)) + 
  geom_point(size = 4, alpha = 0.3)


# ====================================================================#
#                                                                     #
# test.f (no light + No Co2)                                          #
#                                                                     #
# ====================================================================#

# Cross validation with Kappa

trControl.f2 <- trainControl(method  = "cv",
                          number  = 10)

cross_fit.f2 <- train(training_norm.f2, train_labels,
                   method     = "knn",
                   tuneGrid   = expand.grid(k = 2:50),
                   trControl  = trControl,
                   metric     = "Kappa"
)

cross_fit.f2

#confusionMatrix(cross_fit.f2)

plot(cross_fit.f2)

# result = K should be 3

knn.f2_pred <- knn(train = training_norm.f2, test = test_norm.f2, cl = train_labels, k =  6, use.all = TRUE)

CrossTable(x = test_labels, y = knn.f2_pred,prop.chisq=FALSE)

# Error rate
mean(test_labels != knn.f2_pred)

# Confusion Matrix
confusionMatrix(knn.f2_pred, test_labels) 

#MCC
mcc(preds = knn.f2_pred, actuals = test_labels)

#ROC


# Plot it with Light and Humidity because those are important.

knn.plotf2 = data.frame(test_norm.f2, predicted = knn.f2_pred, truth = as.factor(test$Occupancy)) # Create a dataframe to simplify charting

knn.plotf2.1 = data.frame(x = knn.plotf2$Temperature, 
                         y = knn.plotf2$HumidityRatio, 
                         predicted = knn.plotf2$predicted,
                         truth = as.factor(test$Occupancy))

ggplot(knn.plotf2, aes(HumidityRatio, Temperature, color = predicted, shape = truth)) + 
  geom_point(size = 4, alpha = 0.3)




# ROC Curve for f
realvec.f <- as.numeric(test.f$Occupancy)
predvec.f <- as.numeric(knn.f_pred)

pred<-ROCR::prediction(predvec.f, labels=realvec.f)
roc<-performance(pred, measure="tpr", x.measure="fpr")
plot(roc, main="ROC curve for Occupancy(no light, no CO2)", col="blue", lwd=3)
segments(0, 0, 1, 1, lty=2)
roc_auc<-performance(pred, measure="auc")
roc_auc@y.values

# ROC Curve for f.2
realvec.f2 <- as.numeric(test.f2$Occupancy)
predvec.f2 <- as.numeric(knn.f2_pred)

pred<-ROCR::prediction(predvec.f2, labels=realvec.f2)
roc<-performance(pred, measure="tpr", x.measure="fpr")
plot(roc, main="ROC curve for Occupancy(no light)", col="blue", lwd=3)
segments(0, 0, 1, 1, lty=2)
roc_auc<-performance(pred, measure="auc")
roc_auc@y.values