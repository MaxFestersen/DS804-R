# Ex 12 -------------------------------------------------------------------
library(tidyverse) # For convenience
library(rpart) # For decision trees
library(rpart.plot) # For visualizing decision trees
library(e1071) # For naiveBayes algorithm - great package name btw.
#library(caTools) # modefies some base code and some other stuff.
library(caret) # For confusionMatrix
library(class) # For kNN

set.seed(120)  # Setting Seed

# a) ----------------------------------------------------------------------
# Some dataset: ToothGrowth
head(ToothGrowth, 5)
unique(ToothGrowth$supp)
unique(ToothGrowth$dose)
unique(ToothGrowth$len)
data <- ToothGrowth %>%
  rowwise() %>% # Work with rows (to perform task on each row)
  mutate("len_rate" = ifelse(len>7, # categorise by len
                             ifelse(
                               len>14,
                               ifelse(
                                 len>21,
                                 ifelse(
                                   len>28,
                                   "29<"
                                   ,
                                   "22-28"
                                 ),
                                 "15-21"
                               ),
                               "7-14"
                             ),
                             "<=7"
                      )
  ) %>% 
  select(-len) # Do not select len
unique(data$len_rate)


# > Decision trees --------------------------------------------------------

# Find supplement (supp) from categorised teeth length and dose
data.supp <- rpart(formula = supp ~ .,
      data = data
      )

data.supp.method.class <- rpart(formula = supp ~ .,
                   data = data,
                   method = "class" 
)

data.supp.method.poisson <- rpart(formula = supp ~ .,
                   data = data,
                   method = "poisson" 
)

# Find dose from categorised teeth length and supplement
data.dose <- rpart(formula = dose ~ .,
      data = data
      )

data.dose.method.class <- rpart(formula = dose ~ .,
                                data = data,
                                method = "class" 
)

data.dose.method.poisson <- rpart(formula = dose ~ .,
                                data = data,
                                method = "poisson" 
)

# Find supplement (supp) from  teeth length and dose
orr.supp <- rpart(formula = supp ~ .,
      data = ToothGrowth
      )

# Find dose from teeth length and supplement
orr.dose <- rpart(formula = dose ~ .,
      data = ToothGrowth
      )

# Training data
# Preparation:
# > have data
# > Set seed

# Step 1: shuffle sorted data
shuffle_index <- sample(1:nrow(ToothGrowth))
head(shuffle_index)
ToothGrowth <- ToothGrowth[shuffle_index, ]

# Step 2: clean data
# > Drop variables
# >> eg. select or subset
# > Create factor variables
# >> eg. factor
# > Drop the NA
# >> Eg. na.omit

# Note: not needed for ToothGrowth

# Step 3: Training and test set creation
# splitsting data into train and test data
# > Base method
splits <- sample(seq_len(nrow(ToothGrowth)), nrow(ToothGrowth)*0.8)
train_tooth <- ToothGrowth[splits, ]
test_tooth <- ToothGrowth[-splits, ]

# > caTools method
# splits <- sample.split(ToothGrowth, splitsRatio = 0.8) # 80/20 is most common, but do whatever
# train_tooth <- subset(ToothGrowth, splits == "TRUE")
# test_tooth <- subset(ToothGrowth, splits == "FALSE")

# Optional: check dimentions
dim(train_tooth)
nrow(train_tooth)
ncol(train_tooth)

# Optional: use prep.table/table to vertify randomization
prop.table(table(train_tooth$dose))

# Step 4: Build model
trained_tooth <- rpart(dose~., data = train_tooth, method = 'class')

# Step 5: make prediction
predict_w_trained_tooth <- predict(trained_tooth, test_tooth, type = 'class')

# Optional: get accuracy
confusionmatrix_tooth <- table(test_tooth$dose, predict_w_trained_tooth)

accuracy_tooth <- sum(diag(confusionmatrix_tooth)) / sum(confusionmatrix_tooth)
accuracy_tooth

# Step 7: tuning (of hyper parameters)
# Adjusting hyperparameters
control <- rpart.control(minsplit = 8, minbucket = 8/2, cp = 0.001) # for adjusting hyperparameters
# Minsplit: most optimal for this data at 5-48
# minbucket and cp don't really do all that much for this data.

trained_tooth_2 <- rpart(dose~., data = train_tooth, method = "class", control = control)

predict_w_trained_tooth_2 <-predict(trained_tooth_2, test_tooth, type = 'class')

confusionmatrixtest_tooth <- table(test_tooth$dose, predict_w_trained_tooth_2)

accuracytest_tooth <- sum(diag(confusionmatrixtest_tooth)) / sum(confusionmatrixtest_tooth)
accuracytest_tooth

# Visualize decision treee results
rpart.plot(data.supp)
rpart.plot(data.supp.method.class)
rpart.plot(data.supp.method.poisson)
rpart.plot(data.dose)
rpart.plot(data.dose.method.class)
rpart.plot(data.dose.method.poisson)
rpart.plot(orr.supp)
rpart.plot(orr.dose)
rpart.plot(trained_tooth)
rpart.plot(trained_tooth_2)

cat(paste(
  "Decision trees are not allways posible to create, which was the case for some of the tests.",
  "There is quite some randomness involved in the process. The result might the most optimal. One should try adjusting the formula to acchive reults.",
  "Working results can easily be identified by the labels apearing in the top right of the visualization.",
  "Training the data could improve results (try if nothing else works).",
  "By specifiyng what the formula should find, and the method to do so, we could imporve results",
  sep="\n"
))

# > The naïve Bayes classifier --------------------------------------------
# https://www.geeksforgeeks.org/naive-bayes-classifier-in-r-programming/

# Splitsting data into train and test data - Done in previous section
# splits <- sample.splits(ToothGrowth, splitsRatio = 0.7)
# train_tooth <- subset(ToothGrowth, splits == "TRUE")
# test_tooth <- subset(ToothGrowth, splits == "FALSE")

# Feature Scaling
# Note: not needed for this data
# train_scale <- scale(select(train_tooth, -supp)) # scalce - base code - centers valeus / similar too nomalization
# test_scale <- scale(select(test_tooth, -supp))

# Factoring columns - sometimes needed?
train_tooth <- train_tooth %>% 
  mutate(dose = factor(dose))
test_tooth <- test_tooth %>% 
  mutate(dose = factor(dose))


# Fitting Naive Bayes Model to training dataset
classifier_cl <- naiveBayes(dose ~ ., data = train_tooth)
classifier_cl

# Predicting on test data
y_pred <- predict(classifier_cl, newdata = test_tooth, type = "class")

# Confusion Matrix
cm <- table(test_tooth$dose, y_pred)
cm

# Model Evauation
confusionMatrix(cm)

sum(diag(cm)) / sum(cm) # Accuracy for Naïve bayers



# > k nearest neighbor ----------------------------------------------------
train <- train_tooth
test <- test_tooth
cl <- train_tooth[1,1] #ingen anelse om, hvad det her er
knn(train, test, cl, k = 2, l = 0, prob = FALSE, use.all = TRUE)


# b) ----------------------------------------------------------------------
# How does the behavior of the k nearest neighbor classifier
# change with the choice of k?

# c) ----------------------------------------------------------------------
# What is the impact of parameter choices on the quality of decision trees?

# d) ----------------------------------------------------------------------
# How does the behavior of the three classifiers change with the amount
# of training data (e.g., choice of training-test-splits)?

