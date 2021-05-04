# Ex 12 -------------------------------------------------------------------
library(tidyverse) # For convenience
library(rpart) # For decision trees
library(rpart.plot) # For visualizing decision trees

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


# Test training data


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

cat(paste(
  "Decision trees are not allways posible to create, which was the case for some of the tests.",
  "There is quite some randomness involved in the process. The result might the most optimal. One should try adjusting the formula to acchive reults.",
  "Working results can easily be identified by the labels apearing in the top right of the visualization.",
  "By specifiyng what the formula should find, and the method to do so, we could imporve results",
  sep="\n"
))

# > The naïve Bayes classifier --------------------------------------------
# https://www.geeksforgeeks.org/naive-bayes-classifier-in-r-programming/
# Installing Packages
# install.packages("e1071")
# install.packages("caTools")
# install.packages("caret")

# Loading package
library(e1071)
library(caTools)
library(caret)

# splitsting data into train and test data - Done in previous section
# splits <- sample.splits(ToothGrowth, splitsRatio = 0.7)
# train_tooth <- subset(ToothGrowth, splits == "TRUE")
# test_tooth <- subset(ToothGrowth, splits == "FALSE")

# Feature Scaling
train_scale <- scale(select(train_tooth, -supp))
test_scale <- scale(select(test_tooth, -supp))

# Fitting Naive Bayes Model 
# to training dataset
classifier_cl <- naiveBayes(supp ~ ., data = train_tooth)
classifier_cl

# Predicting on test data'
y_pred <- predict(classifier_cl, newdata = test_tooth)

# Confusion Matrix
cm <- table(test_tooth$supp, y_pred)
cm

# Model Evauation
confusionMatrix(cm)


# > k nearest neighbor ----------------------------------------------------



# b) ----------------------------------------------------------------------
# How does the behavior of the k nearest neighbor classifier
# change with the choice of k?

# c) ----------------------------------------------------------------------
# What is the impact of parameter choices on the quality of decision trees?

# d) ----------------------------------------------------------------------
# How does the behavior of the three classifiers change with the amount
# of training data (e.g., choice of training-test-splits)?

