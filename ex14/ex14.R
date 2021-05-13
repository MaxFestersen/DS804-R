# Exercise 14 / Classification project ------------------------------------




# Dataset -----------------------------------------------------------------

test <- read.table("datatest.txt", sep = ",")
test2 <- read.table("datatest2.txt", sep = ",")
training <- read.table("datatraining.txt", sep = ",")


# Choice of Algorithms ----------------------------------------------------


## Decision Tree ----------------------------------------------------------


# Decision Tree -----------------------------------------------------------
 


## Support Vectors and Margin (SVM)----------------------------------------






## Neural Network ----------------------------------------------------------
library(neuralnet)
set.seed(42069701051146) # 420 69 yolo swag

training$Occupancy <- factor(training$Occupancy)

net <- neuralnet(Occupancy ~ Temperature + Humidity + Light + CO2 + HumidityRatio,
                 data = training,
                 hidden = 3,
                 linear.output = FALSE, 
                 err.fct = 'ce', 
                 likelihood = TRUE)


plot(net)




## Naïve Bayes ------------------------------------------------------------

