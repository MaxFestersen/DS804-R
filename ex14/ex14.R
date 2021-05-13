# Exercise 14 / Classification project ------------------------------------




# Dataset -----------------------------------------------------------------

test <- read.table("datatest.txt", sep = ",")
test2 <- read.table("datatest2.txt", sep = ",")
training <- read.table("datatraining.txt", sep = ",")


# Decision Tree -----------------------------------------------------------
 




# SVM ---------------------------------------------------------------------




# Neural Network ----------------------------------------------------------
library(neuralnet)

training$Occupancy <- factor(training$Occupancy)

net <- neuralnet(Occupancy ~ Temperature + Humidity + Light + CO2 + HumidityRatio,
                 data = training,
                 hidden = 3,
                 linear.output = FALSE, 
                 err.fct = 'ce', 
                 likelihood = TRUE)


plot(net)


