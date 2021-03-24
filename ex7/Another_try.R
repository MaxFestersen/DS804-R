library(tidyverse)

dice_roller <- function(n) {
  dice <- expand.grid(1:6, 1:6) %>% # all combinations of two dice
    transmute(total = Var1 + Var2) # total eyes of dice
  sample(dice$total, size = n, replace = T) # sampling n eye outcomes with replacement
}

lapply(lapply(c(5, 10, 50, 100, 1000, 10000), dice_roller), hist) # visualizing different n's 


# Andet forsøg ----

# Jeg troede at det antallet af kast, men det var antallet af terninger der er n.
# Så jeg har lige skrevet noget nyt kode, der kan simulerer forskellige antal terninger istedet.

dice_n <- list(1:6, 1:6, 1:6, 1:6, 1:6, 1:6, 1:6, 1:6, 1:6, 1:6) # list of 10 dice

dices <- vector(mode = "list", length = length(dice_n)) # creating empty list

for (i in 1:length(dice_n)) {
  dice <- expand.grid(dice_n[1:i])
  dice$total <- rowSums(dice)
  dices[i] <- dice['total']
} # rolling different amount of dices and finding rowSums


dice_counts <- lapply(lapply(dices, as.data.frame), count, `X[[i]]`) # counting frequency of outcomes

dice_probs <- lapply(dice_counts, mutate, prob = n/sum(n))

names(dice_counts) <- c("1 die", "2 dice", "3 dice", "4 dice", "5 dice", "6 dice", "7 dice", "8 dice", "9 dice", "10 dice") # naming counts

i = 0
lapply(dice_counts, function(x) {
  i <<- i + 1
  plot(x, main = names(dice_counts)[i],
       ylab = "Count",
       xlab = "Eyes")}) # plotting counts

i = 0
lapply(dice_probs, function(x) {
  i <<- i + 1
  plot(x$`X[[i]]`, x$prob, 
       main = names(dice_counts)[i], 
       ylab = "Probability",
       xlab = "Eyes")}) # plotting probabilities
