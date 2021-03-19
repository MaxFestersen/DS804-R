library(tidyverse)

dice_roller <- function(n) {
  dice <- expand.grid(1:6, 1:6) %>% # all combinations of two dice
    transmute(total = Var1 + Var2) # total eyes of dice
  sample(dice$total, size = n, replace = T) # sampling n eye outcomes with replacement
}

lapply(lapply(c(5, 10, 50, 100, 1000, 10000), dice_roller), hist) # visualizing different n's 



