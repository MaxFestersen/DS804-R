library("tidyverse")
library("highcharter")
# Exercise 7-3 ------------------------------------------------------------
results = tribble(~"#dice", ~"eyes", ~"chance")
for (n.dice in 1:2){
  min = n.dice
  max = n.dice*6
  for(eye in min:max){
    results = results %>%
      add_row(`#dice` = n.dice, eyes = eye, chance = eye/)
  }
}


dice <- expand.grid(1:6, 1:6) %>% 
  mutate(total = Var1 + Var2)
hist(dice$total)
dice.2 <- rep(dice, 50)
hist(dice.2$total)
