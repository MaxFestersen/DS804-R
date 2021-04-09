library(tidyverse)

precision <- function(x) {
  precision <- c()
  for (i in 1:ncol(x)) {
    tp <- max(x[,i]) # finding tp for each column
    row_i <- which.max(x[,i])[[1]] # finding row_index of each tp
    tp_fp <- sum(x[row_i,]) # sum of row for each tp
    precision <- c(precision, tp/tp_fp) # precision = tp/row
  }
  precision
}

recall <- function(x) {
  recall <- c()
  for (i in 1:ncol(x)) {
    tp <- max(x[,i]) # finding tp for each column
    tp_fn <- sum(x[,i]) # sum of each column
    recall <- c(recall, tp/tp_fn) # recall = tp/column
  }
  recall
}

f1 <- function(x) {
  2 * (precision(x) * recall(x)) / (precision(x) + recall(x))
}

support <- function(x) {
  support <- c()
  for (i in 1:ncol(x)) {
    support <- c(support, sum(x[,i]))
  }
  support
}

macro <- function(x) {
  macro_precision <- mean(precision(x))
  macro_recall <- mean(recall(x))
  macro_f1 <- 2 * (macro_precision * macro_recall) / (macro_precision + macro_recall)
  out <- data.frame(precision = macro_precision, recall = macro_recall, f1 = macro_f1, support = sum(x), row.names = "Macro avg")
  out
}

micro <- function(x, digits) {
  tp <- c()
  for (i in 1:ncol(x)) {
    tp <- c(tp, max(x[,i]))
  }
  out <- data.frame(precision = "", recall = "", f1 = round(sum(tp)/sum(x), digits), support = sum(x), row.names = "Accuracy")
  out
}

weighted.avg <- function(x) {
  w_precision <- sum(precision(x)*colSums(x)/sum(x))
  w_recall <- sum(recall(x)*colSums(x)/sum(x))
  w_f1 <- sum(f1(x)*colSums(x)/sum(x))
  out <- data.frame(precision = w_precision, recall = w_recall, f1 = w_f1, support = sum(x), row.names = "Weighted avg")
}

cluster_report <- function(x, digits = 5) {
  out <- round(cbind(precision = precision(x), recall = recall(x), f1 = f1(x), support = support(x)), digits) %>%  
    rbind(c("", "", ""), micro(x, digits), round(macro(x), digits), round(weighted.avg(x), digits)) # binding it all together
  out
}


