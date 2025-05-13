library(tidyverse)

# AUC calculation with for loop

own_for_loop <- function(pred, target) {
  table <- tibble(pred = pred,
                  target = target) %>%
    group_by(pred) %>%
    summarise(target = sum(target),
              n = n()) %>%
    arrange(desc(pred)) %>%
    mutate(TP = 0,
           FP = 0,
           TPR = 0,
           FPR = 0)
  P <- sum(table$target)
  N <- sum(table$n - table$target)
  TP <- 0
  FP <- 0
  for(i in 1:nrow(table)) {
    TP <- TP + table[i, "target"]
    FP <- FP + table[i, "n"] - table[i, "target"]
    table[i, "TP"] <- TP
    table[i, "FP"] <- FP
    table[i, "TPR"] <- TP / P
    table[i, "FPR"] <- FP / N
  }
  return(sum(diff(table$FPR) * (head(table$TPR, -1) + tail(table$TPR, -1)) * 0.5))
}

# AUC calculation with vectors in the tidyverse

AUC_in_tidyverse <- function(pred, target) {
  table <- tibble(pred = pred,
                  target = target) %>%
    group_by(pred) %>%
    summarise(target = sum(target),
              n = n()) %>%
    arrange(desc(pred))
  P <- sum(table$target)
  N <- sum(table$n - table$target)
  table <- table %>%
    mutate(
      TP = cumsum(target),
      FP = cumsum(n - target),
      TPR = TP / P,
      FPR = FP / N
    )
  return(sum(diff(table$FPR) * (head(table$TPR, -1) + tail(table$TPR, -1)) * 0.5))
}

# AUC in C++

AUC_cpp <- function(pred, target) {
  table <- tibble(pred = pred,
                  target = target) %>%
    group_by(pred) %>%
    summarise(target = sum(target),
              n = n()) %>%
    mutate(negatives = n - target) %>%
    arrange(desc(pred))
  AUC_calc(table$pred, table$target, table$n, table$negatives)
}

# AUC from Wilcox test

AUC_wilcox <- function(group1, group2) {
  test <- wilcox.test(group1, group2, exact = FALSE)
  U <- as.numeric(test$statistic)
  n1 <- length(group1)
  n2 <- length(group2)
  auc <- U / (n1 * n2)
  return(auc)
}

# AUC calculation from Mann-Whitney U manual calculation

auc_u <- function(group1, group2) {
  combined <- c(group1, group2)
  labels <- c(rep(1, length(group1)), rep(2, length(group2)))
  ranks <- rank(combined)
  R1 <- sum(ranks[labels == 1])
  n1 <- length(group1)
  n2 <- length(group2)
  U1 <- n1 * n2 + (n1 * (n1 + 1)) / 2 - R1
  auc <- U1 / (n1 * n2)
  return(auc)
}
