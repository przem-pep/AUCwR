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

own_AUC_tidyverse <- function(pred, target) {
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

# AUC in tidyverse and C++

AUC_tidyverse_cpp <- function(pred, target) {
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

own_AUC_wilcox <- function(pred, target) {
  n <- length(pred)
  group1 <- pred[target == 0]
  group2 <- pred[target == 1]
  test <- wilcox.test(group1, group2, exact = FALSE)
  U <- as.numeric(test$statistic)
  n1 <- length(group1)
  n2 <- length(group2)
  AUC <- U / (n1 * n2)
  return(AUC)
}

# AUC calculation from Mann-Whitney U manual calculation

own_AUC_U <- function(pred, target) {
  n <- length(pred)
  group1 <- pred[target == 0]
  group2 <- pred[target == 1]
  ranks <- rank(pred)
  R1 <- sum(ranks[target == 0])
  n1 <- length(group1)
  n2 <- length(group2)
  U1 <- n1 * n2 + (n1 * (n1 + 1)) / 2 - R1
  AUC <- U1 / (n1 * n2)
  return(AUC)
}
