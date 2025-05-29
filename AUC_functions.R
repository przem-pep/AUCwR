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

AUC_wilcox <- function(pred) {
  n <- length(pred)
  n0 <- n / 2
  group1 <- pred[1:n0]
  group2 <- pred[(n0+1):n]
  test <- wilcox.test(group1, group2, exact = FALSE)
  U <- as.numeric(test$statistic)
  n1 <- length(group1)
  n2 <- length(group2)
  auc <- U / (n1 * n2)
  return(auc)
}

# AUC calculation from Mann-Whitney U manual calculation

auc_u <- function(pred) {
  n <- length(pred)
  n0 <- n / 2
  group1 <- pred[1:n0]
  group2 <- pred[(n0+1):n]
  labels <- c(rep(1, length(group1)), rep(2, length(group2)))
  ranks <- rank(pred)
  R1 <- sum(ranks[labels == 1])
  n1 <- length(group1)
  n2 <- length(group2)
  U1 <- n1 * n2 + (n1 * (n1 + 1)) / 2 - R1
  auc <- U1 / (n1 * n2)
  return(auc)
}

# AUC with data.table package

library(data.table)

AUC_dt <- function(pred, target) {
  dt <- data.table(pred = pred, label = target)
  setorder(dt, -pred)  # sort descending by prediction
  
  # Calculate cumulative true positives and false positives
  dt[, `:=`(
    tp = cumsum(label == 1),
    fp = cumsum(label == 0)
  )]
  
  P <- sum(dt$label == 1)
  N <- sum(dt$label == 0)
  
  # Calculate TPR and FPR
  dt[, `:=`(
    TPR = tp / P,
    FPR = fp / N
  )]
  
  # Calculate AUC using trapezoidal rule
  x_diff <- diff(dt$FPR)
  y_avg <- (head(dt$TPR, -1) + tail(dt$TPR, -1)) / 2
  
  auc <- sum(x_diff * y_avg)
  return(auc)
}

cppFunction('
double fast_auc(NumericVector pred, IntegerVector label) {
  int n = pred.size();
  std::vector<std::pair<double, int>> data(n);

  // Combine pred and label into one sortable structure
  for (int i = 0; i < n; ++i) {
    data[i] = std::make_pair(pred[i], label[i]);
  }

  // Sort descending by predicted probability
  std::sort(data.begin(), data.end(), [](const std::pair<double, int>& a, const std::pair<double, int>& b) {
    return a.first > b.first;
  });

  // Rank-based approach for AUC
  double cum_pos = 0.0;
  double cum_neg = 0.0;
  double auc = 0.0;

  for (int i = 0; i < n; ++i) {
    if (data[i].second == 1) {
      cum_pos += 1;
    } else {
      auc += cum_pos;
      cum_neg += 1;
    }
  }

  if (cum_pos == 0 || cum_neg == 0) return NA_REAL;

  return auc / (cum_pos * cum_neg);
}
')
