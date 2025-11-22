# AUC calculation with the pairwise comparison method

own_AUC_pairwise <- function(pred, target) {
  indices <- order(pred, target)
  pred <- pred[indices]
  pred0 <- pred[target[indices] == 0]
  pred1 <- pred[target[indices] == 1]
  return(auc_pairwise_cpp(pred0, pred1))
}

# AUC calculation with the trapezoid method

own_AUC_trapezoid <- function(pred, target) {
  indices <- order(pred, target, decreasing = TRUE)
  pred <- pred[indices]
  target <- target[indices]
  p <- sum(target)
  num_uniq <- length(unique(pred))
  return(auc_trapezoid_cpp(pred, target, p, num_uniq))
}

# AUC calculation with the rank sum method

own_AUC_ranks <- function(pred, target) {
  t = target == 1
  n1 = sum(t)
  auc = (mean(rank(pred)[t]) - (n1 + 1) / 2) / (length(target) - n1)
  return(auc)
}
