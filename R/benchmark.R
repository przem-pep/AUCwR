library(tidyverse)
library(microbenchmark)
library(reticulate)
use_python(Sys.which("python"), required = TRUE)
roc_auc_score <- import("sklearn.metrics")$roc_auc_score

# First dataset (n = 1000)

pred1 <- c(rnorm(500), rnorm(500, 1))

target1 <- c(rep(1, 500), rep(0, 500))

# Second dataset (n = 10000)

pred2 <- c(rnorm(5000), rnorm(5000, 1))

target2 <- c(rep(1, 5000), rep(0, 5000))

# Third dataset (n = 100000)

pred3 <- c(rnorm(50000), rnorm(50000, 1))

target3 <- c(rep(1, 50000), rep(0, 50000))


# Calculations

call_benchmark <- function(pred, target, times = 100) {
  
  benchmark <- microbenchmark(
    
    ROCR::performance(ROCR::prediction(-pred, target), "auc")@y.values[[1]],
    
    pROC::auc(target, pred, lev=c('0', '1'), dir=">"),
    
    mltools::auc_roc(-pred, target),
    
    Hmisc::somers2(-pred, target)["C"],
    
    bigstatsr::AUC(-pred, target),
    
    scorecard::perf_eva(-pred, target, binomial_metric = "auc", show_plot = FALSE)$binomial_metric$dat$AUC,
    
    caTools::colAUC(-pred, target)[1, 1],
    
    precrec::evalmod(precrec::mmdata(scores = -pred, labels = target), mode = "aucroc")$uaucs$aucs,
    
    roc_auc_score(target, -pred),
    
    yardstick::roc_auc_vec(as.factor(target), pred),
    
    (rcompanion::cliffDelta(x = pred[target == 0], y = pred[target == 1], digits = 100) + 1) / 2,
    
    ModelMetrics::auc(target, -pred),
    
    MLmetrics::AUC(-pred, target),
    
    cvAUC::AUC(-pred, target),
    
    fbroc::boot.roc(-pred, as.logical(target))$auc,
    
    DescTools::Cstat(-pred, target),
    
    effsize::VD.A(pred ~ factor(target))$estimate,
    
    rcompanion::vda(x = pred[target == 0], y = pred[target == 1], digits=100),
    
    times = times
  )
  
  levels(benchmark$expr) <- c("ROCR", "pROC",
                                 "mltools", "Hmisc", "bigstatsr", "scorecard",
                                 "caTools", "precrec", "scikit-learn",
                                 "yardstick", "rcompanion::cliffDelta", "ModelMetrics", "MLmetrics",
                                 "cvAUC", "fbroc", "DescTools", "effsize", "rcompanion::vda"
                                 )
  
  benchmark$expr <- reorder(benchmark$expr, -benchmark$time, FUN = median)
  
  return(benchmark)
}

plot_benchmark <- function(benchmark) {
  autoplot(benchmark) +
    theme(axis.text.y = element_text(size = rel(1.5)))
}

# Benchmark 1

benchmark1 <- call_benchmark(pred1, target1)
plot_benchmark(benchmark1)
saveRDS(benchmark1, file = "data/functions_benchmark_1.rds")

# Benchmark 2

benchmark2 <- call_benchmark(pred2, target2)
plot_benchmark(benchmark2)
saveRDS(benchmark2, file = "data/functions_benchmark_2.rds")

# Benchmark 3

benchmark3 <- call_benchmark(pred3, target3)
plot_benchmark(benchmark3)
saveRDS(benchmark3, file = "data/functions_benchmark_3.rds")

