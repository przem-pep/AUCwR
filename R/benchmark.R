library(microbenchmark)
library(Rcpp)
library(reticulate)
sourceCpp("AUC_cpp.cpp")
use_python(Sys.which("python"), required = TRUE)
source_python("auc_python.py")
roc_auc_score <- import("sklearn.metrics")$roc_auc_score

# First dataset

default1 <- c(rep(1, 5000), rep(0, 5000))

score_1 <- c(rnorm(5000), rnorm(5000, 1))

bigstatsr::AUC(-score_1, default1)

# Second dataset

score_2 <- round(score_1)

# Third dataset

score_3 <- c(rnorm(50000), rnorm(50000, 1))

default2 <- c(rep(1, 50000), rep(0, 50000))

# Fourth dataset

score_4 <- round(score_3)


# Calculations

call_benchmark <- function(pred, target, times = 100) {
  
  my_benchmark <- microbenchmark(
    
    ROCR::performance(ROCR::prediction(-pred, target), "auc")@y.values[[1]],
    
    pROC::auc(target, pred, lev=c('0', '1'), dir=">"),
    
    mltools::auc_roc(-pred, target),
    
    Hmisc::somers2(-pred, target)["C"],
    
    bigstatsr::AUC(-pred, target),
    
    own_AUC_tidyverse(-pred, target),
    
    own_AUC_U(-pred, target),
    
    own_AUC_wilcox(pred, target),
    
    scorecard::perf_eva(-pred, target, binomial_metric = "auc", show_plot = FALSE)$binomial_metric$dat$AUC,
    
    # scikitlearn_auc(-pred, target),
    
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
    
    own_AUC_U_cpp(-pred,  target),
    
    times = times
  )
  
  levels(my_benchmark$expr) <- c("ROCR", "pROC",
                                 "mltools", "Hmisc", "bigstatsr",
                                 "own_AUC_tidyverse", "own_Mann_Whitney_U",
                                 "own_AUC_Wilcox", "scorecard", # "scikit-learn",
                                 "caTools", "precrec", "scikit-learn",
                                 "yardstick", "rcompanion::cliffDelta", "ModelMetrics", "MLmetrics",
                                 "cvAUC", "fbroc", "DescTools", "effsize", "rcompanion::vda",
                                 "own_AUC_U_cpp")
  
  my_benchmark$expr <- reorder(my_benchmark$expr, -my_benchmark$time, FUN = median)
  
  autoplot(my_benchmark) +
    theme(axis.text.y = element_text(size = rel(1.5)))
}

# Benchmark 1

call_benchmark(score_1, default1)

# Benchmark 2

call_benchmark(score_2, default1)

# Benchmark 3

call_benchmark(score_3, default2)

# Benchmark 4

call_benchmark(score_4, default2)
