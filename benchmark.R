library(microbenchmark)
library(Rcpp)
library(reticulate)
sourceCpp("AUC_cpp.cpp")
use_python(Sys.which("python"), required = TRUE)
source_python("auc_python.py")
roc_auc_score <- import("sklearn.metrics")$roc_auc_score

# Pierwszy zbiór danych

default1 <- c(rep(1, 5000), rep(0, 5000))

score_1 <- c(rnorm(5000), rnorm(5000, 1))

bigstatsr::AUC(-score_1, default1)

# Drugi zbiór danych

score_2 <- round(score_1)

# Trzeci zbiór danych

score_3 <- c(rnorm(50000), rnorm(50000, 1))

default2 <- c(rep(1, 50000), rep(0, 50000))

# Czwarty zbiór danych

score_4 <- round(score_3)


# Obliczenia

call_benchmark <- function(pred, target, times = 100) {
  
  my_benchmark <- microbenchmark(
    
    ROCR::performance(ROCR::prediction(-pred, target), "auc")@y.values[[1]],
    
    pROC::auc(target, pred, lev=c('0', '1'), dir=">"),
    
    mltools::auc_roc(-pred, target),
    
    (Hmisc::somers2(-pred, target)['Dxy']+1)/2,
    
    bigstatsr::AUC(-pred, target),
    
    AUC_in_tidyverse(-pred, target),
    
    AUC_cpp(-pred, target),
    
    auc_u(pred),
    
    AUC_wilcox(pred),
    
    scorecard::perf_eva(-pred, target, binomial_metric = "auc", show_plot = FALSE)$binomial_metric$dat$AUC,
    
    scikitlearn_auc(-pred, target),
    
    caTools::colAUC(-pred, target)[1, 1],
    
    AUC_dt(-pred, target),
    
    precrec::auc(precrec::evalmod(precrec::mmdata(scores = -pred, labels = target)))[1, "aucs"],
    
    roc_auc_score(target, -pred),
    
    fast_auc(-pred, target),
    
    yardstick::roc_auc_vec(as.factor(target), pred),
    
    (unname(rcompanion::cliffDelta(pred ~ target, data = as.data.frame(cbind(pred, target)))) + 1) / 2,
    
    ModelMetrics::auc(target, -pred),
    
    MLmetrics::AUC(-pred, target),
    
    cvAUC::AUC(-pred, target),
    
    fbroc::perf(fbroc::boot.roc(-pred, as.logical(target)), metric = "auc")$Observed.Performance,
    
    times = times
  )
  
  levels(my_benchmark$expr) <- c("ROCR", "pROC", "mltools", "Hmisc", "bigstatsr",
                               "AUC_in_tidyverse", "AUC_cpp",
                               "own-U", "AUC-wilcox", "scorecard", "scikit-learn",
                               "caTools", "own_data.table_AUC", "precrec", "scikit-learn2",
                               "Rcpp", "yardstick", "rcompanion", "ModelMetrics", "MLmetrics",
                               "cvAUC", "fbroc")
  
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
