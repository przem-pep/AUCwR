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

# Benchmark 1

benchmark1 <- (microbenchmark(
  
  ROCR::performance(ROCR::prediction(-score_1, default1), "auc")@y.values[[1]],
  
  pROC::auc(default1, score_1, lev=c('0', '1'), dir=">"),
  
  mltools::auc_roc(-score_1, default1),
  
  (Hmisc::somers2(-score_1, default1)['Dxy']+1)/2,
  
  bigstatsr::AUC(-score_1, default1),
  
  AUC_in_tidyverse(-score_1, default1),
  
  AUC_cpp(-score_1, default1),
  
  auc_u(score_1[1:5000], score_1[5001:10000]),
  
  AUC_wilcox(score_1[1:5000], score_1[5001:10000]),
  
  scorecard::perf_eva(-score_1, default1, binomial_metric = "auc", show_plot = FALSE)$binomial_metric$dat$AUC,
  
  scikitlearn_auc(-score_1, default1),
  
  caTools::colAUC(-score_1, default1)[1, 1],
  
  AUC_dt(-score_1, default1),
  
  precrec::auc(precrec::evalmod(precrec::mmdata(scores = -score_1, labels = default1)))[1, "aucs"],
  
  roc_auc_score(default1, -score_1),
  
  fast_auc(-score_1, default1)
  
))

# Benchmark 2

benchmark2 <- (microbenchmark(
  
  ROCR::performance(ROCR::prediction(-score_2, default1), "auc")@y.values[[1]],
  
  pROC::auc(default1, score_2, lev=c('0', '1'), dir=">"),
  
  mltools::auc_roc(-score_2, default1),
  
  (Hmisc::somers2(-score_2, default1)['Dxy']+1)/2,
  
  bigstatsr::AUC(-score_2, default1),
  
  AUC_in_tidyverse(-score_2, default1),
  
  AUC_cpp(-score_2, default1),
  
  auc_u(score_2[1:5000], score_2[5001:10000]),
  
  AUC_wilcox(score_2[1:5000], score_2[5001:10000]),
  
  scorecard::perf_eva(-score_2, default1, binomial_metric = "auc", show_plot = FALSE)$binomial_metric$dat$AUC,
  
  scikitlearn_auc(-score_2, default1),
  
  caTools::colAUC(-score_2, default1)[1, 1],
  
  AUC_dt(-score_2, default1),
  
  precrec::auc(precrec::evalmod(precrec::mmdata(scores = -score_2, labels = default1)))[1, "aucs"],
  
  roc_auc_score(default1, -score_2),
  
  fast_auc(-score_2, default1)
  
))

# Benchmark 3

benchmark3 <- (microbenchmark(
  
  ROCR::performance(ROCR::prediction(-score_3, default1), "auc")@y.values[[1]],
  
  pROC::auc(default1, score_3, lev=c('0', '1'), dir=">"),
  
  mltools::auc_roc(-score_3, default1),
  
  (Hmisc::somers2(-score_3, default1)['Dxy']+1)/2,
  
  bigstatsr::AUC(-score_3, default1),
  
  AUC_in_tidyverse(-score_3, default1),
  
  AUC_cpp(-score_3, default1),
  
  auc_u(score_3[1:50000], score_3[50001:100000]),
  
  AUC_wilcox(score_3[1:50000], score_3[50001:100000]),
  
  scorecard::perf_eva(-score_3, default1, binomial_metric = "auc", show_plot = FALSE)$binomial_metric$dat$AUC,
  
  scikitlearn_auc(-score_3, default1),
  
  caTools::colAUC(-score_3, default1)[1, 1],
  
  AUC_dt(-score_3, default1),
  
  precrec::auc(precrec::evalmod(precrec::mmdata(scores = -score_3, labels = default1)))[1, "aucs"],
  
  roc_auc_score(default1, -score_3),
  
  fast_auc(-score_3, default1),
  
  times = 20L
  
))

# Benchmark 4

benchmark4 <- (microbenchmark(
  
  ROCR::performance(ROCR::prediction(-score_4, default2), "auc")@y.values[[1]],
  
  pROC::auc(default2, score_4, lev=c('0', '1'), dir=">"),
  
  mltools::auc_roc(-score_4, default2),
  
  (Hmisc::somers2(-score_4, default2)['Dxy']+1)/2,
  
  bigstatsr::AUC(-score_4, default2),
  
  AUC_in_tidyverse(-score_4, default2),
  
  AUC_cpp(-score_4, default2),
  
  auc_u(score_4[1:50000], score_4[50001:100000]),
  
  AUC_wilcox(score_4[1:50000], score_4[50001:100000]),
  
  scorecard::perf_eva(-score_4, default2, binomial_metric = "auc", show_plot = FALSE)$binomial_metric$dat$AUC,
  
  scikitlearn_auc(-score_4, default2),
  
  caTools::colAUC(-score_4, default2)[1, 1],
  
  AUC_dt(-score_4, default2),
  
  precrec::auc(precrec::evalmod(precrec::mmdata(scores = -score_4, labels = default2)))[1, "aucs"],
  
  roc_auc_score(default2, -score_4),
  
  fast_auc(-score_4, default2),
  
  times = 20L
  
))

# AUC   = (Gini+1)/2 

# Wykresy

levels(benchmark1$expr) <- c("ROCR", "pROC", "mltools", "Hmisc", "bigstatsr",
                               "AUC_in_tidyverse", "AUC_cpp",
                               "own-U", "AUC-wilcox", "scorecard", "scikit-learn",
                               "caTools", "own_data.table_AUC", "precrec", "scikit-learn2",
                               "Rcpp")

benchmark1$expr <- reorder(benchmark1$expr, -benchmark1$time, FUN = median)


levels(benchmark2$expr) <- c("ROCR", "pROC", "mltools", "Hmisc", "bigstatsr",
                             "AUC_in_tidyverse", "AUC_cpp",
                             "own-U", "AUC-wilcox", "scorecard", "scikit-learn",
                             "caTools", "own_data.table_AUC", "precrec", "scikit-learn2",
                             "Rcpp")

levels(benchmark3$expr) <- c("ROCR", "pROC", "mltools", "Hmisc", "bigstatsr",
                             "AUC_in_tidyverse", "AUC_cpp",
                             "own-U", "AUC-wilcox", "scorecard", "scikit-learn",
                             "caTools", "own_data.table_AUC", "precrec", "scikit-learn2",
                             "Rcpp")

levels(benchmark4$expr) <- c("ROCR", "pROC", "mltools", "Hmisc", "bigstatsr",
                             "AUC_in_tidyverse", "AUC_cpp",
                             "own-U", "AUC-wilcox", "scorecard", "scikit-learn",
                             "caTools", "own_data.table_AUC", "precrec", "scikit-learn2",
                             "Rcpp")

autoplot(benchmark1) +
  theme(axis.text.y = element_text(size = rel(1.5)))

autoplot(benchmark2) +
  theme(axis.text.y = element_text(size = rel(1.5)))

autoplot(benchmark3) +
  theme(axis.text.y = element_text(size = rel(1.5)))

autoplot(benchmark4) +
  theme(axis.text.y = element_text(size = rel(1.5)))
