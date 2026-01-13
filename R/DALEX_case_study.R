library(DALEX)
library(tictoc)
library(microbenchmark)

# Setup

titanic <- archivist::aread("pbiecek/models/27e5c")
titanic$survived <- ifelse(titanic$survived == "yes", 1, 0)
titanic_lmr <- rms::lrm(survived == "1" ~ gender + rms::rcs(age) + class + sibsp + parch + fare + embarked, titanic)
titanic_lmr_exp <- explain(model = titanic_lmr,
                           data = titanic[, -9],
                           y = titanic$survived == "1",
                           label = "Logistic Regression")

# Number of repeats performed for every benchmarked operation

repeats <- 20

# Measuring the time to create and plot a model explainer in the DALEX package
# Each explainer uses 50 permutations (B = 50)

tic()
replicate(repeats, plot(model_parts(explainer = titanic_lmr_exp, B = 50, N = NULL)))
time1 <- toc()

time1


# Creating a data set analogical to the one used in the modelling example

pred9 <- c(rnorm(1103), rnorm(1103, 1))
target9 <- c(rep(0, 1103), rep(1, 1103))

# Creating an analogical, competitive function

bigstatsr_one_minus_AUC <- function(observed, predicted) {
  return(1 - bigstatsr::AUC(predicted, observed))
}

# Ensuring the values are identical

DALEX::loss_one_minus_auc(target9, pred9)
bigstatsr_one_minus_AUC(target9, pred9)
all.equal(DALEX::loss_one_minus_auc(target9, pred9), bigstatsr_one_minus_AUC(target9, pred9))


# Benchmarking

call_case_study_benchmark <- function(pred, target, times = 100) {
  
  benchmark <- microbenchmark(
    
    bigstatsr_one_minus_AUC(target, pred),
    
    DALEX::loss_one_minus_auc(target, pred),
    
    times = times
  )
  
  levels(benchmark$expr) <- c("bigstatsr", "DALEX")
  
  benchmark$expr <- reorder(benchmark$expr, -benchmark$time, FUN = median)
  
  return(benchmark)
  
}

dalex_bigstatsr_benchmark <- call_case_study_benchmark(pred9, target9, times = repeats)
ggplot2::autoplot(dalex_bigstatsr_benchmark)
saveRDS(dalex_bigstatsr_benchmark, file = "data/dalex_bigstatsr_benchmark.rds")


# Replacing the function definition inside the DALEX package

assignInNamespace("loss_one_minus_auc", bigstatsr_one_minus_AUC, ns = "DALEX")


# Checking the time again

tic()
replicate(repeats, plot(model_parts(explainer = titanic_lmr_exp, B = 50, N = NULL)))
time2 <- toc()

time1
time2

# Time saved as a percentage
time_saved <- ((time2$toc - time2$tic) / (time1$toc - time1$tic) - 1) * 100
print(paste("Optimising AUC calculation decreased the total calculation time by ",
            round(-time_saved, 3), "%.", sep = ""))
