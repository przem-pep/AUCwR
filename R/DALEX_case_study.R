library(DALEX)
library(tictoc)
library(microbenchmark)
library(ggplot2)
library(dplyr)

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

calculation_times_before <- numeric(repeats)

for(i in 1:repeats) {
  tic()
  vip_lm <- model_parts(explainer = titanic_lmr_exp, B = 50, N = NULL)
  plot(vip_lm)
  time1 <- toc()
  calculation_time <- unname(time1$toc - time1$tic)
  calculation_times_before[i] <- calculation_time
}

median_before <- median(calculation_times_before)


# Creating a data set with a similar number of observations to the one used in the modelling example

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

calculation_times_after <- numeric(repeats)

for(i in 1:repeats) {
  tic()
  vip_lm <- model_parts(explainer = titanic_lmr_exp, B = 50, N = NULL)
  plot(vip_lm)
  time2 <- toc()
  calculation_time <- unname(time2$toc - time2$tic)
  calculation_times_after[i] <- calculation_time
}

median_after <- median(calculation_times_after)

# Setting up data for plotting

optimization_results_data <- data.frame(
  labels = factor(
    x = c("Before optimization", "After optimization"),
    levels = c("Before optimization", "After optimization")
  ),
  median_time = c(median_before, median_after)
)

saveRDS(optimization_results_data, "data/optimization_results_data.rds")

# Optimization result plot

ggplot(optimization_results_data, aes(x = labels, y = median_time)) +
  geom_col(fill = "firebrick") +
  labs(x = NULL,
       y = "Median time [s]",
       title = "Median time of creating a Permutational Variable Importance Plot") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.y = element_text(size = 14, color = "black"),
    axis.text = element_text(size = 14, color = "black")
  )

test_data <- data.frame(
  before = calculation_times_before,
  after = calculation_times_after
)

test_data <- tidyr::pivot_longer(test_data, cols = everything())

ggplot(test_data, aes(name, value)) +
  geom_boxplot(fill = "firebrick") +
  scale_y_continuous(limits = c(0, max(calculation_times_before))) +
  labs(x = NULL,
       y = "Time [s]",
       title = "Box plot of time of creating a Permutational Variable Importance Plot") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.y = element_text(size = 14, color = "black"),
    axis.text = element_text(size = 14, color = "black")
  )

# Time saved as a percentage
time_saved <- ((time2$toc - time2$tic) / (time1$toc - time1$tic) - 1) * 100
print(paste("Optimising AUC calculation decreased the total calculation time by ",
            round(-time_saved, 3), "%.", sep = ""))
