# Jeden zbiór danych 

default <- c(rep(1, 5000), rep(0, 5000))

score_1 <- c(rnorm(5000), rnorm(5000, 1))

bigstatsr::AUC(-score_1, default)



# Jak zaokrąglimy, zmieni się performance:

# score_1 <- round(score_1)



# Obliczenia

library(microbenchmark)

mybenchmark <- (microbenchmark(
  
  ROCR::performance(ROCR::prediction(-score_1, default), "auc")@y.values[[1]],
  
  pROC::auc(default, score_1, lev=c('0', '1'), dir=">"),
  
  mltools::auc_roc(-score_1, default),
  
  (Hmisc::somers2(-score_1, default)['Dxy']+1)/2,
  
  bigstatsr::AUC(-score_1, default),
  
  AUC_in_tidyverse(-score_1, default)
  
))


# AUC   = (Gini+1)/2 


head(score_1)

head(round(score_1))

table(round(score_1))

hist(score_1, breaks=1000)



# Wykres 

mybenchmark2 <- mybenchmark

levels(mybenchmark2$expr) <- c("ROCR", "pROC", "mltools", "Hmisc", "bigstatsr", "tidyverse-table")

library(ggplot2)  

autoplot(mybenchmark2) +  
  theme(axis.text.y=element_text(size=rel(1.5)))
