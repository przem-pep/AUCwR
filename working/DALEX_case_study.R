model_performance_auc <- function(predicted, observed) {
  tpr_tmp <- tapply(observed, predicted, sum)
  TPR <- c(0,cumsum(rev(tpr_tmp)))/sum(observed)
  fpr_tmp <- tapply(1 - observed, predicted, sum)
  FPR <- c(0,cumsum(rev(fpr_tmp)))/sum(1 - observed)
  
  auc <- sum(diff(FPR)*(TPR[-1] + TPR[-length(TPR)])/2)
  auc
}

library(bigstatsr)
library(tictoc)
nsim=1e3
tic()
x <- replicate(nsim, model_performance_auc(rnorm(1000), rbinom(1000,1,.2)))
toc1<-toc()
tic()
y <- replicate(nsim, AUC(rnorm(1000), rbinom(1000,1,.2)))
toc2<-toc()
(toc1$toc-toc1$tic)/(toc2$toc-toc2$tic)
