#XAi explainable AI
# Ten wykres nie działa za szybko
library(DALEX)
titanic<-archivist::aread("pbiecek/models/27e5c")
titanic$survived <- ifelse(titanic$survived == "yes", 1, 0)
titanic_lmr <- rms::lrm(survived == "1" ~ gender + rms::rcs(age) + class + sibsp + parch + fare + embarked, titanic)
titanic_lmr_exp <- explain(model = titanic_lmr,
                           data = titanic[, -9],
                           y = titanic$survived == "1",
                           label = "Logistic Regression")
vip_lm <- model_parts(explainer = titanic_lmr_exp, B = 50, N = NULL)
plot(vip_lm)


#Taka funkcja jest "bebechach" pakietu DALEX
model_performance_auc <- function(predicted, observed) {
  tpr_tmp <- tapply(observed, predicted, sum)
  TPR <- c(0,cumsum(rev(tpr_tmp)))/sum(observed)
  fpr_tmp <- tapply(1 - observed, predicted, sum)
  FPR <- c(0,cumsum(rev(fpr_tmp)))/sum(1 - observed)
  
  auc <- sum(diff(FPR)*(TPR[-1] + TPR[-length(TPR)])/2)
  auc
}

library(Hmisc)
#somers2(-pred, target)[1]

# Funkcja własna oparta na rangach
UNN <- function(x,y){
  y1 = (y==1)
  n1 = sum(y1) 
  (mean(rank(x)[y1]) - (n1 + 1)/2)/(length(y) - n1)}


# sprawdzenie czy to samo
pred <- c(1, 1, 2, 3, 4, 4, 5, 5, 6, 7)
target <- c(1, 1, 1, 1, 0, 1, 0, 0, 1, 0)
model_performance_auc(-pred, target)
bigstatsr::AUC(-pred, target)
Hmisc::somers2(-pred, target)[1]
UNN(-pred, target)

library(bigstatsr)
library(tictoc)
nsim=1e3

#Taka funkcja jest "bebechach" pakietu DALEX
tic()
x <- replicate(nsim, model_performance_auc(rnorm(1000), rbinom(1000,1,.2)))
toc1<-toc()
#a mogłaby być taka (bigstatsr::AUC)
tic()
y <- replicate(nsim, AUC(rnorm(1000), rbinom(1000,1,.2)))
toc2<-toc()
# albo taka (Hmisc::somers2)
tic()
y <- replicate(nsim, somers2(rnorm(1000), rbinom(1000,1,.2))[1])
toc3<-toc()
# albo taka (własna oparta na Hmisc)
tic()
y <- replicate(nsim, UNN(rnorm(1000), rbinom(1000,1,.2)))
toc4<-toc()

print(c(DALEX = (toc1$toc-toc1$tic), 
        bigstatsr = (toc2$toc-toc2$tic),
        Hmisc = (toc3$toc-toc3$tic),
        UNN = (toc4$toc-toc4$tic)))

#przyśpieszenie - krotność (ile razy szybciej)
(toc1$toc-toc1$tic)/(toc4$toc-toc4$tic)
