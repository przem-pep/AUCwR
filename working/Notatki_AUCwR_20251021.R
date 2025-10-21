# Zoptymalizowane porównywanie parami
# AUC = (# par zgodnych + 0.5 * # remisów)/(# wszystkich par)
# pętla for w cpp w pakiecie bigstatr funkcja AUC 

# 100 0
# 100 0
# 100 1
# 100 1
# 120 0
# 120 1
# 120 1
# 120 1

n1 = 5
n0 = 3
n1*n0

9.5/15
pred = c(10,10,10,10,12,12,12,12)*10
target = c(0,0,1,1,0,1,1,1)
bigstatsr::AUC(pred, target)

somersdelta = 2*AUC-1
AUC = (somersdelta +1)/2 = sd/2 +1/2

DescTools::Cstat(pred, target)

# Niezoptymalizowane porównywanie parami ???
# AUC = (# par zgodnych + 0.5 * # remisów)/(# wszystkich par)
# pętla for w cpp w pakiecie bigstatr funkcja AUC 
DescTools::SomersDelta(pred, target)/2 + 1/2
# SomersDelta(pred, target)/2 + 1/2 powinno być wolniejsze niż 
# DescTools::Cstat(pred, target)

# Metoda sumy rang
ownranksum <- function(pred, target){
  t = target==1
  n1 = sum(t)
  (mean(rank(pred)[t]) - (n1 + 1)/2)/(length(target) - n1)}
ownranksum(pred, target)

# metoda trapezów na krzywej ROC
# ... 
owntraproc <- function(pred, target){
  o <- order(pred)
  p <- pred[o]
  t <- target[o]
  p1 <- p[t==1]
  p0 <- p[t==0]
  y <- c(0, cumsum(table(p1))/length(p1))
  x <- c(0, cumsum(table(p0))/length(p0))
  plot(x,y)
  sum(diff(x)*((1-y)[-1]+(1-y)[-length(y)])/2)
}

# metoda trapezów na krzywej CAP
# ...
owntracap <- function(pred, target){
  o <- order(pred)
  p <- pred[o]
  t <- target[o]
  ...
}

x <- c(0, .5, 1)
y <- c(0, 3/5, 1)
plot(x, y, type="b")
lines(0:10/10, 0:10/10, col="blue")
lines(c(0,5/8, 1), c(0,1,1), col="green")
underblue = 0.5
underblack = .5 *3/5/2 + (3/5+1)/2*0.5
undergreen = 5/8*1/2 + 3/8*1
CAP_AR = (underblack - underblue)/(undergreen-underblue)
CAP_AR/2+1/2

# sprawdzenie pakietu DALEX
observed = target
predicted = pred
tpr_tmp <- tapply(observed, predicted, sum)
TPR <- c(0,cumsum(rev(tpr_tmp)))/sum(observed)
fpr_tmp <- tapply(1 - observed, predicted, sum)
FPR <- c(0,cumsum(rev(fpr_tmp)))/sum(1 - observed)
auc <- sum(diff(FPR)*(TPR[-1] + TPR[-length(TPR)])/2)
