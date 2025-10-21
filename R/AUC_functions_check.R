## Działanie funkcji - demonstracja

pred <- c(1, 1, 2, 3, 4, 4, 5, 5, 6, 7)
target <- c(1, 1, 1, 1, 0, 1, 0, 0, 1, 0)
caTools::colAUC(-pred, target, alg="Wilcoxon")

## Standaryzacja do sprawdzania

fun <- function(p, t){caTools::colAUC(-p, t)[1, 1]}  # sprawdzana funkcja
#fun <- function(p, t){bigstatsr::AUC(-p, t)}
aucuf <- function(f, p, t){f(p,t)}  # standaryzowana formuła obliczania AUC do checków
aucuf(fun, pred, target)  # test standaryzowanej foruły

## Sprawdzenia

# CHECK1 should be 0.8541667
aucuf(fun, c(1, 1, 2, 3, 4, 4, 5, 5, 6, 7), c(1, 1, 1, 1, 0, 1, 0, 0, 1, 0)) 
# CHECK2 should be 0.1458333
aucuf(fun, c(1, 1, 2, 3, 4, 4, 5, 5, 6, 7), c(0, 0, 0, 0, 1, 0, 1, 1, 0, 1)) 
# CHECK3 ideal model <; should be 0
aucuf(fun, c(1, 1, 2, 3, 4, 4, 5, 5, 6, 7), c(0, 0, 1, 1, 1, 1, 1, 1, 1, 1)) 
# CHECK4 ideal model >; should be 1
aucuf(fun, c(1, 1, 2, 3, 4, 4, 5, 5, 6, 7), c(1, 1, 1, 1, 1, 1, 0, 0, 0, 0)) 
# CHECK5 non-binary target; should be error
aucuf(fun, c(1, 1, 2, 3, 4, 4, 5, 5, 6, 7), c(2, 2, 1, 1, 0, 1, 0, 0, 1, 0)) 
# CHECK6 differing lengths; should be error
aucuf(fun, c(1, 1, 2, 3, 4, 4, 5, 5, 6, 7), c(1, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0)) 
# CHECK7 non-numeric pred; should be error
aucuf(fun, as.character(c(1, 1, 2, 3, 4, 4, 5, 5, 6, 7)), c(1, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0)) 
# CHECK8 logical pred, can be error or can treat TRUE like 1
aucuf(fun, c(1, 1, 2, 3, 4, 4, 5, 5, 6, 7), c(1, 1, 1, 1, 0, 1, 0, 0, 1, 0)==1) 



