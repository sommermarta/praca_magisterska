library('nnet')
library("dplyr")

ppk <- function(estymowana, prawdziwa){
     round(sum(estymowana==prawdziwa)*100/length(prawdziwa), 2)
}
vus <- function(prawdziwe_klasy, estymacja_porzadku){
     
     stopifnot(length(prawdziwe_klasy) == length(estymacja_porzadku))
     
     o <- order(estymacja_porzadku)
     posort_dane <- prawdziwe_klasy[o]
     
     jakie <- as.numeric(names(table(posort_dane)))
     mianownik <- prod(table(posort_dane))
     
     m <- matrix(0, ncol=length(posort_dane), nrow=length(jakie))
     
     for(i in 1:ncol(m)){
          if(posort_dane[i]==jakie[1]){
               if(i==1) m[1, i] <- 1 else m[1,i] <- m[1, i-1] + 1
          } else {
               if(i==1) m[1, i] <- 0 else m[1, i] <- m[1, i-1]
          }
     }
     for(i in 2:nrow(m)){
          for(j in 1:ncol(m)){
               if(posort_dane[j]==jakie[i]){
                    if(j==1) ile <- 1 else m[i, j] <- m[i, j-1] + m[i-1, j-1]    
               } else{
                    if(j==1) m[i, j] <- 0 else m[i, j] <- m[i, j-1] 
               }
               
          }
     }
     
     vus <- m[nrow(m), ncol(m)]/mianownik
     return(vus)
     
}

posortuj <- function(wektor){
     
     ile_zamian <- 0
     for(j in length(wektor):2){
          for(i in 1:(j-1)){
               if(wektor[i]>wektor[i+1]){
                    pamietaj <- wektor[i]
                    wektor[i] <- wektor[i+1]
                    wektor[i+1] <- pamietaj
                    ile_zamian <- ile_zamian + 1
               }     
          }     
     }
     
     ile_zamian
}

bab <- function(wektor){
     round(100*(posortuj(sort(wektor, decreasing=TRUE))-posortuj(wektor))/posortuj(sort(wektor, decreasing=TRUE)), 2)     
}

abserr <- function(estymacja, prawdziwa){
     round(sum(abs(estymacja-prawdziwa))/length(prawdziwa), 2)
}

setwd("C:\\Users\\Marta\\Desktop\\Marta\\GitHub\\praca_magisterska\\symulacje\\dane\\zmienione0")

dir() -> pliki

# abalone

read.table(pliki[3]) -> tren
read.table(pliki[2]) -> test
read.table(pliki[1])[, 1] -> test_klas

model <- multinom(V11~., data=tren)
predict(model, test) %>% as.numeric() -> est
apply(predict(model, test, "prob"), 1, function(x) max(x)) -> f
vus(test_klas, f)
ppk(est, test_klas)
abserr(est, test_klas)
bab(test_klas[order(f)])

# auto

read.table(pliki[6]) -> tren
read.table(pliki[5]) -> test
read.table(pliki[4])[, 1] -> test_klas

model <- multinom(V8~., data=tren)
predict(model, test) %>% as.numeric() -> est
apply(predict(model, test, "prob"), 1, function(x) max(x)) -> f
vus(test_klas, f)
ppk(est, test_klas)
abserr(est, test_klas)
bab(test_klas[order(f)])

# diabetes

read.table(pliki[9]) -> tren
read.table(pliki[8]) -> test
read.table(pliki[7])[, 1] -> test_klas

model <- multinom(V3~., data=tren)
predict(model, test) %>% as.numeric() -> est
apply(predict(model, test, "prob"), 1, function(x) max(x)) -> f
vus(test_klas, f)
ppk(est, test_klas)
abserr(est, test_klas)
bab(test_klas[order(f)])


# housing

read.table(pliki[12]) -> tren
read.table(pliki[11]) -> test
read.table(pliki[10])[, 1] -> test_klas

model <- multinom(V14~., data=tren)
predict(model, test) %>% as.numeric() -> est
apply(predict(model, test, "prob"), 1, function(x) max(x)) -> f
vus(test_klas, f)
ppk(est, test_klas)
abserr(est, test_klas)
bab(test_klas[order(f)])

# machine

read.table(pliki[15]) -> tren
read.table(pliki[14]) -> test
read.table(pliki[13])[, 1] -> test_klas

model <- multinom(V7~., data=tren)
predict(model, test) %>% as.numeric() -> est
apply(predict(model, test, "prob"), 1, function(x) max(x)) -> f
vus(test_klas, f)
ppk(est, test_klas)
abserr(est, test_klas)
bab(test_klas[order(f)])

# pyrim

read.table(pliki[18]) -> tren
read.table(pliki[17]) -> test
read.table(pliki[16])[, 1] -> test_klas

model <- multinom(V28~., data=tren)
predict(model, test) %>% as.numeric() -> est
apply(predict(model, test, "prob"), 1, function(x) max(x)) -> f
vus(test_klas, f)
ppk(est, test_klas)
abserr(est, test_klas)
bab(test_klas[order(f)])

# stock

read.table(pliki[21]) -> tren
read.table(pliki[20]) -> test
read.table(pliki[19])[, 1] -> test_klas

model <- multinom(V10~., data=tren)
predict(model, test) %>% as.numeric() -> est
apply(predict(model, test, "prob"), 1, function(x) max(x)) -> f
vus(test_klas, f)
ppk(est, test_klas)
abserr(est, test_klas)
bab(test_klas[order(f)])

# triazines

read.table(pliki[24]) -> tren
read.table(pliki[23]) -> test
read.table(pliki[22])[, 1] -> test_klas

model <- multinom(V61~., data=tren)
predict(model, test) %>% as.numeric() -> est
apply(predict(model, test, "prob"), 1, function(x) max(x)) -> f
vus(test_klas, f)
ppk(est, test_klas)
abserr(est, test_klas)
bab(test_klas[order(f)])


# wpbc

read.table(pliki[27]) -> tren
read.table(pliki[26]) -> test
read.table(pliki[25])[, 1] -> test_klas

model <- multinom(V33~., data=tren)
predict(model, test) %>% as.numeric() -> est
apply(predict(model, test, "prob"), 1, function(x) max(x)) -> f
vus(test_klas, f)
ppk(est, test_klas)
abserr(est, test_klas)
bab(test_klas[order(f)])

