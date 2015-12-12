library("dplyr")

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

wsp <- function(wektor){
     round(100*(posortuj(sort(wektor, decreasing=TRUE))-posortuj(wektor))/posortuj(sort(wektor, decreasing=TRUE)), 2)     
}

vus <- function(wektor){

     posort_dane <- wektor
     
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
     
     vus <- round(100*m[nrow(m), ncol(m)]/mianownik, 2)
     return(vus)
     
}

macierz <- data.frame()

wektor <- c(1, 2, 3, 4, 5, 6, 7) %>% as.integer()
macierz <- rbind(macierz, wektor)


wektor <- c(1, 1, 1, 2, 3, 4, 5)
macierz <- rbind(macierz, wektor)


wektor <- 7:1
macierz <- rbind(macierz, wektor)


wektor <- c(5, 4, 3, 2, 1, 1, 1, 1)
macierz <- rbind(macierz, wektor)


wektor <- c(1, 1, 1, 2, 3, 5, 4)
macierz <- rbind(macierz, wektor)


wektor <- c(5, 1, 1, 1, 2, 3, 4)
macierz <- rbind(macierz, wektor)


wektor <- c(5, 1, 1, 1, 1, 1, 1)
macierz <- rbind(macierz, wektor)


wektor <- c(5, 1, 1, 1, 1, 1, 6)
macierz <- rbind(macierz, wektor)


wektor <- c(5, 1, 1, 1, 1, 1, 5)
macierz <- rbind(macierz, wektor)


wektor <- c(1, 1, 1, 5, 1, 1, 1)
macierz <- rbind(macierz, wektor)


wektor <- c(5, 1, 1, 5, 1, 1, 1)
macierz <- rbind(macierz, wektor)


wektor <- c(1, 5, 2, 3, 4, 5, 4, 1)
macierz <- rbind(macierz, wektor)

# names(macierz) %>%
#      paste(collapse=", ")

apply(macierz, 1, function(x) vus(x)) -> vuss
apply(macierz, 1, function(x) wsp(x)) -> bscc

macierz <- cbind(macierz, vuss, bscc)

colnames(macierz) <- c(rep(" ", 7), "VUS", "BSC")


library("xtable")

for(i in 1:7){
     macierz[, i] <- as.integer(macierz[, i])
}

print(xtable(macierz), include.rownames=FALSE)






