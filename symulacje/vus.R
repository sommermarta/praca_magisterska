
vus <- function(prawdziwe_klasy, estymacja_porzadku){
     
     stopifnot(length(prawdziwe_klasy) == length(estymacja_porzadku))
     
     o <- order(estymacja_porzadku)
     posort_dane <- prawdziwe_klasy[o]
     
     jakie <- as.numeric(names(table(posort_dane)))
     mianownik <- prod(table(posort_dane))
     
     m <- matrix(0, ncol=length(posort_dane), nrow=length(jakie))
     
     ile <- 0
     for(i in 1:ncol(m)){
          if(posort_dane[i]==jakie[1]){
               ile <- ile+1
          } 
          m[1,i] <- ile
     }
     for(i in 2:nrow(m)){
          ile <- 0
          for(j in 1:ncol(m)){
               if(posort_dane[j]==jakie[i]){
                    if(j==1) ile <- 1 else ile <- m[i, j-1] + m[i-1, j]    
               }
               m[i, j] <- ile
          }
     }
     
     vus <- m[nrow(m), ncol(m)]/mianownik
     return(vus)
     
}

# posort_dane <- c(1,2,1,3,2,3,4,1,2,3,4,5,4,5,3,4,5)

# przyklad:
# 
# y <- c(1, 2, 1, 3, 2, 3, 1, 2, 3, 3)
# f <- c(1, 2, 4, 5, 8, 11, 12, 16, 17, 20)
# 
# vus(y, f)



