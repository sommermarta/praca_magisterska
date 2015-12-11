# http://www.dcc.fc.up.pt/~ltorgo/Regression/DataSets.html

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
    
    vus <- 100*m[nrow(m), ncol(m)]/mianownik
    return(vus)
    
}

library("MASS")
library("stringi")
library("rms")
library("dplyr")
library("arules")

# machine <- read.table("C:\\Users\\marsom\\Desktop\\machine.txt",
#                       sep=",")
# names(machine)[ncol(machine)] <- "y"
# 
# lista_rownoliczne <- vector("list", 4)
# lista_przedzialy <- vector("list", 4)
# k <- 1
# for(i in c(3, 5, 7, 10)){
#     d <- discretize(machine$y, categories=i, labels=1:i, method="frequency")
#     dd <- discretize(machine$y, categories=i, labels=1:i, method="cluster")
#     m <- machine
#     m$y <- d
#     lista_rownoliczne[[k]] <- m
#     m$y <- dd
#     lista_przedzialy[[k]] <- m
#     
#     k <- k+1
# }
# 
# save(lista_rownoliczne, lista_przedzialy, s, 
#      file="C:\\Users\\marsom\\Desktop\\lista_danych_machine.RData")

setwd("C:/Users/Marta/Desktop/Marta/GitHub/praca_magisterska/symulacje/MACHINE/metody")

load("..\\lista_danych_machine.RData")

# zbior testowy i treningowy

# set.seed(1234567)
# s <- sample(1:nrow(machine), floor(0.8*nrow(machine)))

# rownoliczne

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

wsp_bab <- function(wektor){
     (posortuj(sort(wektor, decreasing=TRUE))-posortuj(wektor))/posortuj(sort(wektor, decreasing=TRUE))     
}

tabela_rownoliczne <- matrix(ncol=4, nrow=4)
for(i in 1:length(lista_rownoliczne)){
    m <- lrm(y~., data=lista_rownoliczne[[i]][s, ])
    m_pred_prob <- predict(m, lista_rownoliczne[[i]][-s, ], type="fitted.ind")
    m_pred_class <- max.col(m_pred_prob)
    m_pred_f <- predict(m, lista_rownoliczne[[i]][-s, ], type="lp")
    m_praw_class <- lista_rownoliczne[[i]][-s, ]$y   
    
    est_vus <- vus(prawdziwe_klasy=m_praw_class, estymacja_porzadku=m_pred_f)
    proc_poprawnosci <- sum(m_pred_class == m_praw_class)*100/length(m_praw_class)
    abss <- sum(abs(as.numeric(as.character(m_praw_class))-m_pred_class))/length(m_pred_class)
    babelki <- wsp_bab(as.numeric(m_praw_class[order(m_pred_f)]))

    tabela_rownoliczne[i, ] <- c(est_vus, proc_poprawnosci, abss, babelki)  
}

tabela_rownoliczne <- as.data.frame(tabela_rownoliczne)
colnames(tabela_rownoliczne) <- c("vus", "ppk", "abs", "sb")
rownames(tabela_rownoliczne) <- c("3", "5", "7", "10")

# przedzialy

tabela_przedzialy <- matrix(ncol=4, nrow=4)
for(i in 1:length(lista_przedzialy)){
    m <- lrm(as.factor(y)~., data=lista_przedzialy[[i]][s,])
    m_pred_prob <- predict(m, lista_przedzialy[[i]][-s, ], type="fitted.ind")
    m_pred_class <- max.col(m_pred_prob)
    m_pred_f <- predict(m, lista_przedzialy[[i]][-s, ], type="lp")
    m_praw_class <- lista_przedzialy[[i]][-s, ]$y   
    
    est_vus <- vus(prawdziwe_klasy=m_praw_class, estymacja_porzadku=m_pred_f)
    proc_poprawnosci <- sum(m_pred_class == m_praw_class)*100/length(m_praw_class)
    abss <- sum(abs(as.numeric(as.character(m_praw_class))-m_pred_class))/length(m_pred_class)
    babelki <- wsp_bab(as.numeric(m_praw_class[order(m_pred_f)]))
    
    tabela_przedzialy[i, ] <- c(est_vus, proc_poprawnosci, abss, babelki)  
}

tabela_przedzialy <- as.data.frame(tabela_przedzialy)
colnames(tabela_przedzialy) <- c("vus", "ppk", "abs", "sb")
rownames(tabela_przedzialy) <- c("3", "5", "7", "10")

cbind(rodzaj_klastra="metoda_k_srednich", 
      ile_poziomow_zmiennej_odpowiedzi=c(3, 5, 7, 10),
      tabela_przedzialy) -> jeden

cbind(rodzaj_klastra="rownoliczne_klastry", 
      ile_poziomow_zmiennej_odpowiedzi=c(3, 5, 7, 10),
      tabela_rownoliczne) -> dwa

rbind(jeden, dwa) -> pom

write.table(pom, "..\\wnioski\\pom.txt", col.names=TRUE, 
            row.names=FALSE, quote=FALSE, sep="\t")








