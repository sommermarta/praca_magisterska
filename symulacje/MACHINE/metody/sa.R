library("MASS")
library("stringi")
library("rms")
library("dplyr")
library("arules")
library("rpart")

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

###################

setwd("C:/Users/Marta/Desktop/Marta/GitHub/praca_magisterska/symulacje/MACHINE/metody")

load("..\\lista_danych_machine.RData")

######################

sa <- function(dane_treningowe, dane_testowe){

    ile <- length(levels(dane_treningowe$y))
    lista_zbiorow <- vector("list", ile-1)
    
    for(i in 1:(ile-1)){
        wektor <- numeric(nrow(dane_treningowe))
        tabelka <- dane_treningowe
        wektor[which(as.numeric(as.character(dane_treningowe$y))>i)] <- 1 
        tabelka$y <- as.factor(wektor)
        lista_zbiorow[[i]] <- tabelka
    }
    
    lista_modeli <- vector("list", ile-1)
    lista_klas <- vector("list", ile-1)
    lista_prawd <- vector("list", ile-1)
    lista_pstw <- vector("list", ile-1)
    
    for(j in 1:length(lista_zbiorow)){
        lista_modeli[[j]] <- rpart(y~., data=lista_zbiorow[[j]])
        lista_klas[[j]] <- predict(lista_modeli[[j]], newdata=dane_testowe, type="class")
        lista_prawd[[j]] <- predict(lista_modeli[[j]], newdata=dane_testowe, type="prob")[,2]
    }
    
    lista_pstw[[1]] <- 1 - lista_prawd[[1]] 
    lista_pstw[[ile]] <- lista_prawd[[ile-1]]
    for(i in 2:length(lista_zbiorow)){
        lista_pstw[[i]] <- lista_prawd[[i-1]] - lista_prawd[[i]]    
    }
    
    tabela <- matrix(lista_pstw[[1]])
    for(i in 2:ile){
        tabela <- cbind(tabela, lista_pstw[[i]])
    }
    
    estym_klasa <- max.col(tabela)
    praw_klasa <- dane_testowe$y
    f <- apply(tabela, 1, max)+estym_klasa
    
    ppk <- 100*sum(estym_klasa==praw_klasa)/length(estym_klasa)
    est_vus <- vus(prawdziwe_klasy=praw_klasa, estymacja_porzadku=f)
    abss <- sum(abs(estym_klasa-as.numeric(as.character(praw_klasa))))/length(estym_klasa)
    babelki <- wsp_bab(as.numeric(praw_klasa[order(f)]))
    
    c(est_vus, ppk, abss, babelki)    
    
}

######################

# dane_treningowe <- lista_przedzialy[[1]][s, ]
# dane_testowe <- lista_przedzialy[[1]][-s, ] 

tabela_rownoliczne <- matrix(ncol=4, nrow=4)
for(i in 1:length(lista_rownoliczne)){
    tabela_rownoliczne[i, ] <- sa(lista_rownoliczne[[i]][s, ], lista_rownoliczne[[i]][-s, ])  
}

tabela_rownoliczne <- as.data.frame(tabela_rownoliczne)
colnames(tabela_rownoliczne) <- c("vus", "ppk", "abs", "sa")
rownames(tabela_rownoliczne) <- c("3", "5", "7", "10")

# przedzialy

tabela_przedzialy <- matrix(ncol=4, nrow=4)
for(i in 1:length(lista_rownoliczne)){
    tabela_przedzialy[i, ] <- sa(lista_przedzialy[[i]][s, ], lista_przedzialy[[i]][-s, ])  
}

tabela_przedzialy <- as.data.frame(tabela_przedzialy)
colnames(tabela_przedzialy) <- c("vus", "ppk", "abs", "sa")
rownames(tabela_przedzialy) <- c("3", "5", "7", "10")

cbind(rodzaj_klastra="metoda_k_srednich", 
      ile_poziomow_zmiennej_odpowiedzi=c(3, 5, 7, 10),
      tabela_przedzialy) -> jeden

cbind(rodzaj_klastra="rownoliczne_klastry", 
      ile_poziomow_zmiennej_odpowiedzi=c(3, 5, 7, 10),
      tabela_rownoliczne) -> dwa

rbind(jeden, dwa) -> pom

write.table(pom, "..\\wnioski\\sa.txt", col.names=TRUE, 
            row.names=FALSE, quote=FALSE, sep="\t")





