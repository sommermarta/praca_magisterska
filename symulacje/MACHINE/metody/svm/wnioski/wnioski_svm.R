# biblioteki:

library("stringi")
library("dplyr")

# funkcje:

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

ppk <- function(prawdziwe_klasy, estymowane_klasy){
    100*sum(estymowane_klasy==prawdziwe_klasy)/length(estymowane_klasy)
}

abserr <- function(prawdziwe_klasy, estymowane_klasy){
    sum(abs(estymowane_klasy-as.numeric(as.character(prawdziwe_klasy))))/length(estymowane_klasy)
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

wsp <- function(wektor){
     (posortuj(sort(wektor, decreasing=TRUE))-posortuj(wektor))/posortuj(sort(wektor, decreasing=TRUE))     
}

# ladowanie danych:

setwd("C:/Users/Marta/Desktop/Marta/GitHub/praca_magisterska/symulacje/MACHINE/metody/svm/wnioski")

nazwy <- stri_match_all_regex(dir(), "(.*?)_train[.]0") %>% 
    lapply(.,function(x) x[,2]) %>%
    unlist() %>%
    na.omit() %>%
    unique()

for(i in 1:length(nazwy)){
    
    nazwa <- nazwy[i]
    
    stri_extract_all_regex(nazwa, "[0-9]+") %>% unlist() -> numer
    stri_extract_all_regex(nazwa, "[a-z]+") %>% unlist() -> rodzaj
    
    tren <- read.table(paste(nazwa, "_train.0", sep=""))
    ile_poz <- read.table(paste(nazwa, "_train.0", sep=""))[nrow(tren),ncol(tren)]
    f <- read.table(paste(nazwa, "_cguess.0.svm.conf", sep=""))[,1]
    y_est <- read.table(paste(nazwa, "_cguess.0", sep=""))[,1]
    y <- read.table(paste(nazwa, "_targets.0", sep=""))[,1]
    
    est_vus <- vus(prawdziwe_klasy=y, estymacja_porzadku=f)
    proc_poprawnosci <- ppk(y, y_est)
    abss <- abserr(y, y_est)
    babelki <- wsp_bab(y[order(f)])
    
    c(ifelse(rodzaj=="przedzialy", "rownoliczne_klastry", "metoda_k_srednich"),
      numer, est_vus, proc_poprawnosci, abss, babelki) -> linijka_do_tabeli
    
    if(i==1){
        tabela <- as.data.frame(linijka_do_tabeli) %>% t() 
    } else{
        tabela <- rbind(tabela, linijka_do_tabeli)
    }
    if(i==length(nazwy)){
        tabela <- as.data.frame(tabela)
        names(tabela) <- c("rodzaj_klastra", "ile_poziomow_zmiennej_odpowiedzi", 
                           "vus", "ppk", "abs", "sb")
        tabela$ile_poziomow_zmiennej_odpowiedzi <- as.numeric(as.character(tabela$ile_poziomow_zmiennej_odpowiedzi))
        tabela %>% 
            arrange(rodzaj_klastra, ile_poziomow_zmiennej_odpowiedzi) -> tabela
    }
}

write.table(tabela, "..\\..\\..\\wnioski\\svm.txt", col.names=TRUE, 
            row.names=FALSE, quote=FALSE, sep="\t")



