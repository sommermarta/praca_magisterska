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

wsp_bab <- function(wektor){
     (posortuj(sort(wektor, decreasing=TRUE))-posortuj(wektor))/posortuj(sort(wektor, decreasing=TRUE))     
}

# ladowanie danych:

setwd("C:/Users/Marta/Desktop/Marta/GitHub/praca_magisterska/symulacje/MACHINE/metody/sieci_neuronowe/wyniki")

nazwy <- stri_match_all_regex(dir("..\\..\\gaussian_process\\"), "(.*?)_train[.]0") %>% 
    lapply(.,function(x) x[,2]) %>%
    unlist() %>%
    na.omit() %>%
    unique()

for(i in 1:length(nazwy)){
    
    nazwa <- nazwy[i]
    
    stri_extract_all_regex(nazwa, "[0-9]+") %>% unlist() -> numer
    stri_extract_all_regex(nazwa, "[a-z]+") %>% unlist() -> rodzaj
    
    tren <- read.table(paste("..\\..\\gaussian_process\\", nazwa, "_train.0", sep=""))
    ile_poz <- tren[nrow(tren),ncol(tren)]
    
    klasa <- readLines(paste(nazwa, ".txt", sep=""))
    
    klasa %>%
        stri_match_all_regex(., "(\\p{Nd}+)[:]") %>%
        lapply(., function(x) x[,2]) %>% 
        unlist() %>%
        na.omit() %>%
        as.numeric() %>%
        "+"(.,1) -> y_est
    
    y <- read.table(paste("..\\..\\gaussian_process\\", nazwa, "_targets.0", sep=""))[,1]
                   
    est_vus <- NA
    proc_poprawnosci <- ppk(y, y_est)
    abss <- abserr(y, y_est)
    babelki <- NA
    
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

write.table(tabela, "..\\..\\..\\wnioski\\nnrank.txt", col.names=TRUE, 
            row.names=FALSE, quote=FALSE, sep="\t")


