library("dplyr")
library("stringi")

kara10 <- c(0, 0.1, 1, 2, 5, 10, 10, 10, 10)
kara5 <- c(0, 0.1, 3, 10, 10)

################################################################
############### GAUSSIAN PROCESSES #############################
################################################################

setwd("C:\\Users\\Marta\\Desktop\\Marta\\GitHub\\praca_magisterska\\symulacje\\gaussian_processes\\zmienione0")
source("..\\..\\vus.R")

nazwy <- stri_match_all_regex(dir(), "(.*?)_train[.]0") %>% 
     lapply(.,function(x) x[,2]) %>%
     unlist() %>%
     na.omit() %>%
     unique()

for(i in 1:length(nazwy)){
     
     nazwa <- nazwy[i]
     
     tren <- read.table(paste(nazwa, "_train.0", sep=""))
     ile_poz <- tren[nrow(tren),ncol(tren)]
     f <- read.table(paste(nazwa, "_test.0.func", sep=""))[,1]
     y_est <- read.table(paste(nazwa, "_test.0.guess", sep=""))[,1]
     y <- read.table(paste(nazwa, "_targets.0", sep=""))[,1]
     
     est_vus <- vus(prawdziwe_klasy=y, estymacja_porzadku=f)
     proc_poprawnosci <- sum(y_est == y)*100/length(y)
     
     data.frame(y=y, y_est=y_est) %>%
          mutate(czy_zgodne = ifelse(y==y_est, 1, 0)) %>%
          group_by(y) %>%
          summarise(ile_zgodnych = sum(czy_zgodne==1),
                    ile_y_w_grupie = n(),
                    procent_zgodnych_w_grupach = 100*sum(czy_zgodne==1)/n()) -> dane
     
     ile <- length(y)
     
     if(ile_poz==10) kara <- kara10 else kara <- kara5
     tabela <- abs(y-y_est)
     #moj_wsk <- sum(tabela*kara[as.numeric(names(tabela))+1])/(sum(!(y-y_est==0)))
     moj_wsk <- sum(tabela)/length(y)
     
     gdzie <- paste("..\\podsumowanie\\", nazwa, "_gpor_podsumowanie.txt", sep="") 
     
     write(ile_poz, gdzie)
     write(ile, gdzie, append=TRUE)
     write(est_vus, gdzie, append=TRUE)
     write(moj_wsk, gdzie, append=TRUE)
     write(proc_poprawnosci, gdzie, append=TRUE)
     write.table(dane, gdzie, col.names=TRUE, row.names=FALSE, quote=FALSE, append=TRUE)
     
}

# ile poziomow zmiennej odpowiedzi
# ile obserwacji w zbiorze testowym
# vus
# jaki procent poprawnosci
# tabela procentu poprawnosci w poszczegolnych grupach

# jak to potem odczytac:
# read.table(gdzie, skip=3, header=T)

################################################################
######################### SVM ##################################
################################################################

setwd("C:\\Users\\Marta\\Desktop\\Marta\\GitHub\\praca_magisterska\\symulacje\\svm\\svorim\\zmienione0")

nazwy <- stri_match_all_regex(dir(), "(.*?)_train[.]0") %>% 
     lapply(.,function(x) x[,2]) %>%
     unlist() %>%
     na.omit() %>%
     unique()

for(i in 1:length(nazwy)){

     nazwa <- nazwy[i]
     
     tren <- read.table(paste(nazwa, "_train.0", sep=""))
     ile_poz <- read.table(paste(nazwa, "_train.0", sep=""))[nrow(tren),ncol(tren)]
     f <- read.table(paste(nazwa, "_cguess.0.svm.conf", sep=""))[,1]
     y_est <- read.table(paste(nazwa, "_cguess.0", sep=""))[,1]
     y <- read.table(paste(nazwa, "_targets.0", sep=""))[,1]
     
     est_vus <- vus(prawdziwe_klasy=y, estymacja_porzadku=f)
     proc_poprawnosci <- sum(y_est == y)*100/length(y)
     
     data.frame(y=y, y_est=y_est) %>%
          mutate(czy_zgodne = ifelse(y==y_est, 1, 0)) %>%
          group_by(y) %>%
          summarise(ile_zgodnych = sum(czy_zgodne==1),
                    ile_y_w_grupie = n(),
                    procent_zgodnych_w_grupach = 100*sum(czy_zgodne==1)/n()) -> dane
     
     ile <- length(y)
     
     if(ile_poz==10) kara <- kara10 else kara <- kara5
     tabela <- abs(y-y_est)
     #moj_wsk <- sum(tabela*kara[as.numeric(names(tabela))+1])/(sum(!(y-y_est==0)))
     moj_wsk <- sum(tabela)/length(y)
     
     gdzie <- paste("..\\..\\podsumowanie\\", nazwa, "_svm_podsumowanie.txt", sep="") 
     
     write(ile_poz, gdzie)
     write(ile, gdzie, append=TRUE)
     write(est_vus, gdzie, append=TRUE)
     write(moj_wsk, gdzie, append=TRUE)
     write(proc_poprawnosci, gdzie, append=TRUE)
     write.table(dane, gdzie, col.names=TRUE, row.names=FALSE, quote=FALSE, append=TRUE)
     
}

################################################################
################### NEURAL NETWORK #############################
################################################################

setwd("C:\\Users\\Marta\\Desktop\\Marta\\GitHub\\praca_magisterska\\symulacje\\sieci_neuronowe\\wyniki")

nazwy <- stri_match_all_regex(dir(), "(.*?)_train[.]txt") %>% 
     lapply(.,function(x) x[,2]) %>%
     unlist() %>%
     na.omit() %>%
     unique()

for(i in 1:length(nazwy)){
     
     nazwa <- nazwy[i]
     
     ile_poz <- readLines(paste(nazwa, "_train.txt", sep=""), n=2)[2] %>% 
          stri_extract_all_regex(., "(\\p{Nd}+)") %>% unlist() %>% as.numeric() %>% "["(1)
     
     klasa <- readLines(paste(nazwa, ".out", sep=""))
     
     klasa %>%
          stri_match_all_regex(., "(\\p{Nd}+)[:]") %>%
          lapply(., function(x) x[,2]) %>% 
          unlist() %>%
          na.omit() %>%
          as.numeric() %>%
          "+"(.,1) -> y_est
     
     y <- read.table(paste("C:\\Users\\Marta\\Desktop\\Marta\\GitHub\\praca_magisterska\\symulacje\\svm\\svorim\\zmienione0\\", nazwa, "_targets.0", sep=""))[,1]
     
     proc_poprawnosci <- sum(y_est == y)*100/length(y)
     
     data.frame(y=y, y_est=y_est) %>%
          mutate(czy_zgodne = ifelse(y==y_est, 1, 0)) %>%
          group_by(y) %>%
          summarise(ile_zgodnych = sum(czy_zgodne==1),
                    ile_y_w_grupie = n(),
                    procent_zgodnych_w_grupach = 100*sum(czy_zgodne==1)/n()) -> dane
     
     ile <- length(y)
     
     if(ile_poz==10) kara <- kara10 else kara <- kara5
     tabela <- abs(y-y_est)
     #moj_wsk <- sum(tabela*kara[as.numeric(names(tabela))+1])/(sum(!(y-y_est==0)))
     moj_wsk <- sum(tabela)/length(y)
     
     gdzie <- paste("..\\podsumowanie\\", nazwa, "_nn_podsumowanie.txt", sep="") 
     
     write(ile_poz, gdzie)
     write(ile, gdzie, append=TRUE)
     write(NA, gdzie, append=TRUE)
     write(moj_wsk, gdzie, append=TRUE)
     write(proc_poprawnosci, gdzie, append=TRUE)
     write.table(dane, gdzie, col.names=TRUE, row.names=FALSE, quote=FALSE, append=TRUE)
     
}

######################################## pom ########################################

gdzie <- "C:\\Users\\Marta\\Desktop\\Marta\\GitHub\\praca_magisterska\\symulacje\\proportional_odds_model\\model_pom"
load(gdzie)
source("..\\..\\vus.R")

setwd("C:\\Users\\Marta\\Desktop\\Marta\\GitHub\\praca_magisterska\\symulacje\\proportional_odds_model\\podsumowanie")

for(i in 1:length(nazwy)){
     
     nazwa <- nazwy[i]
     
     tren <- read.table(paste("..\\..\\dane\\zmienione0\\", nazwa, "_train.0", sep=""))
     ile_poz <- tren[nrow(tren),ncol(tren)]
     f <- pred_f[[i]]
     y_est <- pred_class[[i]]
     y <- praw_class[[i]][,1]
     
     est_vus <- vus(prawdziwe_klasy=y, estymacja_porzadku=f)
     proc_poprawnosci <- sum(y_est == y)*100/length(y)
     
     data.frame(y=y, y_est=y_est) %>%
          mutate(czy_zgodne = ifelse(y==y_est, 1, 0)) %>%
          group_by(y) %>%
          summarise(ile_zgodnych = sum(czy_zgodne==1),
                    ile_y_w_grupie = n(),
                    procent_zgodnych_w_grupach = 100*sum(czy_zgodne==1)/n()) -> dane
     
     ile <- length(y)
     
     if(ile_poz==10) kara <- kara10 else kara <- kara5
     tabela <- abs(y-y_est)
     #moj_wsk <- sum(tabela*kara[as.numeric(names(tabela))+1])/(sum(!(y-y_est==0)))
     moj_wsk <- sum(tabela)/length(y)
     
     gdzie <- paste(".\\", nazwa, "_pom_podsumowanie.txt", sep="") 
     
     write(ile_poz, gdzie)
     write(ile, gdzie, append=TRUE)
     write(est_vus, gdzie, append=TRUE)
     write(moj_wsk, gdzie, append=TRUE)
     write(proc_poprawnosci, gdzie, append=TRUE)
     write.table(dane, gdzie, col.names=TRUE, row.names=FALSE, quote=FALSE, append=TRUE)
     
}

######################## simple approach ###########################################

gdzie <- "C:\\Users\\Marta\\Desktop\\Marta\\GitHub\\praca_magisterska\\symulacje\\simple_approach\\model_pom"
load(gdzie)
source("..\\..\\vus.R")

for(i in 1:length(nazwy)){
     
     nazwa <- nazwy[[i]]
     
     tren <- read.table(paste("..\\..\\dane\\zmienione0\\", nazwa, "_train.0", sep=""))
     ile_poz <- tren[nrow(tren),ncol(tren)]
     y <- prawdziwy_y[[i]]
     y_est <- klasa[[i]][,1]
     f <- ff[[i]]
     est_vus <- vus(prawdziwe_klasy=y, estymacja_porzadku=f)
     proc_poprawnosci <- sum(y_est == y)*100/length(y)
     
     data.frame(y=y, y_est=y_est) %>%
          mutate(czy_zgodne = ifelse(y==y_est, 1, 0)) %>%
          group_by(y) %>%
          summarise(ile_zgodnych = sum(czy_zgodne==1),
                    ile_y_w_grupie = n(),
                    procent_zgodnych_w_grupach = 100*sum(czy_zgodne==1)/n()) -> dane
     
     ile <- length(y)
     
     if(ile_poz==10) kara <- kara10 else kara <- kara5
     tabela <- abs(y-y_est)
     #moj_wsk <- sum(tabela*kara[as.numeric(names(tabela))+1])/(sum(!(y-y_est==0)))
     moj_wsk <- sum(tabela)/length(y)
     
     gdzie <- paste("..\\..\\simple_approach\\podsumowanie\\", nazwa, "_sa_podsumowanie.txt", sep="") 
     
     write(ile_poz, gdzie)
     write(ile, gdzie, append=TRUE)
     write(est_vus, gdzie, append=TRUE)
     write(moj_wsk, gdzie, append=TRUE)
     write(proc_poprawnosci, gdzie, append=TRUE)
     write.table(dane, gdzie, col.names=TRUE, row.names=FALSE, quote=FALSE, append=TRUE)
     
}



