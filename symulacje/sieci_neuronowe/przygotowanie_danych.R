library("stringi")
library("dplyr")

setwd("C:\\Users\\Marta\\Desktop\\Marta\\GitHub\\praca_magisterska\\symulacje\\dane\\zmienione")

# wczytanie danych

dane <- dir()
dane_tren <- dane[stri_detect_regex(dane, ".*?_train[.]txt")]
dane_test <- dane[stri_detect_regex(dane, ".*?_test[.]txt")]
dane_targets <- dane[stri_detect_regex(dane, ".*?_targets[.]txt")]

for(i in 1:length(dane_tren)){
     tren2 <- read.table(dane_tren[i], header=FALSE)
     
     tren <- tren2 
     ile_kolumn <- ncol(tren2)
     input_dimention <- ile_kolumn-1
     
     tren[,1] <- tren2[,ile_kolumn]-1
     tren[,-1] <- tren2[,-ile_kolumn]
     
     jakie <- as.numeric(names(table(tren[,1])))
     klasy <- paste(c(length(jakie), jakie), collapse=" ")
     ile <- nrow(tren)
     
     nazwa <- stri_match_all_regex(dane_tren[i], "(.*?)_train[.]txt") %>% unlist() %>% "["(2)
     gdzie <- stri_paste("C:\\Users\\Marta\\Desktop\\Marta\\GitHub\\praca_magisterska\\symulacje\\sieci_neuronowe\\dane\\",
                         nazwa, "_train.txt", sep="")
     write(input_dimention, gdzie)
     write(klasy, gdzie, append=TRUE)
     write(ile, gdzie, append=TRUE)
     write.table(tren, gdzie, append=TRUE, quote=FALSE, col.names=FALSE, row.names=FALSE)
     
     test <- read.table(dane_test[i], header=FALSE)
     targets <- read.table(dane_targets[i], header=FALSE)
     
     test <- cbind(targets-1, test)
     
     ile <- nrow(test)
     
     gdzie <- stri_paste("C:\\Users\\Marta\\Desktop\\Marta\\GitHub\\praca_magisterska\\symulacje\\sieci_neuronowe\\dane\\",
                         nazwa, "_test.txt", sep="")
     write(input_dimention, gdzie)
     write(klasy, gdzie, append=TRUE)
     write(ile, gdzie, append=TRUE)
     write.table(test, gdzie, append=TRUE, quote=FALSE, col.names=FALSE, row.names=FALSE)
     
}
















