library("stringi")
library("dplyr")

# wczytanie danych

load("..\\lista_danych_machine.RData")

# przedzialy

dane_tren <- lapply(lista_przedzialy, function(x) x[s, ])
dane_test <- lapply(lista_przedzialy, function(x) x[-s, -ncol(x)])
dane_targets <- lapply(lista_przedzialy, function(x) x$y[-s])

k <- c(3, 5, 7, 10)
for(i in 1:length(dane_tren)){
    tren2 <- dane_tren[[i]] %>% arrange(y)
    
    tren <- tren2 
    ile_kolumn <- ncol(tren2)
    input_dimention <- ile_kolumn-1
    
    tren[,1] <- as.numeric(as.character(tren2[,ile_kolumn]))-1
    tren[,-1] <- tren2[,-ile_kolumn]
    
    jakie <- as.numeric(names(table(tren[,1])))
    klasy <- paste(c(length(jakie), jakie), collapse=" ")
    ile <- nrow(tren)
    
    nazwa <- paste("przedzialy", k[i], sep="")
    gdzie <- stri_paste(".\\",
                        nazwa, "_train.txt", sep="")
    write(input_dimention, gdzie)
    write(klasy, gdzie, append=TRUE)
    write(ile, gdzie, append=TRUE)
    write.table(tren, gdzie, append=TRUE, quote=FALSE, col.names=FALSE, row.names=FALSE)
    
    test <- dane_test[[i]]
    targets <- dane_targets[[i]]
    
    test <- cbind(as.numeric(as.character(targets))-1, test)
    
    ile <- nrow(test)
    
    gdzie <- stri_paste(".\\",
                        nazwa, "_test.txt", sep="")
    write(input_dimention, gdzie)
    write(klasy, gdzie, append=TRUE)
    write(ile, gdzie, append=TRUE)
    write.table(test, gdzie, append=TRUE, quote=FALSE, col.names=FALSE, row.names=FALSE)
    
}

# rownoliczne


dane_tren <- lapply(lista_rownoliczne, function(x) x[s, ])
dane_test <- lapply(lista_rownoliczne, function(x) x[-s, -ncol(x)])
dane_targets <- lapply(lista_rownoliczne, function(x) x$y[-s])

k <- c(3, 5, 7, 10)
for(i in 1:length(dane_tren)){
    tren2 <- dane_tren[[i]] %>% arrange(y)
    
    tren <- tren2 
    ile_kolumn <- ncol(tren2)
    input_dimention <- ile_kolumn-1
    
    tren[,1] <- as.numeric(as.character(tren2[,ile_kolumn]))-1
    tren[,-1] <- tren2[,-ile_kolumn]
    
    jakie <- as.numeric(names(table(tren[,1])))
    klasy <- paste(c(length(jakie), jakie), collapse=" ")
    ile <- nrow(tren)
    
    nazwa <- paste("rownoliczne", k[i], sep="")
    gdzie <- stri_paste(".\\",
                        nazwa, "_train.txt", sep="")
    write(input_dimention, gdzie)
    write(klasy, gdzie, append=TRUE)
    write(ile, gdzie, append=TRUE)
    write.table(tren, gdzie, append=TRUE, quote=FALSE, col.names=FALSE, row.names=FALSE)
    
    test <- dane_test[[i]]
    targets <- dane_targets[[i]]
    
    test <- cbind(as.numeric(as.character(targets))-1, test)
    
    ile <- nrow(test)
    
    gdzie <- stri_paste(".\\",
                        nazwa, "_test.txt", sep="")
    write(input_dimention, gdzie)
    write(klasy, gdzie, append=TRUE)
    write(ile, gdzie, append=TRUE)
    write.table(test, gdzie, append=TRUE, quote=FALSE, col.names=FALSE, row.names=FALSE)
    
}












