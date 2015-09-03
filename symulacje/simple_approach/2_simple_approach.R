library("stringi")
library("dplyr")
library("rpart")

setwd("C:\\Users\\Marta\\Desktop\\Marta\\GitHub\\praca_magisterska\\symulacje\\dane\\zmienione")

# wczytanie danych

dane <- dir()
dane_tren <- dane[stri_detect_regex(dane, ".*?_train[.]txt")]
dane_test <- dane[stri_detect_regex(dane, ".*?_test[.]txt")]
dane_targets <- dane[stri_detect_regex(dane, ".*?_targets[.]txt")]

l <- length(dane_tren)
lista_tren <- vector("list", l)
lista_test <- vector("list", l)
lista_targets <- vector("list", l)

for(i in 1:l){
     lista_tren[[i]] <- read.table(dane_tren[i], header=FALSE)
     lista_test[[i]] <- read.table(dane_test[i], header=FALSE)
     lista_targets[[i]] <- read.table(dane_targets[i], header=FALSE)
}

ile_kolumn <- unlist(lapply(lista_tren, function(x) ncol(x)))
ile_zbiorow <- unlist(lapply(lista_tren, function(x) length(table(x[, ncol(x)]))-1))

# stworzenie nowych zbiorow

lista_zbiorow <- vector("list", l)

for(i in 1:l){
     lista_zbiorow[[i]] <- vector("list", ile_zbiorow[i])
}

for(j in 1:l){
     for(i in 1:ile_zbiorow[j]){
          wektor <- numeric(nrow(lista_tren[[j]]))
          tabelka <- lista_tren[[j]]
          wektor[which(lista_tren[[j]][, ile_kolumn[j]]>i)] <- 1 
          tabelka[, ile_kolumn[j]] <- as.factor(wektor)
          lista_zbiorow[[j]][[i]] <- tabelka
     }     
}

# dopasowanie modeli

lista_modeli <- vector("list", l)
lista_klas <- vector("list", l)
lista_prawd <- vector("list", l)
lista_pstw <- vector("list", l) 

for(i in 1:l){
     lista_modeli[[i]] <- vector("list", ile_zbiorow[i])
     lista_klas[[i]] <- vector("list", ile_zbiorow[i])
     lista_prawd[[i]] <- vector("list", ile_zbiorow[i])
     lista_pstw[[i]] <- vector("list", ile_zbiorow[i]+1)
}

for(i in 1:l){
     dane <- lista_zbiorow[[i]]
     testowy <- lista_test[[i]]
     for(j in 1:ile_zbiorow[i]){
          lista_modeli[[i]][[j]] <- rpart(as.formula(paste("V", ncol(dane[[j]]), "~.", sep="")), 
                                          data=dane[[j]])
          lista_klas[[i]][[j]] <- predict(lista_modeli[[i]][[j]], newdata=testowy, type="class")
          lista_prawd[[i]][[j]] <- predict(lista_modeli[[i]][[j]], newdata=testowy, type="prob")[,2]
     }
}

for(i in 1:l){
     lista_pstw[[i]][[1]] <- 1 - lista_prawd[[i]][[1]] 
     lista_pstw[[i]][[ile_zbiorow[i]+1]] <- lista_prawd[[i]][[ile_zbiorow[i]]] 
     for(j in 2:ile_zbiorow[i]){
          lista_pstw[[i]][[j]] <- lista_prawd[[i]][[j-1]] - lista_prawd[[i]][[j]]     
     }
}


klasa <- vector("list", l)
for(j in 1:l){
     tabela <- matrix(lista_pstw[[j]][[1]])
     for(i in 2:(ile_zbiorow[j]+1)){
          tabela <- cbind(tabela, lista_pstw[[j]][[i]])
     }
     klasa[[j]] <- max.col(tabela)     
}

prawdziwy_y <- lapply(lista_targets, function(x) x$V1)
klasa

# ile to taki zwykly procent dopasowania na razie

ile <- numeric(l)
for(i in 1:l){
     ile[i] <- sum(prawdziwy_y[[i]] == klasa[[i]])*100/length(klasa[[i]])     
}

ile
