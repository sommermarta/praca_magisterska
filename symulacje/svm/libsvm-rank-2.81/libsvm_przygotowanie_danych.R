library("stringi")

setwd("C:\\Users\\Marta\\Desktop\\Marta\\GitHub\\praca_magisterska\\symulacje\\dane\\zmienione")

dane <- dir()
dane_tren <- dane[stri_detect_regex(dane, ".*?_train[.]txt")]
dane_test <- dane[stri_detect_regex(dane, ".*?_test[.]txt")]
dane_targets <- dane[stri_detect_regex(dane, ".*?_targets[.]txt")]
nazwy <- unlist(lapply(stri_match_all_regex(dane_tren, "(.*?)_train[.]txt"), function(x) x[,2]))

for(i in 1:length(dane_tren)){
     
     a <- read.table(dane_tren[i])
     
     b <- a
     b[,1] <- a[,ncol(a)]
     b[,2:ncol(b)] <- a[,1:(ncol(b)-1)]
     
     for(j in 2:ncol(b)){
          b[,j] <- paste(j, ":", b[,j], sep="")
     }
     
     write.table(b, 
                 paste("C:\\Users\\Marta\\Desktop\\Marta\\GitHub\\praca_magisterska\\symulacje\\libsvm-rank-2.81\\dane\\",
                       nazwy[i], ".train", sep=""), col.names=F, row.names=F, quote=F)
     
     a <- read.table(dane_test[i])
     b <- read.table(dane_targets[i])
     c <- cbind(b, a)
     
     for(j in 2:ncol(c)){
          c[,j] <- paste(j, ":", c[,j], sep="")
     }
     
     write.table(c, paste("C:\\Users\\Marta\\Desktop\\Marta\\GitHub\\praca_magisterska\\symulacje\\libsvm-rank-2.81\\dane\\",
                          nazwy[i], ".test", sep=""), col.names=F, row.names=F, quote=F)
     
}

