library("dplyr")

load("..\\..\\lista_danych_machine.RData")

k <- c(3, 5, 7, 10)

for(i in 1:length(lista_przedzialy)){
    
    dane_train <- lista_przedzialy[[i]][s, ] %>% arrange(y)
    dane_test <- lista_przedzialy[[i]][-s, -ncol(lista_przedzialy[[i]])] 
    dane_targ <- lista_przedzialy[[i]][-s, ]$y %>% as.data.frame
    
    write.table(dane_train, 
                paste(".\\przedzialy", k[i], "_train.0", sep=""),
                col.names=FALSE, row.names=FALSE, quote=FALSE)
    write.table(dane_test, 
                paste(".\\przedzialy", k[i], "_test.0", sep=""),
                col.names=FALSE, row.names=FALSE, quote=FALSE)
    write.table(dane_targ, 
                paste(".\\przedzialy", k[i], "_targets.0", sep=""),
                col.names=FALSE, row.names=FALSE, quote=FALSE)
    
}

for(i in 1:length(lista_rownoliczne)){
    
    dane_train <- lista_rownoliczne[[i]][s, ] %>% arrange(y)
    dane_test <- lista_rownoliczne[[i]][-s, -ncol(lista_rownoliczne[[i]])] 
    dane_targ <- lista_rownoliczne[[i]][-s, ]$y %>% as.data.frame
    
    write.table(dane_train, 
                paste(".\\rownoliczne", k[i], "_train.0", sep=""),
                col.names=FALSE, row.names=FALSE, quote=FALSE)
    write.table(dane_test, 
                paste(".\\rownoliczne", k[i], "_test.0", sep=""),
                col.names=FALSE, row.names=FALSE, quote=FALSE)
    write.table(dane_targ, 
                paste(".\\rownoliczne", k[i], "_targets.0", sep=""),
                col.names=FALSE, row.names=FALSE, quote=FALSE)
    
}






