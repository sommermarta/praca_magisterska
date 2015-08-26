library("MASS")
library("stringi")

setwd("C:\\Users\\Marta\\Desktop\\Marta\\GitHub\\praca_magisterska\\symulacje\\dane")

dane <- dir()
dane_tren <- dane[stri_detect_regex(dane, ".*?_treningowy[.]txt")]
dane_test <- dane[stri_detect_regex(dane, ".*?_testowy[.]txt")]

l <- length(dane_tren)
lista_tren <- vector("list", l)
lista_test <- vector("list", l)

for(i in 1:l){
     lista_tren[[i]] <- read.table(dane_tren[i], header=TRUE)
     lista_test[[i]] <- read.table(dane_test[i], header=TRUE)
}

ile_kolumn <- unlist(lapply(lista_tren, function(x) ncol(x)))

# for(i in 1:l){
#      paste("m", i, " <- polr(as.factor(V", ile_kolumn[i], ")~., data=lista_tren[[",
#            i, "]], Hess=TRUE)\nm", i, 
#            "_pred_prob <- predict(m", i, ", lista_test[[", i, "]][, -", 
#            ile_kolumn[i], "], type=\"prob\")\nm", i, 
#            "_pred_class <- predict(m", i, ", lista_test[[", i, "]][, -", 
#            ile_kolumn[i], "], type=\"class\")\nm", i, 
#            "_praw_class <- lista_test[[", i, "]][, ", 
#            ile_kolumn[i], "]\n\n", sep="") -> tekst
#      cat(tekst)     
# }


m1 <- polr(as.factor(V11)~., data=lista_tren[[1]], Hess=TRUE)
m1_pred_prob <- predict(m1, lista_test[[1]][, -11], type="prob")
m1_pred_class <- predict(m1, lista_test[[1]][, -11], type="class")
m1_praw_class <- lista_test[[1]][, 11]

m2 <- polr(as.factor(V8)~., data=lista_tren[[2]], Hess=TRUE)
m2_pred_prob <- predict(m2, lista_test[[2]][, -8], type="prob")
m2_pred_class <- predict(m2, lista_test[[2]][, -8], type="class")
m2_praw_class <- lista_test[[2]][, 8]

m3 <- polr(as.factor(V3)~., data=lista_tren[[3]], Hess=TRUE)
m3_pred_prob <- predict(m3, lista_test[[3]][, -3], type="prob")
m3_pred_class <- predict(m3, lista_test[[3]][, -3], type="class")
m3_praw_class <- lista_test[[3]][, 3]

m4 <- polr(as.factor(V14)~., data=lista_tren[[4]], Hess=TRUE)
m4_pred_prob <- predict(m4, lista_test[[4]][, -14], type="prob")
m4_pred_class <- predict(m4, lista_test[[4]][, -14], type="class")
m4_praw_class <- lista_test[[4]][, 14]

m5 <- polr(as.factor(V7)~., data=lista_tren[[5]], Hess=TRUE)
m5_pred_prob <- predict(m5, lista_test[[5]][, -7], type="prob")
m5_pred_class <- predict(m5, lista_test[[5]][, -7], type="class")
m5_praw_class <- lista_test[[5]][, 7]

m6 <- polr(as.factor(V28)~., data=lista_tren[[6]], Hess=TRUE)
m6_pred_prob <- predict(m6, lista_test[[6]][, -28], type="prob")
m6_pred_class <- predict(m6, lista_test[[6]][, -28], type="class")
m6_praw_class <- lista_test[[6]][, 28]

m7 <- polr(as.factor(V10)~., data=lista_tren[[7]], Hess=TRUE)
m7_pred_prob <- predict(m7, lista_test[[7]][, -10], type="prob")
m7_pred_class <- predict(m7, lista_test[[7]][, -10], type="class")
m7_praw_class <- lista_test[[7]][, 10]

m8 <- polr(as.factor(V61)~., data=lista_tren[[8]], Hess=TRUE)
m8_pred_prob <- predict(m8, lista_test[[8]][, -61], type="prob")
m8_pred_class <- predict(m8, lista_test[[8]][, -61], type="class")
m8_praw_class <- lista_test[[8]][, 61]

m9 <- polr(as.factor(V33)~., data=lista_tren[[9]], Hess=TRUE)
m9_pred_prob <- predict(m9, lista_test[[9]][, -33], type="prob")
m9_pred_class <- predict(m9, lista_test[[9]][, -33], type="class")
m9_praw_class <- lista_test[[9]][, 33]

# 1, 5, 6, 8 -> nie chca sie zrobic

install.packages("ordinal")
library("ordinal")
