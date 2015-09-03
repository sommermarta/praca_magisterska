library("MASS")
library("stringi")
library("rms")

setwd("C:\\Users\\Marta\\Desktop\\Marta\\GitHub\\praca_magisterska\\symulacje\\dane\\zmienione")

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

# for(i in 1:l){
#      paste("m", i, " <- polr(as.factor(V", ile_kolumn[i], ")~., data=lista_tren[[",
#            i, "]], Hess=TRUE)\nm", i, 
#            "_pred_prob <- predict(m", i, ", lista_test[[", i, "]], type=\"prob\")\nm", i, 
#            "_pred_class <- predict(m", i, ", lista_test[[", i, "]], type=\"class\")\nm", i, 
#            "_praw_class <- lista_targets[[", i, "]]\n\n", sep="") -> tekst
#      cat(tekst)     
# }

m1 <- polr(as.factor(V11)~., data=lista_tren[[1]], Hess=TRUE)
m1_pred_prob <- predict(m1, lista_test[[1]], type="prob")
m1_pred_class <- predict(m1, lista_test[[1]], type="class")
m1_praw_class <- lista_targets[[1]]

m2 <- polr(as.factor(V8)~., data=lista_tren[[2]], Hess=TRUE)
m2_pred_prob <- predict(m2, lista_test[[2]], type="prob")
m2_pred_class <- predict(m2, lista_test[[2]], type="class")
m2_praw_class <- lista_targets[[2]]

m3 <- polr(as.factor(V3)~., data=lista_tren[[3]], Hess=TRUE)
m3_pred_prob <- predict(m3, lista_test[[3]], type="prob")
m3_pred_class <- predict(m3, lista_test[[3]], type="class")
m3_praw_class <- lista_targets[[3]]

m4 <- polr(as.factor(V14)~., data=lista_tren[[4]], Hess=TRUE)
m4_pred_prob <- predict(m4, lista_test[[4]], type="prob")
m4_pred_class <- predict(m4, lista_test[[4]], type="class")
m4_praw_class <- lista_targets[[4]]

m5 <- polr(as.factor(V7)~., data=lista_tren[[5]], Hess=TRUE)
m5_pred_prob <- predict(m5, lista_test[[5]], type="prob")
m5_pred_class <- predict(m5, lista_test[[5]], type="class")
m5_praw_class <- lista_targets[[5]]

m6 <- polr(as.factor(V28)~., data=lista_tren[[6]], Hess=TRUE)
m6_pred_prob <- predict(m6, lista_test[[6]], type="prob")
m6_pred_class <- predict(m6, lista_test[[6]], type="class")
m6_praw_class <- lista_targets[[6]]

m7 <- polr(as.factor(V10)~., data=lista_tren[[7]], Hess=TRUE)
m7_pred_prob <- predict(m7, lista_test[[7]], type="prob")
m7_pred_class <- predict(m7, lista_test[[7]], type="class")
m7_praw_class <- lista_targets[[7]]

m8 <- polr(as.factor(V61)~., data=lista_tren[[8]], Hess=TRUE)
m8_pred_prob <- predict(m8, lista_test[[8]], type="prob")
m8_pred_class <- predict(m8, lista_test[[8]], type="class")
m8_praw_class <- lista_targets[[8]]

m9 <- polr(as.factor(V33)~., data=lista_tren[[9]], Hess=TRUE)
m9_pred_prob <- predict(m9, lista_test[[9]], type="prob")
m9_pred_class <- predict(m9, lista_test[[9]], type="class")
m9_praw_class <- lista_targets[[9]]

# 1, 5, 6, 8 -> nie chca sie zrobic

m1 <- lrm(as.factor(V11)~., data=lista_tren[[1]][,-3])
m1_pred_prob <- predict(m1, lista_test[[1]], type="fitted.ind")
m1_pred_class <- max.col(m1_pred_prob)
m1_pred_f <- predict(m1, lista_test[[1]], type="lp")
m1_praw_class <- lista_targets[[1]]

m5 <- lrm(as.factor(V7)~., data=lista_tren[[5]])
m5_pred_prob <- predict(m5, lista_test[[5]], type="fitted.ind")
m5_pred_class <- max.col(m5_pred_prob)
m5_pred_f <- predict(m5, lista_test[[5]], type="lp")
m5_praw_class <- lista_targets[[5]]

# tu wciaz nie dziala...:

m6 <- lrm(as.factor(V28)~., data=lista_tren[[6]][,c(-25, -27)])
m6_pred_prob <- predict(m6, lista_test[[6]], type="fitted.ind")
m6_pred_class <- max.col(m6_pred_prob)
m6_pred_f <- predict(m6, lista_test[[6]], type="lp")
m6_praw_class <- lista_targets[[6]]

m8 <- lrm(as.factor(V61)~., data=lista_tren[[8]][,-c(58:60, 53:54, 43:44, 24:30, 20, 14, 19)])
m8_pred_prob <- predict(m5, lista_test[[5]], type="fitted.ind")
m8_pred_class <- max.col(m5_pred_prob)
m8_pred_f <- predict(m5, lista_test[[5]], type="lp")
m8_praw_class <- lista_targets[[8]]




