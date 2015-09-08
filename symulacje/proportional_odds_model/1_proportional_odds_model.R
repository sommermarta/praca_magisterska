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

dane_tren %>%
     stri_match_all_regex(., "(.*?)_train[.]txt") %>%
     lapply(., function(x) x[,2]) %>%
     unlist() -> nazwy

# for(i in 1:l){
#      paste("m", i, " <- polr(as.factor(V", ile_kolumn[i], ")~., data=lista_tren[[",
#            i, "]], Hess=TRUE)\nm", i, 
#            "_pred_prob <- predict(m", i, ", lista_test[[", i, "]], type=\"prob\")\nm", i, 
#            "_pred_class <- predict(m", i, ", lista_test[[", i, "]], type=\"class\")\nm", i, 
#            "_praw_class <- lista_targets[[", i, "]]\n\n", sep="") -> tekst
#      cat(tekst)     
# }

# # m1 <- polr(as.factor(V11)~., data=lista_tren[[1]], Hess=TRUE)
# # m1_pred_prob <- predict(m1, lista_test[[1]], type="prob")
# # m1_pred_class <- predict(m1, lista_test[[1]], type="class")
# # m1_praw_class <- lista_targets[[1]]
# 
# m2 <- polr(as.factor(V8)~., data=lista_tren[[2]], Hess=TRUE)
# m2_pred_prob <- predict(m2, lista_test[[2]], type="prob")
# m2_pred_class <- predict(m2, lista_test[[2]], type="class")
# m2_praw_class <- lista_targets[[2]]
# 
# m3 <- polr(as.factor(V3)~., data=lista_tren[[3]], Hess=TRUE)
# m3_pred_prob <- predict(m3, lista_test[[3]], type="prob")
# m3_pred_class <- predict(m3, lista_test[[3]], type="class")
# m3_praw_class <- lista_targets[[3]]
# 
# m4 <- polr(as.factor(V14)~., data=lista_tren[[4]], Hess=TRUE)
# m4_pred_prob <- predict(m4, lista_test[[4]], type="prob")
# m4_pred_class <- predict(m4, lista_test[[4]], type="class")
# m4_praw_class <- lista_targets[[4]]
# 
# # m5 <- polr(as.factor(V7)~., data=lista_tren[[5]], Hess=TRUE)
# # m5_pred_prob <- predict(m5, lista_test[[5]], type="prob")
# # m5_pred_class <- predict(m5, lista_test[[5]], type="class")
# # m5_praw_class <- lista_targets[[5]]
# 
# # m6 <- polr(as.factor(V28)~., data=lista_tren[[6]], Hess=TRUE)
# # m6_pred_prob <- predict(m6, lista_test[[6]], type="prob")
# # m6_pred_class <- predict(m6, lista_test[[6]], type="class")
# # m6_praw_class <- lista_targets[[6]]
# 
# m7 <- polr(as.factor(V10)~., data=lista_tren[[7]], Hess=TRUE)
# m7_pred_prob <- predict(m7, lista_test[[7]], type="prob")
# m7_pred_class <- predict(m7, lista_test[[7]], type="class")
# m7_praw_class <- lista_targets[[7]]
# 
# # m8 <- polr(as.factor(V61)~., data=lista_tren[[8]], Hess=TRUE)
# # m8_pred_prob <- predict(m8, lista_test[[8]], type="prob")
# # m8_pred_class <- predict(m8, lista_test[[8]], type="class")
# # m8_praw_class <- lista_targets[[8]]
# 
# m9 <- polr(as.factor(V33)~., data=lista_tren[[9]], Hess=TRUE)
# m9_pred_prob <- predict(m9, lista_test[[9]], type="prob")
# m9_pred_class <- predict(m9, lista_test[[9]], type="class")
# m9_praw_class <- lista_targets[[9]]
# 
# # 1, 5, 6, 8 -> nie chca sie zrobic
# 
# m1 <- lrm(as.factor(V11)~., data=lista_tren[[1]][,-3])
# m1_pred_prob <- predict(m1, lista_test[[1]], type="fitted.ind")
# m1_pred_class <- max.col(m1_pred_prob)
# m1_pred_f <- predict(m1, lista_test[[1]], type="lp")
# m1_praw_class <- lista_targets[[1]]
# 
# m5 <- lrm(as.factor(V7)~., data=lista_tren[[5]])
# m5_pred_prob <- predict(m5, lista_test[[5]], type="fitted.ind")
# m5_pred_class <- max.col(m5_pred_prob)
# m5_pred_f <- predict(m5, lista_test[[5]], type="lp")
# m5_praw_class <- lista_targets[[5]]
# 
# # tu wciaz nie dziala...:
# 
# # m6 <- lrm(as.factor(V28)~., data=lista_tren[[6]][,c(-25, -27)])
# # m6_pred_prob <- predict(m6, lista_test[[6]], type="fitted.ind")
# # m6_pred_class <- max.col(m6_pred_prob)
# # m6_pred_f <- predict(m6, lista_test[[6]], type="lp")
# # m6_praw_class <- lista_targets[[6]]
# # 
# # m8 <- lrm(as.factor(V61)~., data=lista_tren[[8]][,-c(58:60, 53:54, 43:44, 24:30, 20, 14, 19)])
# # m8_pred_prob <- predict(m5, lista_test[[5]], type="fitted.ind")
# # m8_pred_class <- max.col(m5_pred_prob)
# # m8_pred_f <- predict(m5, lista_test[[5]], type="lp")
# # m8_praw_class <- lista_targets[[8]]
# 
# gdzie <- "C:\\Users\\Marta\\Desktop\\Marta\\GitHub\\praca_magisterska\\symulacje\\proportional_odds_model\\model_pom"
# save(m1, m1_pred_prob, m1_pred_class, m1_pred_f, m1_praw_class,
#      m2, m2_pred_prob, m2_pred_class, m2_pred_f, m2_praw_class,
#      m3, m3_pred_prob, m3_pred_class, m3_pred_f, m3_praw_class,
#      m4, m4_pred_prob, m4_pred_class, m4_pred_f, m4_praw_class,
#      m5, m5_pred_prob, m5_pred_class, m5_pred_f, m5_praw_class,
# #     m6, m6_pred_prob, m6_pred_class, m6_pred_f, m6_praw_class,
#      m7, m7_pred_prob, m7_pred_class, m7_pred_f, m7_praw_class,
# #     m8, m8_pred_prob, m8_pred_class, m8_pred_f, m8_praw_class,
#      m9, m9_pred_prob, m9_pred_class, m9_pred_f, m9_praw_class,
#      file=gdzie)

# load(gdzie)

# for(i in 1:l){
#      paste("m", i, " <- lrm(as.factor(V", ile_kolumn[i], ")~., data=lista_tren[[",
#            i, "]])\nm", i, 
#            "_pred_prob <- predict(m", i, ", lista_test[[", i, "]], type=\"fitted.ind\")\nm", i, 
#            "_pred_class <- max.col(m", i, "_pred_prob)\nm", i, 
#            "_pred_f <- predict(m", i, ", lista_test[[", i, "]], type=\"lp\")\nm", i, 
#            "_praw_class <- lista_targets[[", i, "]]\n\n", sep="") -> tekst
#      cat(tekst)     
# }

m1 <- lrm(as.factor(V11)~., data=lista_tren[[1]][,-3])
m1_pred_prob <- predict(m1, lista_test[[1]], type="fitted.ind")
m1_pred_class <- max.col(m1_pred_prob)
m1_pred_f <- predict(m1, lista_test[[1]], type="lp")
m1_praw_class <- lista_targets[[1]]

m2 <- lrm(as.factor(V8)~., data=lista_tren[[2]])
m2_pred_prob <- predict(m2, lista_test[[2]], type="fitted.ind")
m2_pred_class <- max.col(m2_pred_prob)
m2_pred_f <- predict(m2, lista_test[[2]], type="lp")
m2_praw_class <- lista_targets[[2]]

m3 <- lrm(as.factor(V3)~., data=lista_tren[[3]])
m3_pred_prob <- predict(m3, lista_test[[3]], type="fitted.ind")
m3_pred_class <- max.col(m3_pred_prob)
m3_pred_f <- predict(m3, lista_test[[3]], type="lp")
m3_praw_class <- lista_targets[[3]]

m4 <- lrm(as.factor(V14)~., data=lista_tren[[4]])
m4_pred_prob <- predict(m4, lista_test[[4]], type="fitted.ind")
m4_pred_class <- max.col(m4_pred_prob)
m4_pred_f <- predict(m4, lista_test[[4]], type="lp")
m4_praw_class <- lista_targets[[4]]

m5 <- lrm(as.factor(V7)~., data=lista_tren[[5]])
m5_pred_prob <- predict(m5, lista_test[[5]], type="fitted.ind")
m5_pred_class <- max.col(m5_pred_prob)
m5_pred_f <- predict(m5, lista_test[[5]], type="lp")
m5_praw_class <- lista_targets[[5]]

m6 <- lrm(as.factor(V28)~., data=data.frame(scale(lista_tren[[6]][, -c(25, 27, 26)])), maxit=50)
m6_pred_prob <- predict(m6, lista_test[[6]], type="fitted.ind")
m6_pred_class <- max.col(m6_pred_prob)
m6_pred_f <- predict(m6, lista_test[[6]], type="lp")
m6_praw_class <- lista_targets[[6]]

m7 <- lrm(as.factor(V10)~., data=lista_tren[[7]])
m7_pred_prob <- predict(m7, lista_test[[7]], type="fitted.ind")
m7_pred_class <- max.col(m7_pred_prob)
m7_pred_f <- predict(m7, lista_test[[7]], type="lp")
m7_praw_class <- lista_targets[[7]]

m8 <- lrm(as.factor(V61)~., data=data.frame(scale(lista_tren[[8]][,-c(58:60, 53:54, 43:44, 24:30, 20, 14, 19)])),
          maxit=50)
m8_pred_prob <- predict(m8, lista_test[[8]], type="fitted.ind")
m8_pred_class <- max.col(m8_pred_prob)
m8_pred_f <- predict(m8, lista_test[[8]], type="lp")
m8_praw_class <- lista_targets[[8]]

m9 <- lrm(as.factor(V33)~., data=lista_tren[[9]][,-c(31,28, 24)])
m9_pred_prob <- predict(m9, lista_test[[9]], type="fitted.ind")
m9_pred_class <- max.col(m9_pred_prob)
m9_pred_f <- predict(m9, lista_test[[9]], type="lp")
m9_praw_class <- lista_targets[[9]]

model <- vector("list", 9)
pred_prob <- vector("list", 9)
pred_class <- vector("list", 9)
pred_f <- vector("list", 9)
praw_class <- vector("list", 9)

# for(i in 1:9){
#      cat(paste("model[[", i, "]] <- m", i, "\n", sep=""))
#      cat(paste("pred_prob[[", i, "]] <- m", i, "_pred_prob\n", sep=""))
#      cat(paste("pred_class[[", i, "]] <- m", i, "_pred_class\n", sep=""))
#      cat(paste("pred_f[[", i, "]] <- m", i, "_pred_f\n", sep=""))
#      cat(paste("praw_class[[", i, "]] <- m", i, "_praw_class\n\n", sep=""))
# }

model[[1]] <- m1
pred_prob[[1]] <- m1_pred_prob
pred_class[[1]] <- m1_pred_class
pred_f[[1]] <- m1_pred_f
praw_class[[1]] <- m1_praw_class

model[[2]] <- m2
pred_prob[[2]] <- m2_pred_prob
pred_class[[2]] <- m2_pred_class
pred_f[[2]] <- m2_pred_f
praw_class[[2]] <- m2_praw_class

model[[3]] <- m3
pred_prob[[3]] <- m3_pred_prob
pred_class[[3]] <- m3_pred_class
pred_f[[3]] <- m3_pred_f
praw_class[[3]] <- m3_praw_class

model[[4]] <- m4
pred_prob[[4]] <- m4_pred_prob
pred_class[[4]] <- m4_pred_class
pred_f[[4]] <- m4_pred_f
praw_class[[4]] <- m4_praw_class

model[[5]] <- m5
pred_prob[[5]] <- m5_pred_prob
pred_class[[5]] <- m5_pred_class
pred_f[[5]] <- m5_pred_f
praw_class[[5]] <- m5_praw_class

model[[6]] <- m6
pred_prob[[6]] <- m6_pred_prob
pred_class[[6]] <- m6_pred_class
pred_f[[6]] <- m6_pred_f
praw_class[[6]] <- m6_praw_class

model[[7]] <- m7
pred_prob[[7]] <- m7_pred_prob
pred_class[[7]] <- m7_pred_class
pred_f[[7]] <- m7_pred_f
praw_class[[7]] <- m7_praw_class

model[[8]] <- m8
pred_prob[[8]] <- m8_pred_prob
pred_class[[8]] <- m8_pred_class
pred_f[[8]] <- m8_pred_f
praw_class[[8]] <- m8_praw_class

model[[9]] <- m9
pred_prob[[9]] <- m9_pred_prob
pred_class[[9]] <- m9_pred_class
pred_f[[9]] <- m9_pred_f
praw_class[[9]] <- m9_praw_class

# gdzie <- "C:\\Users\\Marta\\Desktop\\Marta\\GitHub\\praca_magisterska\\symulacje\\proportional_odds_model\\model_pom"
# save(model, pred_prob, pred_class, pred_f, praw_class,
#      nazwy,
#      file=gdzie)

############################################################################



















