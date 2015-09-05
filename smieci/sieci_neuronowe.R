library("stringi")
library("dplyr")
library("rpart")

setwd("C:\\Users\\Marta\\Desktop\\Marta\\GitHub\\praca_magisterska\\symulacje\\dane\\zmienione")

# wczytanie danych

dane <- dir()
dane_tren <- dane[stri_detect_regex(dane, ".*?_train[.]txt")]
dane_test <- dane[stri_detect_regex(dane, ".*?_test[.]txt")]
dane_targets <- dane[stri_detect_regex(dane, ".*?_targets[.]txt")]

dane <- read.table(dane_tren[4], header=FALSE)
test <- read.table(dane_test[4], header=FALSE)

K <- ncol(dane)-1
ile_wierszy <- nrow(dane)
ile_poziomow <- length(table(dane[,K+1]))

s <- sample(1:ile_wierszy, 1)

wyjscie0 <- dane[s, -(K+1)]
y <- dane[s, K+1]

y <- c(rep(1, y), rep(0, ile_poziomow-y))
wyjscie2 <- y

m <- K+5

wyjscie1 <- numeric(m)

wagi1 <- matrix(0.01, ncol=m, nrow=K)
wagi2 <- matrix(0.01, ncol=ile_poziomow, nrow=m)
eta <- 0.01
epsilon <- 0.01

f <- function(x){1/(1+exp(-x))}

for(i in 1:m){
     wyjscie1[i] <- f(sum(wagi1[,i]*wyjscie0))
}
for(i in 1:ile_poziomow){
     wyjscie2[i] <- f(sum(wagi2[,i]*wyjscie1))
}

delta_wyjsciowa <- wyjscie2*(1-wyjscie2)*(wyjscie2-y)
delta_ukryta <- numeric(m) 
     
stala <- wyjscie1*(1-wyjscie1)
for(i in 1:m){
     delta_ukryta[i] <- stala[i]*sum(wagi2[i,]*delta_wyjsciowa)
}

wagi1
for(i in 1:K){
     for(j in 1:m){
          wagi1[i, j] <- wagii1[i, j]-eta*
     }
}




install.packages('neuralnet')
library("neuralnet")

?neuralnet


dane
m <- matrix(0, ncol=5, nrow=nrow(dane))
for(i in 1:nrow(dane)){
     m[i, ] <- c(rep(1, dane[i,14]), rep(0, 5-dane[i, 14]))
}

dane2 <- cbind(dane[,-14], m)
colnames(dane2) <- c("input1","input2","input3","input4","input5","input6","input7","input8","input9","input10","input11","input12","input13","output1","output2","output3","output4","output5")
colnames(test) <- c("input1","input2","input3","input4","input5","input6","input7","input8","input9","input10","input11","input12","input13")

a <- "\"input1\""
for(i in 2:13){
     a <- paste(a, ",\"", "input", i, "\"", sep="")
}
for(i in 1:5){
     a <- paste(a, ",\"", "output", i, "\"", sep="")
}
cat(a)


b <- "input1+"
for(i in 2:13){
     if(i==13) b <- paste(b, "input", i, "~", sep="") else b <- paste(b, "input", i, "+", sep="")
}
for(i in 1:5){
     if(i==5) b <- paste(b, "output", i, sep="") else b <- paste(b, "output", i, "+", sep="")
}
cat(b)


net <- neuralnet(output1+output2+output3+output4+output5~input1+input2+input3+input4+input5+input6+input7+input8+input9+input10+input11+input12+input13,
                 dane2, hidden=0, rep=10)
print(net)
prediction(net, test)
o <- compute(net, test)$net.result

library("nnet")
net <- nnet(input1+input2+input3+input4+input5+input6+input7+input8+input9+input10+input11+input12+input13~output1+output2+output3+output4+output5,
          dane2, size=15)
o <- predict(net, test)

