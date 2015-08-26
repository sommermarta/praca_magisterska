library("dplyr")
library("stringi")
library("ggplot2")
library("gridExtra")

setwd("C:\\Users\\Marta\\Desktop\\Marta\\GitHub\\praca_magisterska\\symulacje\\dane")

###############################################################
###################### wczytywanie danych #####################
###############################################################

dane <- dir()
l <- length(dane)
lista_danych <- vector("list", l)

for(i in 1:l){
     lista_danych[[i]] <- read.table(dane[i])
}

# head(lista_danych[[6]])

###############################################################
###################### parametry danych #######################
###############################################################

ile_wierszy <- numeric(l)
ile_kolumn <- numeric(l)

for(i in 1:l){
     ile_wierszy[i] <- nrow(lista_danych[[i]])
     ile_kolumn[i] <- ncol(lista_danych[[i]])
}

nazwa_zbioru <- stri_match_all_regex(dane, "(.*?)[.]txt") %>% lapply(function(x) x[,2]) %>% unlist()
tabela <- data.frame(nazwa_zbioru, ile_wierszy, ile_kolumn)

wykresy <- vector("list", l)
for(i in 1:l){
     tabela2 <- as.data.frame(table(lista_danych[[i]][ile_kolumn[i]]))
     
     ggplot(tabela2, aes(x=Var1, y=Freq)) +
          geom_bar(stat="identity", color="black", fill="red")+
          theme_bw()+
          ggtitle(nazwa_zbioru[i])+
          theme(axis.text = element_text(colour="grey20",size=16,face="plain",
                                         family="serif"),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                plot.title=element_text(size=24,family="serif"),
                panel.grid.minor.x=element_blank(), 
                panel.grid.major.x=element_blank())  -> wykresy[[i]]  
}

grid.arrange(wykresy[[1]], wykresy[[2]], wykresy[[3]],
             wykresy[[4]], wykresy[[5]], wykresy[[6]],
             wykresy[[7]], wykresy[[8]], wykresy[[9]],ncol=3, nrow=3)


###############################################################
######### podzial na zbior testowy i treningowy ###############
###############################################################

set.seed(456872415)

# 30% - testowy, 70% - treningowy

for(i in 1:l){
     ile <- ceiling(0.3*ile_wierszy[i])
     s <- sample(1:ile_wierszy[i], ile)
     
     testowy <- lista_danych[[i]][s, ]
     treningowy <- lista_danych[[i]][-s, ]
     
     write.table(testowy, paste(nazwa_zbioru[i], "_testowy.txt", sep=""),
                 col.names=TRUE, row.names=FALSE, sep=" ", quote=FALSE)
     write.table(treningowy, paste(nazwa_zbioru[i], "_treningowy.txt", sep=""),
                 col.names=TRUE, row.names=FALSE, sep=" ", quote=FALSE)     
}

