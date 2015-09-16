library("stringi")
library("dplyr")

setwd("C:\\Users\\Marta\\Desktop\\Marta\\GitHub\\praca_magisterska\\symulacje")

list.dirs() %>% 
     stri_extract_all_regex(., ".*?/podsumowanie") %>%
     unlist() %>%
     na.omit() -> podsumowania

wszystko <- vector("list", 0)

for(i in 1:length(podsumowania)){
     wszystko[[i]] <- paste(podsumowania[i], "/", dir(podsumowania[i]), sep="")
}

names(wszystko) <- c("gpor", "pom", "nn", "sa", "svm")

wszystko %>%
     unlist() %>%
     stri_match_all_regex(., "podsumowanie[/](.*?)_gpor_podsumowanie[.]txt") %>%
     lapply(., function(x) x[, 2]) %>%
     unlist() %>%
     na.omit() -> nazwy


for(i in 1:length(nazwy)){

     nazwa <- nazwy[i]
     
     m <- data.frame()     
     
     for(j in 1:5){
          
          przeczytaj <- readLines(wszystko[[j]][i], n=5)
          ile_klas <- przeczytaj[1] %>% as.numeric()
          ile_obs <- przeczytaj[2] %>% as.numeric()
          vus <- przeczytaj[3] %>% as.numeric()
          moj <- przeczytaj[4] %>% as.numeric()
          procent <- przeczytaj[5] %>% as.numeric()
          tabela <- read.table(wszystko[[j]][i], skip=5, header=TRUE)[,4]
          
          m <- rbind(m, c(vus, procent, moj, tabela))     
     }
     
     colnames(m) <- c("vus", "ppk", "abs", paste("pp_", 1:length(tabela), sep=""))
     rownames(m) <- names(wszystko)
     
     write.table(m, paste(".\\ostatecznie\\", nazwa, ".txt", sep=""), 
                 col.names=TRUE, row.names=TRUE, quote=FALSE)
     
}


# setwd(".\\ostatecznie")
# 
# a <- read.table(paste(nazwy[7], ".txt", sep=""), header=TRUE)
# View(a)














































