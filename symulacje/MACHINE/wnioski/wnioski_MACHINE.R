library("stringi")
library("dplyr")
library("xtable")

setwd("C:/Users/Marta/Desktop/Marta/GitHub/praca_magisterska/symulacje/MACHINE/wnioski")

d <- dir()

stri_extract_all_regex(d, ".*?[.]txt") %>%
     unlist() %>%
     na.omit() -> d

# klastrowa:

for(i in 1:length(d)){
     pomocnicza <- read.table(d[i], header=TRUE) %>%
          filter(rodzaj_klastra=="metoda_k_srednich") %>%
          select(-rodzaj_klastra, -ile_poziomow_zmiennej_odpowiedzi)
     pomocnicza <- rbind(d[i], c("VUS", "PPK", "ABSerr", "SB"), pomocnicza)

     if(i==1) tabela <- pomocnicza else tabela <- cbind(tabela, pomocnicza)
}

for(i in 3:nrow(tabela)){
     tabela[i, ] <- round(as.numeric(tabela[i, ]), 2) %>% stri_replace_all_fixed(., ".", ",")
     if(any(is.na(tabela[i, ]))){
          tabela[i, ][which(is.na(tabela[i, ])==TRUE)] <- " -- "
     }
}

tabela <- cbind(c("", "Liczba klastrów", 3, 5, 7, 10), tabela)

z <- print(xtable(tabela[-1, ]), include.rownames=FALSE, include.colnames=FALSE,
           only.contents=TRUE)

cat("\\begin{table}[h!t]\n\\centering\n\\begin{tabular}{r|", 
    rep("rrrr|", ncol(tabela)/4-1), "rrrr}\n", sep="")
cat("\\multicolumn{1}{c}{} & \\multicolumn{4}{c}{\\rotatebox{40}{Procesy gaussowskie}} & \\multicolumn{4}{c}{\\rotatebox{40}{Sieci neuronowe}} & \\multicolumn{4}{c}{\\rotatebox{40}{\\parbox{3.5cm}{Model proporcjonalnych szans}}}& \\multicolumn{4}{c}{\\rotatebox{40}{Metoda Franka i Halla}} & \\multicolumn{4}{c}{\\rotatebox{40}{\\parbox{4cm}{Wektory maszyn\\\\ podpierających (SVM)}}}\\\\")
print(xtable(tabela[-1, ]), include.rownames=FALSE, include.colnames=FALSE,
      only.contents=TRUE)
cat("\\end{tabular}\n\\caption{Wyniki analizy zbioru \\textit{machine} dla różnej liczby klas zmiennej odpowiedzi, stosując \\textbf{dyskretyzację klastrową}.}\n\\end{table}\n")

# rownomierna:

for(i in 1:length(d)){
     pomocnicza <- read.table(d[i], header=TRUE) %>%
          filter(rodzaj_klastra=="rownoliczne_klastry") %>%
          select(-rodzaj_klastra, -ile_poziomow_zmiennej_odpowiedzi)
     pomocnicza <- rbind(d[i], c("VUS", "PPK", "ABSerr", "SB"), pomocnicza)
     
     if(i==1) tabela <- pomocnicza else tabela <- cbind(tabela, pomocnicza)
}

for(i in 3:nrow(tabela)){
     tabela[i, ] <- round(as.numeric(tabela[i, ]), 2) %>% stri_replace_all_fixed(., ".", ",")
     if(any(is.na(tabela[i, ]))){
          tabela[i, ][which(is.na(tabela[i, ])==TRUE)] <- " -- "
     }
}

tabela <- cbind(c("", "Liczba klastrów", 3, 5, 7, 10), tabela)

z <- print(xtable(tabela[-1, ]), include.rownames=FALSE, include.colnames=FALSE,
           only.contents=TRUE)

cat("\\begin{table}[h!t]\n\\centering\n\\begin{tabular}{r|", 
    rep("rrrr|", ncol(tabela)/4-1), "rrrr}\n", sep="")
cat("\\multicolumn{1}{c}{} & \\multicolumn{4}{c}{\\rotatebox{40}{Procesy gaussowskie}} & \\multicolumn{4}{c}{\\rotatebox{40}{Sieci neuronowe}} & \\multicolumn{4}{c}{\\rotatebox{40}{\\parbox{3.5cm}{Model proporcjonalnych szans}}}& \\multicolumn{4}{c}{\\rotatebox{40}{Metoda Franka i Halla}} & \\multicolumn{4}{c}{\\rotatebox{40}{\\parbox{4cm}{Wektory maszyn\\\\ podpierających (SVM)}}}\\\\")
print(xtable(tabela[-1, ]), include.rownames=FALSE, include.colnames=FALSE,
      only.contents=TRUE)
cat("\\end{tabular}\n\\caption{Wyniki analizy zbioru \\textit{machine} dla różnej liczby klas zmiennej odpowiedzi, stosując \\textbf{dyskretyzację równomierną}.}\n\\end{table}\n")










