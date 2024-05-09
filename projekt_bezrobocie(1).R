getwd()
setwd('C:/Users/user/Desktop/SEMESTR 4/STATYSTYKA')


install.packages("readxl")
library(readxl)
dane <- read_excel('BEZROBOTNI_REJESTROWANI.xlsx', 2, col_names =  F)
View(dane)


library(dplyr)
dane <- dane %>% slice(-c(2,4))


# tworzenie tabeli tylko dla stycznia

dane_styczen <- dane[,2:16]
View(dane_styczen)

h1 <- c(as.character(dane_styczen[1,1]), as.character(dane_styczen[2,2:length(dane_styczen[2,])])) 
h1
dane_styczen <- dane_styczen %>% slice(-c(1:2))
colnames(dane_styczen) <- h1

View(dane_styczen)


# chcemy porownac tylko 2014 i 2024

gen_dane <- dane_styczen[c(1, 5, 15) ]
View(gen_dane)


#zamiana na typ liczbowy

(typeof(gen_dane$`2014`))
gen_dane$`2014` <- as.numeric(gen_dane$`2014`)
gen_dane$`2024` <- as.numeric(gen_dane$`2024`)


# obliczenia parametrow

## srednia

(srednia_2014 <- mean(gen_dane$`2014`)) 
(srednia_2024 <- mean(gen_dane$`2024`))


## odchylenie standardowe

(odchylenie_2014 <- sd(gen_dane$`2014`))
(odchylenie_2024 <- sd(gen_dane$`2024`))


## wspolczynnik zmiennosci

(wspol_zm_2014 <- (odchylenie_2014 / srednia_2014) * 100)
(wspol_zm_2024 <- (odchylenie_2024 / srednia_2024) * 100)


## kwantyle i mediana

(kwantyl1_2014 <- quantile(gen_dane$`2014`, probs = 0.25))
(mediana_2014 <- median(gen_dane$`2014`))
(kwantyl3_2014 <- quantile(gen_dane$`2014`, probs = 0.75))

(kwantyl1_2024 <- quantile(gen_dane$`2024`, probs = 0.25))
(mediana_2024 <- median(gen_dane$`2024`))
(kwantyl3_2024 <- quantile(gen_dane$`2024`, probs = 0.75))


## wartosci min i max

# Wartość minimalna dla każdej kolumny
(min_values <- sapply(gen_dane[, c("2014", "2024")], min))

# Wartość maksymalna dla każdej kolumny
(max_values <- sapply(gen_dane[, c("2014", "2024")], max))


## Rozstep

(rozstep_2014 <- max_values[1] - min_values[1])
(rozstep_2024 <- max_values[2]- min_values[2])

## wariancja

(war_2014 <- var(gen_dane$`2014`))
(war_2024 <- var(gen_dane$`2024`))


## Skośność
install.packages("e1071")
library(e1071)
(skosnosc_2014 <- round(skewness(gen_dane$`2014`), 3))
(skosnosc_2024 <- round(skewness(gen_dane$`2024`), 3))


## Dominanta
(dominanta_2014 <- as.numeric(names(which.max(table(gen_dane$`2014`)))))
(dominanta_2024 <- as.numeric(names(which.max(table(gen_dane$`2024`)))))


# Wykresy

library(ggplot2)

