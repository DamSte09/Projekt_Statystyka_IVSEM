getwd()
setwd('C:/Users/user/Desktop/SEMESTR 4/STATYSTYKA')


install.packages("readxl")
library(readxl)
dane <- read_excel('BEZROBOTNI_REJESTROWANI.xlsx', 2, col_names =  F)
View(dane)


library(dplyr)
dane <- dane %>% slice(-c(2,4))

# Tworzenie tabeli tylko ze stycznia

dane_styczen <- dane[,2:16]

h1 <- c(as.character(dane_styczen[1,1]),
        as.character(dane_styczen[2,2:length(dane_styczen[2,])])) 
h1
dane_styczen <- dane_styczen %>% slice(-c(1:2))
colnames(dane_styczen) <- h1

View(dane_styczen)

# Chcemy tylko 2014 i 2024

gen_dane <- dane_styczen[c(1, 5, 15) ]

# Zamiana na typ liczbowy

(typeof(gen_dane$`2014`))
gen_dane$`2014` <- as.numeric(gen_dane$`2014`)
gen_dane$`2024` <- as.numeric(gen_dane$`2024`)

View(gen_dane)


# obliczenia parametrow

## srednia

(srednia_2014 <- mean(gen_dane$`2014`)) 
(srednia_2024 <- mean(gen_dane$`2024`))

## wariancja

(war_2014 <- var(gen_dane$`2014`))
(war_2024 <- var(gen_dane$`2024`))

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



## Skośność
install.packages("e1071")
library(e1071)
(skosnosc_2014 <- round(skewness(gen_dane$`2014`), 3))
(skosnosc_2024 <- round(skewness(gen_dane$`2024`), 3))


## Dominanta
(dominanta_2014 <- as.numeric(names(which.max(table(gen_dane$`2014`)))))
(dominanta_2024 <- as.numeric(names(which.max(table(gen_dane$`2024`)))))


## Kurtoza
(kurtoza_2014 <- kurtosis(gen_dane$`2014`))
(kurtoza_2014 <- kurtosis(gen_dane$`2024`))


# Wykresy

library(ggplot2)


## Wykres pudełkowy

#install.packages("reshape2")
library(reshape2)
gen_dane_long <- melt(gen_dane, variable.name = 'Rok', value.name = 'Bezrobotni')
View(gen_dane_long)

ggplot(gen_dane_long, aes(x = Rok, y = Bezrobotni, fill = Rok)) +
  geom_boxplot() +
  labs(title = "Wykres pudełkowy ilości osób bezrobotnych",
       y = "Ilość osób bezrobotnych uprzednio pracujących") +
  theme_minimal()


## Histogram

ggplot(gen_dane, aes(x=`2014`)) +
  geom_histogram( binwidth=1000,
                 color="darkblue", fill="lightblue") +
  labs(title="Histogram osób bezrobotnych(przedział 1000 osób) w 2014 roku",
       x="Ilość osób", y="Ilość powiatów") 


## Wykres dystrybuanty

ggplot(gen_dane_long, aes(x = Bezrobotni, color = Rok)) +
  stat_ecdf(size = 1) +
  labs(title = "Wykres dystrybuanty",
       x = "Ilość ludzi bezrobotnych",
       y = "Dystrybuanta",
       color = "Rok") +  
  theme_minimal() +
  theme(legend.position = "right")  


## Wykres kwantyl - kwantyl

qqplot(gen_dane$`2014`, gen_dane$`2024`,
       main = "Wykres kwantyl-kwantyl dla 2014 vs 2024", 
       xlab = "kwantyle roku 2014", ylab = "kwantyle roku 2024")

# Dodanie linii referencyjnej
abline(0, 1, col = "red")


# Hipotezy


# Hipoteza o rozkladzie 

## Hipoteza zerowa
### Dane pochodzą z rozkładu normalnego

## Hipoteza alternatywna
### Dane nie pochodzą z rozkładu normalnego

shapiro_test_2014 <- shapiro.test(gen_dane$`2014`)
print(shapiro_test_2014)
shapiro_test_2014$p.value

shapiro_test_2024 <- shapiro.test(gen_dane$`2024`)
print(shapiro_test_2024)
shapiro_test_2024$p.value


# p value jest mniejsze niz 0,05 więc hipoteza zerowa zostaje odrzucona



# Hipoteza o sredniej (sparowane, te same powiaty)

## Hipoteza zerowa
### srednias ludzi bezrobotnych nie zmieniła się

## Hipoteza alternatywna
### srednia ludzi bezrobotnych zmieniła się

wilc <- wilcox.test(gen_dane$`2014`, gen_dane$`2024`,
                    paired = TRUE)
print(wilc)
wilc$p.value


# p value jest mniejsze niz 0,05 więc hipoteza zerowa jest nieprawdziwa



