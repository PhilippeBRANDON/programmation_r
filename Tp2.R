#Pokemon 
"Exercice 1"

#a
library(readxl)
read_xlsx(pokemon.xlsx)

#b
dim(x = pokemon)

#c
colnames(x = pokemon)

#d
str(pokemon)

#e
pokemon$generation <- as.factor(x = pokemon$generation)
class(x = pokemon$generation)

pokemon$is_legendary <- as.factor(x = pokemon$is_legendary)
class(x = pokemon$is_legendary)

pokemon$type<- as.factor(x = pokemon$type)
class(x = pokemon$type)

#f
length(levels(pokemon$generation))
length(levels(pokemon$is_legendary))
length(levels(pokemon$type))

#g
summary(pokemon)


"Exercice 2"

#a
mean(pokemon$weight_kg, na.rm = TRUE)

#b
median(pokemon$weight_kg, na.rm = TRUE)

#c
quantile(x = pokemon$height_m, na.rm = TRUE)

#d
quantile(x = pokemon$height_m, probs = 0.1, na.rm = TRUE)

#e
var(x=pokemon$weight_kg, na.rm = TRUE)
sd(x=pokemon$weight_kg, na.rm = TRUE)

#f
tab1<-table(x=pokemon$generation)
tab2<-table(x=pokemon$is_legendary)
tab3<-table(x=pokemon$type)

sort(tab1)
sort(tab2)
sort(tab3)


"Exercice 3"

#a