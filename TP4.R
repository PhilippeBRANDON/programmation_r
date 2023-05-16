#EXERCICE N°1
#1
population <- rnorm(10000000, mean = 171, sd = 9)

#2
moyenne <- mean(population)
ecartype <- sd(population)
#oui cela correspond 
#3
hist(population, main = "taille des francais")

#4
grand <- length(population[population > 190])
proba1 <- pnorm(190, mean = 171, sd = 9)
#en théorie, on devrait en trouver : 1-proba1*length(population) = 173813.8

#5
petit <- grand <- length(population[population < 144])
proba1 <- pnorm(144, mean = 171, sd = 9)
#en théorie, on devrait en trouver : proba1*length(population) = 13498.98

#EXERCICE N°2
#1
echantillion <- population[sample(c(1:10000000), size = 100, replace = TRUE)]
echantillion2.0 <- sample(x = population, size = 100, replace = TRUE)
ecartypeechantillion <- sd(echantillion)
moyenneechantillion <- mean(echantillion)
#les valeurs obtenue diffère un peu

#2
risque <- qnorm(p = 1-(0.05/2))
amplitude <- risque*ecartypeechantillion/sqrt(length(echantillion))
intervalleg <- moyenneechantillion - amplitude
intervalled <- moyenneechantillion + amplitude

#3
tableau <- replicate(1000, sample(x = population, size = 100, replace = TRUE))
moyennetab <- apply(X=tableau, MARGIN = 2, FUN = mean)
ecartypetab <- apply(X=tableau, MARGIN = 2, FUN = sd)
datef <- rbind(moyennetab,ecartypetab)

#4
hist(x = datef[1,], main = "vue des moyenne de tout les echantillions")

#5
moyennedemoyenne <- mean(datef[1, ])
ecartypedemoyenne <- sd(datef[1,])

#6
supechantillion <- length(datef[1, ][datef[1, ]>172.8])

#7
amplitudeech <- risque*datef[2,]/sqrt(length(1000))
datef <- rbind(datef,amplitudeech)
intervallegech <- datef[1,] - datef[3,]
intervalledech <- datef[1,] + datef[3,]
datef <- rbind(datef,intervalledech,intervalledech)

#8
library(gplots)
plotCI(intervallegech,invtervalledroite)