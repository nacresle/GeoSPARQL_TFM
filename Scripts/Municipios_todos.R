
library(dplyr)

Datos <- read.csv("recintos_municipales_inspire_TODOS.csv")

summary(Datos)

which(duplicated(Datos))
which(duplicated(Datos$IDmuni))
Datos[c(3094,3161,3163,3217,4738,8000),]

which(Datos$IDmuni == 33018)
which(Datos$IDmuni == 33069)
which(Datos$IDmuni == 33070)
which(Datos$IDmuni == 7032)
which(Datos$IDmuni == 46180)
which(Datos$IDmuni == 3014)

Datos[c(3093, 3094),] #Coaña y Cuaña es el mismo municipio pero con nombre asturiano y nombre castellano
Datos[c(3160, 3161),] #Soto del Barco/Sotu'l Barcu
Datos[c(3162, 3163),] #Tapia/Tapia de Casariego
Datos[c(3216, 3217),] #Maó/ Maó_Mahón
Datos[c(4737, 4738),] #Novetlè/Novelé Novelé/Novetlè   
Datos[c(7999, 8000),] #Alacant/Alicante Alicante/Alacant

Datos_repe <- Datos[c(3093, 3094, 3160, 3161, 3162, 3163, 3216, 3217, 4737, 4738, 7999, 8000),] 



