#install.packages("sqldf")
#install.packages("dplyr")

library(dplyr)
library(sqldf)

my_virtdb <- read.csv ("Parcelas_Con_muni.csv")
onto_IFN <- read.csv ("Parcelas_Con_muni_onto.csv")
plots_no_coor <- read.csv ("Parcelas_sn_coor.csv")

which(is.na(my_virtdb$IDmuni))

my_virt <-(my_virtdb[, c(1, 13) ])

unique(nchar(onto_IFN$Municipio)) #Cuantos caracteres tiene los vectores de la columna municipio

onto_IFN$Municipio <- substr(onto_IFN$Municipio, start = 79, stop = 83)    # Para extraer la subcadena desde el 79 al ultimo caracter

my_virt$IDmuni<-ifelse((nchar(my_virt$IDmuni))<5, 
                       paste0("0",my_virt$IDmuni), 
                       my_virt$IDmuni)
#Poner un 0 delante de los ID que tengan menos de 5 caracteres (pasa de int a character)

unique(nchar(my_virt$IDmuni)) #numero de caracteres 

names (onto_IFN) = c("plot", "IDmuni") #cambiar nombre columnas para que sean los mismos

Datos <-merge(x= onto_IFN, y =my_virt, by = "plot", all.x =T) #Parcelas que tienen asignado municipio (en campo) en la ontologia del IFN
sum(is.na(Datos)) #Datos de parcelas que no tienen coor?


Datos_ok <- sqldf('SELECT * FROM my_virt INTERSECT SELECT * FROM onto_IFN') #Seleccionar filas comunes

Datos_wrong <- sqldf('SELECT * FROM onto_IFN EXCEPT SELECT * FROM Datos_ok')

Datos_wrong <- merge(x = Datos_wrong, y = my_virt, by = "plot", all.x=T)

Datos_ok <- merge(x = Datos_ok, y = my_virt, by = "plot") #Unir Tablas por identificador de parcela

sum(is.na(Datos_wrong$IDmuni.y))

Datos_wrong_c <- merge(x = plots_no_coor, y = Datos_wrong, by = "plot") #Datos NA coinciden con datos de parcelas sin coordenadas

Wrong <- subset(Datos_wrong, is.na(Datos_wrong$IDmuni.y))

Datos_wrong2 <- sqldf('SELECT * FROM Wrong EXCEPT SELECT * FROM Datos_wrong_c') #Esta parcela si tiene coor pero no tiene muni definido

nrow(Datos_wrong_c )

muni <- as.factor(unique(my_virt$IDmuni)) #Municipios que tienen parcelas (6335)

plots_no_coor$tipoPlot <- substr(plots_no_coor$plot, start = 78, stop = 83) 

unique(nchar(plots_no_coor$plot))

unique(plots_no_coor$tipoPlot)
