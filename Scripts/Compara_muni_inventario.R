####################################################################################################
############################################### Librerias ##########################################
####################################################################################################

#install.packages("sqldf")
library(sqldf) #Consultas SQL

#install.packages("dyplyr")
library(dplyr) #Manipulación de datos

####################################################################################################
################################### Datos Asignación FUSEKI ########################################
####################################################################################################

#Cargar datos
Asig_FUSEKI <- read.csv("Plot_muni_TODOS.csv")

#Substraer de las IRI de Parcela y Municipio los códigos de Provincia, Estadillo, Clase, Subclase y Código INE
Asig_FUSEKI$Provincia <- substr(Asig_FUSEKI$plot, start = 71, stop = 72)
Asig_FUSEKI$Estadillo <- substr(Asig_FUSEKI$plot, start = 74, stop = 77)
Asig_FUSEKI$Clase_PCPar <- substr(Asig_FUSEKI$plot, start = 79, stop = 79)
Asig_FUSEKI$Subclase <- substr(Asig_FUSEKI$plot, start = 81, stop = 83)
Asig_FUSEKI$INE <- substr(Asig_FUSEKI$muni, start = 79, stop = 85)
Asig_FUSEKI$INE3 <- substr(Asig_FUSEKI$INE, start = 3, stop = 5)

####################################################################################################
################################### Datos Asignación QGIS ##########################################
####################################################################################################

#Cargar datos
Asig_QGIS <- read.csv("Plot_muni_QGIS.csv")

#Substraer columnas que me interesan y cambiar nombre
Asig_QGIS$Provincia <- substr(Asig_QGIS$plot, start = 71, stop = 72)
Asig_QGIS$Estadillo <- substr(Asig_QGIS$plot, start = 74, stop = 77)
Asig_QGIS$Clase_PCPar <- substr(Asig_QGIS$plot, start = 79, stop = 79)
Asig_QGIS$Subclase <- substr(Asig_QGIS$plot, start = 81, stop = 83)

Asig_QGIS$IDmuni<-ifelse((nchar(Asig_QGIS$IDmuni))<5, 
                           paste0("0", Asig_QGIS$IDmuni), 
                           Asig_QGIS$IDmuni)

Asig_QGIS <- Asig_QGIS[, c(14:17, 13)]

names(Asig_QGIS) <- c("Provincia", "Estadillo", "Clase_PCPar", "Subclase", "INE_QGIS")

####################################################################################################
################################ Comparar asignaciones FUSEKI y QGIS ###############################
####################################################################################################

#Unir las dos tablas
Fuseki_QGIS <- merge(Asig_FUSEKI, Asig_QGIS, by= c("Provincia", "Estadillo", "Clase_PCPar", "Subclase"))

#Comparar código INE asignado por FUSEKI y por QGIS
Comp_F_Q <- subset(Fuseki_QGIS, Fuseki_QGIS$INE == Fuseki_QGIS$INE_QGIS) #100% coincidencias

####################################################################################################
################################### Datos tabla IFN3 PCDatosMap ####################################
####################################################################################################

#Cargar datos
PCDatosMap <- read.csv("pcdatosmap.csv")

#Extraer solo columnas que me interesan
PCDatosMap <- PCDatosMap[, c(2:4, 7, 8, 9)]

#Homogeneizar datos Provincia, Estadillo y Código INE
PCDatosMap$Provincia<-ifelse((nchar(PCDatosMap$Provincia))<2, 
                          paste0("0", PCDatosMap$Provincia), 
                          PCDatosMap$Provincia)
PCDatosMap$Estadillo<-ifelse((nchar(PCDatosMap$Estadillo))<4, 
                             paste0("0", PCDatosMap$Estadillo), 
                             PCDatosMap$Estadillo)
PCDatosMap$Estadillo<-ifelse((nchar(PCDatosMap$Estadillo))<4, 
                             paste0("0", PCDatosMap$Estadillo), 
                             PCDatosMap$Estadillo)
PCDatosMap$Estadillo<-ifelse((nchar(PCDatosMap$Estadillo))<4, 
                             paste0("0", PCDatosMap$Estadillo), 
                             PCDatosMap$Estadillo)
PCDatosMap$INE3<-ifelse( nchar(PCDatosMap$INE)==5, substr(PCDatosMap$INE, start = 3, stop = 5), PCDatosMap$INE)
PCDatosMap$INE3<-ifelse ( nchar(PCDatosMap$INE) ==4, substr(PCDatosMap$INE, start = 2, stop = 4), PCDatosMap$INE3)
PCDatosMap$INE3<-ifelse ( nchar(PCDatosMap$INE)==2, paste0("0", PCDatosMap$INE) , PCDatosMap$INE3)
PCDatosMap$INE3<-ifelse ( nchar(PCDatosMap$INE) ==1, paste0("00", PCDatosMap$INE), PCDatosMap$INE3)

#Número de Parcelas con código INE de municipio
sum((!is.na(PCDatosMap$INE3) & PCDatosMap$INE3 != "000")) #77 212 tienen codigo INE
sum(is.na(PCDatosMap$INE3)) #19 184 NA's en codigo municipio

#Corrección de discrepancias
PCDatosMap[c(32966, 61044, 61070), 3] <- "A"

####################################################################################################
################################### Datos tabla IFN3 PCParcelas ####################################
####################################################################################################

#Cargar datos
PCParcelas <- read.csv("pcparcelas.csv")

#Extraer columnas que me interesan
PCParcelas <- PCParcelas[,c(2:5, 6, 7, 16)]

#Cambiar nombre columnas par que coincidan con la otra tabla
names(PCParcelas) <- c ("Provincia", "Estadillo", "Clase", "Subclase", "CoorX", "CoorY", "INE")

#Homogeneizar códigos de Provincia, Estadillo y Código INE
PCParcelas$INE <- as.numeric(PCParcelas$INE)
PCParcelas$Provincia<-ifelse((nchar(PCParcelas$Provincia))<2, 
                             paste0("0", PCParcelas$Provincia), 
                             PCParcelas$Provincia)
PCParcelas$Estadillo<-ifelse((nchar(PCParcelas$Estadillo))<4, 
                             paste0("0", PCParcelas$Estadillo), 
                             PCParcelas$Estadillo)
PCParcelas$Estadillo<-ifelse((nchar(PCParcelas$Estadillo))<4, 
                             paste0("0", PCParcelas$Estadillo), 
                             PCParcelas$Estadillo)
PCParcelas$Estadillo<-ifelse((nchar(PCParcelas$Estadillo))<4, 
                             paste0("0", PCParcelas$Estadillo), 
                             PCParcelas$Estadillo)
PCParcelas$INE3<-ifelse ( nchar(PCParcelas$INE) == 4, substr(PCParcelas$INE, start = 2, stop = 4), PCParcelas$INE)
PCParcelas$INE3<-ifelse ( nchar(PCParcelas$INE)==2, paste0("0", PCParcelas$INE) , PCParcelas$INE3)
PCParcelas$INE3<-ifelse ( nchar(PCParcelas$INE) ==1, paste0("00", PCParcelas$INE), PCParcelas$INE3)

#Número de parcelas con código INE de muncipio
sum((!is.na(PCParcelas$INE) & PCParcelas$INE3 != "000")) #46 443 tienen código INE
sum((is.na(PCParcelas$INE) | PCParcelas$INE3 == "000")) #52 603 no tienen código INE

#Corrección de errores
PCParcelas$Estadillo <- ifelse(PCParcelas$Estadillo == 9999, 999, PCParcelas$Estadillo)

####################################################################################################
###################### Unión de las dos tablas del IFN3 y comparación Código INE ###################
####################################################################################################

#Unión ambas tablas por las columnas de Provincia y Estadillo
Parcelas_IFN3 <- merge(PCDatosMap, PCParcelas, by= c("Provincia", "Estadillo"))  
summary(Parcelas_IFN3)

#cuantas parcelas tienen dato de INE y que ese dato no sea 000
sum((!is.na(Parcelas_IFN3$INE3.x) & Parcelas_IFN3$INE3.x != "000")) #80.386 tienen codigo INE en PCDatosMap
sum((!is.na(Parcelas_IFN3$INE3.y) & Parcelas_IFN3$INE3.y != "000")) #46.443 tienen codigo INE en PCParcelas

#Cambiar el nombre de las columnas para entender la tabla mejor
names (Parcelas_IFN3) <- c("Provincia", "Estadillo", "Clase_PCMap", "CoorX_PCMap", "CoorY_PCMap", "INE_PCMap", "INE3_PCMap", "Clase_PCPar",
                           "Subclase", "CoorX_PCPar", "CoorY_PCPar", "INE_PCPar", "INE3_PCPar")


#Parcelas con mismo código INE en PCDatosMap y PCParcelas
INE_igual <- subset(Parcelas_IFN3, ((Parcelas_IFN3$INE3_PCMap == Parcelas_IFN3$INE3_PCPar) & Parcelas_IFN3$INE3_PCPar != "000")) #45 670 iguales

#Parcelas con distinto código INE en PCDatosMap y PCParcelas
INE_dist <- subset(Parcelas_IFN3, Parcelas_IFN3$INE3_PCMap != Parcelas_IFN3$INE3_PCPar) #773 distintas

#Parcelas sin código INE en algunos de los dos
sum(is.na(Parcelas_IFN3$INE3_PCMap) | is.na(Parcelas_IFN3$INE3_PCPar)) #52 783

####################################################################################################
######### Comparar Código INE de las parcelas del IFN3 con la asignación realizada con FUSEKI ######
####################################################################################################

#Unir tabla Parcelas_IFN3 con Asig_FUSEKI
merge_PCpar <- merge(Parcelas_IFN3, Asig_FUSEKI, by= c("Provincia", "Estadillo", "Clase_PCPar", "Subclase")) 

#Actualizar código INE PCMap
merge_PCpar$INE3_PCMap <- ifelse((merge_PCpar$INE3_PCMap ==128 & merge_PCpar$INE ==10902), 902, 
                               ifelse((merge_PCpar$INE3_PCMap ==180 & merge_PCpar$INE == 10904), 904, 
                                      ifelse((merge_PCpar$INE3_PCMap ==180 & merge_PCpar$INE == 10905), 905,
                                             ifelse(((merge_PCpar$INE3_PCMap == "026" | merge_PCpar$INE3_PCMap == "063" ) & merge_PCpar$INE == 15902), 902, 
                                                    ifelse(((merge_PCpar$INE3_PCMap == "011" | merge_PCpar$INE3_PCMap == "012" ) & merge_PCpar$INE == 36902), 902, 
                                                           ifelse((merge_PCpar$INE3_PCMap == "013" & merge_PCpar$INE == 38901), 901,
                                                                  ifelse((merge_PCpar$INE3_PCMap == "015" & merge_PCpar$INE == "06902"), 902,
                                                                         ifelse((merge_PCpar$INE3_PCMap == "060" & merge_PCpar$INE == 48915), 915, 
                                                                                ifelse((merge_PCpar$INE3_PCMap == 105 & merge_PCpar$INE == 18915), 915,
                                                                                       merge_PCpar$INE3_PCMap)))))))))
#Actualizar Código INE PCPar
merge_PCpar$INE3_PCPar <- ifelse((merge_PCpar$INE3_PCPar ==128 & merge_PCpar$INE ==10902), 902, 
                                 ifelse((merge_PCpar$INE3_PCPar ==180 & merge_PCpar$INE == 10904), 904, 
                                        ifelse((merge_PCpar$INE3_PCPar ==180 & merge_PCpar$INE == 10905), 905,
                                               ifelse(((merge_PCpar$INE3_PCPar == "026" | merge_PCpar$INE3_PCPar == "063" ) & merge_PCpar$INE == 15902), 902, 
                                                      ifelse(((merge_PCpar$INE3_PCPar == "011" | merge_PCpar$INE3_PCPar == "012" ) & merge_PCpar$INE == 36902), 902, 
                                                             ifelse((merge_PCpar$INE3_PCPar == "013" & merge_PCpar$INE == 38901), 901,
                                                                    ifelse((merge_PCpar$INE3_PCPar == "015" & merge_PCpar$INE == "06902"), 902,
                                                                           ifelse((merge_PCpar$INE3_PCPar == "060" & merge_PCpar$INE == 48915), 915, 
                                                                                  ifelse((merge_PCpar$INE3_PCPar == 105 & merge_PCpar$INE == 18915), 915,
                                                                                         merge_PCpar$INE3_PCPar)))))))))


#Comparar Código INE con datos de PCDatosMap
Asig_PCMap <- subset(merge_PCpar, merge_PCpar$INE3 == merge_PCpar$INE3_PCMap) #44 910 iguales
Asig_PCMap_d <- subset(merge_PCpar, merge_PCpar$INE3 != merge_PCpar$INE3_PCMap) #35 376 distintos

#Comparar Código INE con datos de PCParcelas
Asig_PCPar <- subset(merge_PCpar, merge_PCpar$INE3 == merge_PCpar$INE3_PCPar) #44 910 iguales
Asig_PCPar_d <- subset(merge_PCpar, merge_PCpar$INE3 != merge_PCpar$INE3_PCPar) #5 369 distintos

