library(dplyr)

###################################################################################################
######################################### Especies/ Nº pies/ha ####################################
###################################################################################################

#Cargar datos 
pies_ha <- read.csv("N.csv")

#Crear columna del código de la especie extrayendo del IRI de la especie
pies_ha$IDmuni <- substr(pies_ha$muni, start = 79, stop = 83)
pies_ha$SP <- substr(pies_ha$species, start = 69, stop = 70)

#Quedarnos con las columas del Código del municipio la media de N y la especie
pies_ha2 <-pies_ha[, c(1, 4,5,3)]

#Cambio del código de la especie a su nombre científico
pies_ha2$Nombre_SP <- ifelse(pies_ha2$SP == 7, "Acacia spp.",
                                  ifelse(pies_ha2$SP == 8, "Phillyrea latifolia",
                                         ifelse(pies_ha2$SP == 15, "Crataegus spp.", 
                                                ifelse(pies_ha2$SP == 18, "Chamaecyparis lawsoniana",
                                                       ifelse(pies_ha2$SP == 19, "Otras coníferas",
                                                              ifelse(pies_ha2$SP == 21, "Pinus sylvestris",
                                                                     ifelse(pies_ha2$SP == 22, "Pinus uncinata", 
                                                                            ifelse(pies_ha2$SP == 23, "Pinus pinea",
                                                                                   ifelse(pies_ha2$SP == 24, "Pinus halepensis",
                                                                                          ifelse(pies_ha2$SP == 25, "Pinus nigra",
                                                                                                 ifelse(pies_ha2$SP == 26, "Pinus pinaster",
                                                                                                        ifelse(pies_ha2$SP == 27, "Pinus canariensis",
                                                                                                               ifelse(pies_ha2$SP == 28, "Pinus radiata",
                                                                                                                      ifelse(pies_ha2$SP == 29, "Otros pinos",
                                                                                                                             ifelse(pies_ha2$SP == 31, "Abies alba",
                                                                                                                                    ifelse(pies_ha2$SP == 32, "Larix spp.",
                                                                                                                                           ifelse(pies_ha2$SP == 34, "Pseudotsuga menziesii",
                                                                                                                                                  ifelse(pies_ha2$SP == 35, "Otros pinos",
                                                                                                                                                         ifelse(pies_ha2$SP == 36, "Cupressus sempervirens", 
                                                                                                                                                                ifelse(pies_ha2$SP == 38, "Juniperus thurifera", 
                                                                                                                                                                       ifelse(pies_ha2$SP == 39, "Juniperus phoenicea", 
                                                                                                                                                                              ifelse(pies_ha2$SP == 41, "Quercus robur", 
                                                                                                                                                                                     ifelse(pies_ha2$SP == 42, "Quercus petraea", 
                                                                                                                                                                                            ifelse(pies_ha2$SP == 43, "Quercus pyrenaica", 
                                                                                                                                                                                                   ifelse(pies_ha2$SP == 44, "Quercus faginea", 
                                                                                                                                                                                                          ifelse(pies_ha2$SP == 45, "Quercus ilex", 
                                                                                                                                                                                                                 ifelse(pies_ha2$SP == 46, "Quercus suber",
                                                                                                                                                                                                                        ifelse(pies_ha2$SP == 47, "Quercus canariensis",
                                                                                                                                                                                                                               ifelse(pies_ha2$SP == 48, "Quercus rubra", 
                                                                                                                                                                                                                                      ifelse(pies_ha2$SP == 50, "Mezcla de árboles de ribera", 
                                                                                                                                                                                                                                             ifelse(pies_ha2$SP == 51, "Populus alba", 
                                                                                                                                                                                                                                                    ifelse(pies_ha2$SP == 53, "Tamarix spp.", 
                                                                                                                                                                                                                                                           ifelse(pies_ha2$SP == 54, "Alnus glutinosa",
                                                                                                                                                                                                                                                                  ifelse(pies_ha2$SP == 55, "Fraxinus angustifolia",
                                                                                                                                                                                                                                                                         ifelse(pies_ha2$SP == 57, "Salix spp.",
                                                                                                                                                                                                                                                                                ifelse(pies_ha2$SP == 58, "Populus nigra", 
                                                                                                                                                                                                                                                                                       pies_ha2$SP))))))))))))))))))))))))))))))))))))

pies_ha2$Nombre_SP <- ifelse(pies_ha2$SP == 60, "Mezcla de eucaliptos",
                                  ifelse(pies_ha2$SP == 61, "Eucalyptus globulus",
                                         ifelse(pies_ha2$SP == 62, "Eucalyptus camaldulensis", 
                                                ifelse(pies_ha2$SP == 64, "Eucalyptus nitens",
                                                       ifelse(pies_ha2$SP == 65, "Ilex aquifolium",
                                                              ifelse(pies_ha2$SP == 66, "Olea europaea",
                                                                     ifelse(pies_ha2$SP == 67, "Ceratonia siliqua",
                                                                            ifelse(pies_ha2$SP == 68, "Arbutus unedo",
                                                                                   ifelse(pies_ha2$SP == 70, "Mezcla de frondosas de gran porte", 
                                                                                          ifelse(pies_ha2$SP == 71, "Fagus sylvatica",
                                                                                                 ifelse(pies_ha2$SP == 72, "Castanea sativa", 
                                                                                                        ifelse(pies_ha2$SP == 73, "Betula spp.", 
                                                                                                               ifelse(pies_ha2$SP == 74, "Corylus avellana",
                                                                                                                      ifelse(pies_ha2$SP == 76, "Acer campestre",
                                                                                                                             ifelse(pies_ha2$SP == 77, "Tilia spp.",
                                                                                                                                    ifelse(pies_ha2$SP == 78, "Sorbus spp.", 
                                                                                                                                           ifelse(pies_ha2$SP == 81, "Myrica faya",
                                                                                                                                                  ifelse(pies_ha2$SP == 82, "Ilex canariensis",
                                                                                                                                                         ifelse(pies_ha2$SP == 83, "Erica arborea",
                                                                                                                                                                ifelse(pies_ha2$SP == 83, "Erica arborea", 
                                                                                                                                                                ifelse(pies_ha2$SP == 84, "Persea indica", 
                                                                                                                                                                       ifelse(pies_ha2$SP == 89, "Otras laurisilvas", 
                                                                                                                                                                       ifelse(pies_ha2$SP == 90, "Mezcla de pequeñas frondosas",
                                                                                                                                                                              ifelse(pies_ha2$SP == 91, "Buxus sempervirens",
                                                                                                                                                                                     ifelse(pies_ha2$SP == 92, "Robinia pseudacacia",
                                                                                                                                                                                            ifelse(pies_ha2$SP == 93, "Pistacia terebinthus",
                                                                                                                                                                                                   ifelse(pies_ha2$SP == 95, "Prunus spp.", 
                                                                                                                                                                                                          ifelse(pies_ha2$SP == 97, "Sambucus nigra",
                                                                                                                                                                                                                 ifelse(pies_ha2$SP == 98, "Carpinus betulus",
                                                                                                                                                                                                                        ifelse(pies_ha2$SP == 99, "Otras frondosas", 
                                                                                                                                                                                                                               pies_ha2$Nombre_SP))))))))))))))))))))))))))))))
#Comprobar que todos los números estan cambiados a nombre especie
unique(pies_ha2$Nombre_SP) 

#Crear otra tabla con la especie dominante por municipio 
pies_ha2_max<- pies_ha2 %>% 
  group_by(IDmuni) %>% 
  top_n(n = 1, wt = meanN)

#Guardar tabla resultado en un csv
write.csv(pies_ha2, "pies_ha_max.csv")

###################################################################################################
######################################### Especies/ AB ############################################
###################################################################################################

#Cargar datos 
areaB <- read.csv("G.csv")

#Crear columna del código INE extrayendo del IRI del municipio
areaB$IDmuni <- substr(areaB$muni, start = 79, stop = 83)

#Crear columna del código de la especie extrayendo del IRI de la especie
areaB$SP <- substr(areaB$species, start = 69, stop = 70)

#Quedarnos con las columas del Código del municipio la media de N y la especie
areaB2 <-areaB[, c(1, 4,5,3)]





#Leyenda especies
areaB2$Nombre_SP <- ifelse(areaB2$SP == 7, "Acacia spp.",
                           ifelse(areaB2$SP == 8, "Phillyrea latifolia",
                                  ifelse(areaB2$SP == 15, "Crataegus spp.", 
                                         ifelse(areaB2$SP == 18, "Chamaecyparis lawsoniana",
                                                ifelse(areaB2$SP == 19, "Otras coníferas",
                                                       ifelse(areaB2$SP == 21, "Pinus sylvestris",
                                                              ifelse(areaB2$SP == 22, "Pinus uncinata", 
                                                                     ifelse(areaB2$SP == 23, "Pinus pinea",
                                                                            ifelse(areaB2$SP == 24, "Pinus halepensis",
                                                                                   ifelse(areaB2$SP == 25, "Pinus nigra",
                                                                                          ifelse(areaB2$SP == 26, "Pinus pinaster",
                                                                                                 ifelse(areaB2$SP == 27, "Pinus canariensis",
                                                                                                        ifelse(areaB2$SP == 28, "Pinus radiata",
                                                                                                               ifelse(areaB2$SP == 29, "Otros pinos",
                                                                                                                      ifelse(areaB2$SP == 31, "Abies alba",
                                                                                                                             ifelse(areaB2$SP == 32, "Larix spp.",
                                                                                                                                    ifelse(areaB2$SP == 34, "Pseudotsuga menziesii",
                                                                                                                                           ifelse(areaB2$SP == 35, "Otros pinos",
                                                                                                                                                  ifelse(areaB2$SP == 36, "Cupressus sempervirens", 
                                                                                                                                                         ifelse(areaB2$SP == 38, "Juniperus thurifera", 
                                                                                                                                                                ifelse(areaB2$SP == 39, "Juniperus phoenicea", 
                                                                                                                                                                       ifelse(areaB2$SP == 41, "Quercus robur", 
                                                                                                                                                                              ifelse(areaB2$SP == 42, "Quercus petraea", 
                                                                                                                                                                                     ifelse(areaB2$SP == 43, "Quercus pyrenaica", 
                                                                                                                                                                                            ifelse(areaB2$SP == 44, "Quercus faginea", 
                                                                                                                                                                                                   ifelse(areaB2$SP == 45, "Quercus ilex", 
                                                                                                                                                                                                          ifelse(areaB2$SP == 46, "Quercus suber",
                                                                                                                                                                                                                 ifelse(areaB2$SP == 47, "Quercus canariensis",
                                                                                                                                                                                                                        ifelse(areaB2$SP == 48, "Quercus rubra", 
                                                                                                                                                                                                                               ifelse(areaB2$SP == 50, "Mezcla de árboles de ribera", 
                                                                                                                                                                                                                                      ifelse(areaB2$SP == 51, "Populus alba", 
                                                                                                                                                                                                                                             ifelse(areaB2$SP == 53, "Tamarix spp.", 
                                                                                                                                                                                                                                                    ifelse(areaB2$SP == 54, "Alnus glutinosa",
                                                                                                                                                                                                                                                           ifelse(areaB2$SP == 55, "Fraxinus angustifolia",
                                                                                                                                                                                                                                                                  ifelse(areaB2$SP == 57, "Salix spp.",
                                                                                                                                                                                                                                                                         ifelse(areaB2$SP == 58, "Populus nigra", 
                                                                                                                                                                                                                                                                                areaB2$SP))))))))))))))))))))))))))))))))))))

areaB2$Nombre_SP <- ifelse(areaB2$SP == 60, "Mezcla de eucaliptos",
                           ifelse(areaB2$SP == 61, "Eucalyptus globulus",
                                  ifelse(areaB2$SP == 62, "Eucalyptus camaldulensis", 
                                         ifelse(areaB2$SP == 64, "Eucalyptus nitens",
                                                ifelse(areaB2$SP == 65, "Ilex aquifolium",
                                                       ifelse(areaB2$SP == 66, "Olea europaea",
                                                              ifelse(areaB2$SP == 67, "Ceratonia siliqua",
                                                                     ifelse(areaB2$SP == 68, "Arbutus unedo",
                                                                            ifelse(areaB2$SP == 70, "Mezcla de frondosas de gran porte", 
                                                                                   ifelse(areaB2$SP == 71, "Fagus sylvatica",
                                                                                          ifelse(areaB2$SP == 72, "Castanea sativa", 
                                                                                                 ifelse(areaB2$SP == 73, "Betula spp.", 
                                                                                                        ifelse(areaB2$SP == 74, "Corylus avellana",
                                                                                                               ifelse(areaB2$SP == 76, "Acer campestre",
                                                                                                                      ifelse(areaB2$SP == 77, "Tilia spp.",
                                                                                                                             ifelse(areaB2$SP == 78, "Sorbus spp.", 
                                                                                                                                    ifelse(areaB2$SP == 81, "Myrica faya",
                                                                                                                                           ifelse(areaB2$SP == 82, "Ilex canariensis",
                                                                                                                                                  ifelse(areaB2$SP == 83, "Erica arborea", 
                                                                                                                                                         ifelse(areaB2$SP == 84, "Persea indica", 
                                                                                                                                                                ifelse(areaB2$SP == 89, "Otras laurisilvas", 
                                                                                                                                                                       ifelse(areaB2$SP == 90, "Mezcla de pequeñas frondosas",
                                                                                                                                                                              ifelse(areaB2$SP == 91, "Buxus sempervirens",
                                                                                                                                                                                     ifelse(areaB2$SP == 92, "Robinia pseudacacia",
                                                                                                                                                                                            ifelse(areaB2$SP == 93, "Pistacia terebinthus",
                                                                                                                                                                                                   ifelse(areaB2$SP == 95, "Prunus spp.", 
                                                                                                                                                                                                          ifelse(areaB2$SP == 97, "Sambucus nigra",
                                                                                                                                                                                                                 ifelse(areaB2$SP == 98, "Carpinus betulus",
                                                                                                                                                                                                                        ifelse(areaB2$SP == 99, "Otras frondosas", 
                                                                                                                                                                                                                               areaB2$Nombre_SP)))))))))))))))))))))))))))))

unique(areaB2$Nombre_SP) 

#Crear otra tabla con la especie dominante por municipio 
areaB2_max<- areaB2 %>% 
  group_by(IDmuni) %>% 
  top_n(n = 1, wt = meanG)

#Guardar tabla resultado en un csv
write.csv(areaB2_max, "area_basimetrica_max.csv")


###################################################################################################
################################### Sierra de la Demanda ##########################################
###################################################################################################

#Cargar datos municipios de la Sierra
Sierra <- read.csv("Sierra de la demanda/Parcelas/Sierra_demanda.csv")

#Homogeneizar código INE del municipio
Sierra$IDmuni <- ifelse(nchar(Sierra$IDmuni) < 5, paste0(0, Sierra$IDmuni), Sierra$IDmuni)

#Unir la tabla de los municipios con la tabla con la media de N por municipio y especie
Sierra_N <- merge(Sierra, pies_ha2, by = "IDmuni")

#Extraer columnas de: el código del municipio, la especie y el N media
Sierra_N2 <- Sierra_N[, c(1, 18, 21, 20)]

#Crear una tabla con la especie dominante de cada municipio
Sierra_N2_D <- Sierra_N2 %>% 
  group_by(IDmuni) %>% 
  top_n(n = 1, wt = meanN)

#Crear una tabla con las especies de cada municipio que tengan un porcentaje de N >= 40%
Sierra_N2_40 <- Sierra_N2 %>% 
  group_by(IDmuni) %>% 
  mutate(porcentaje = meanN/sum(meanN)*100) %>% 
  filter(porcentaje >= 40) 

#Guardar tablas resultados en un csv
write.csv(Sierra_N2_D, "Sierra de la demanda/Parcelas/Sierra_N_max.csv")
write.csv(Sierra_N2_40, "Sierra de la demanda/Parcelas/Sierra_N_40.csv")



#Crear una tabla con las especies de cada municipio que tengan un porcentaje de N >= 40%
Sierra_G2_40 <- Sierra_G2 %>% 
  group_by(IDmuni) %>% 
  mutate(porcentaje = meanG/sum(meanG)*100) 

Sierra_pinaster <- Sierra_G2_40 %>% 
  filter(Nombre_SP == "Pinus sylvestris" | Nombre_SP =="Pinus pinaster")

#Guardar tablas resultados en un csv
write.csv(Sierra_G2_D, "Sierra de la demanda/Parcelas/Sierra_G_max.csv")
write.csv(Sierra_G2_40, "Sierra de la demanda/Parcelas/Sierra_G_40.csv")
