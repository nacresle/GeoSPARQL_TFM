#Librerias utilizadas
library(dplyr)

##########################################################################
################### Especies dominantes numero pies por ha ###############
##########################################################################


#Cargar datos 
pies_ha <- read.csv("N.csv")

#Crear columna del código INE extrayendolo del IRI del municipio
pies_ha$IDmuni <- substr(pies_ha$muni, start = 79, stop = 83)

#Crear columna del código de la especie extrayendolo del IRI de la especie
pies_ha$SP <- substr(pies_ha$species, start = 69, stop = 70)

#Sustituir código de especie por nombre científico
pies_ha$Nombre_SP <- ifelse(pies_ha$SP == 7, "Acacia spp.",
                      ifelse(pies_ha$SP == 8, "Phillyrea latifolia",
                      ifelse(pies_ha$SP == 15, "Crataegus spp.", 
                      ifelse(pies_ha$SP == 18, "Chamaecyparis lawsoniana",
                      ifelse(pies_ha$SP == 19, "Otras coníferas",
                      ifelse(pies_ha$SP == 21, "Pinus sylvestris",
                      ifelse(pies_ha$SP == 22, "Pinus uncinata", 
                      ifelse(pies_ha$SP == 23, "Pinus pinea",
                      ifelse(pies_ha$SP == 24, "Pinus halepensis",
                      ifelse(pies_ha$SP == 25, "Pinus nigra",
                      ifelse(pies_ha$SP == 26, "Pinus pinaster",
                      ifelse(pies_ha$SP == 27, "Pinus canariensis",
                      ifelse(pies_ha$SP == 28, "Pinus radiata",
                      ifelse(pies_ha$SP == 29, "Otros pinos",
                      ifelse(pies_ha$SP == 31, "Abies alba",
                      ifelse(pies_ha$SP == 32, "Larix spp.",
                      ifelse(pies_ha$SP == 34, "Pseudotsuga menziesii",
                      ifelse(pies_ha$SP == 35, "Otros pinos",
                      ifelse(pies_ha$SP == 36, "Cupressus sempervirens", 
                      ifelse(pies_ha$SP == 38, "Juniperus thurifera", 
                      ifelse(pies_ha$SP == 39, "Juniperus phoenicea", 
                      ifelse(pies_ha$SP == 41, "Quercus robur", 
                      ifelse(pies_ha$SP == 42, "Quercus petraea", 
                      ifelse(pies_ha$SP == 43, "Quercus pyrenaica", 
                      ifelse(pies_ha$SP == 44, "Quercus faginea", 
                      ifelse(pies_ha$SP == 45, "Quercus ilex", 
                      ifelse(pies_ha$SP == 46, "Quercus suber",
                      ifelse(pies_ha$SP == 47, "Quercus canariensis",
                      ifelse(pies_ha$SP == 48, "Quercus rubra", 
                      ifelse(pies_ha$SP == 50, "Mezcla de árboles de ribera", 
                      ifelse(pies_ha$SP == 51, "Populus alba", 
                      ifelse(pies_ha$SP == 53, "Tamarix spp.", 
                      ifelse(pies_ha$SP == 54, "Alnus glutinosa",
                      ifelse(pies_ha$SP == 55, "Fraxinus angustifolia",
                      ifelse(pies_ha$SP == 57, "Salix spp.",
                      ifelse(pies_ha$SP == 58, "Populus nigra", 
                      pies_ha$SP))))))))))))))))))))))))))))))))))))

pies_ha$Nombre_SP <- ifelse(pies_ha$SP == 60, "Mezcla de eucaliptos",
                      ifelse(pies_ha$SP == 61, "Eucalyptus globulus",
                      ifelse(pies_ha$SP == 62, "Eucalyptus camaldulensis", 
                      ifelse(pies_ha$SP == 64, "Eucalyptus nitens",
                      ifelse(pies_ha$SP == 65, "Ilex aquifolium",
                      ifelse(pies_ha$SP == 66, "Olea europaea",
                      ifelse(pies_ha$SP == 67, "Ceratonia siliqua",
                      ifelse(pies_ha$SP == 68, "Arbutus unedo",
                      ifelse(pies_ha$SP == 70, "Mezcla de frondosas de gran porte", 
                      ifelse(pies_ha$SP == 71, "Fagus sylvatica",
                      ifelse(pies_ha$SP == 72, "Castanea sativa", 
                      ifelse(pies_ha$SP == 73, "Betula spp.", 
                      ifelse(pies_ha$SP == 74, "Corylus avellana",
                      ifelse(pies_ha$SP == 76, "Acer campestre",
                      ifelse(pies_ha$SP == 77, "Tilia spp.",
                      ifelse(pies_ha$SP == 78, "Sorbus spp.", 
                      ifelse(pies_ha$SP == 81, "Myrica faya",
                      ifelse(pies_ha$SP == 82, "Ilex canariensis",
                      ifelse(pies_ha$SP == 83, "Erica arborea",
                      ifelse(pies_ha$SP == 83, "Erica arborea", 
                      ifelse(pies_ha$SP == 84, "Persea indica", 
                      ifelse(pies_ha$SP == 89, "Otras laurisilvas", 
                      ifelse(pies_ha$SP == 90, "Mezcla de pequenas frondosas",
                      ifelse(pies_ha$SP == 91, "Buxus sempervirens",
                      ifelse(pies_ha$SP == 92, "Robinia pseudacacia",
                      ifelse(pies_ha$SP == 93, "Pistacia terebinthus",
                      ifelse(pies_ha$SP == 95, "Prunus spp.", 
                      ifelse(pies_ha$SP == 97, "Sambucus nigra",
                      ifelse(pies_ha$SP == 98, "Carpinus betulus",
                      ifelse(pies_ha$SP == 99, "Otras frondosas", 
                      pies_ha$Nombre_SP))))))))))))))))))))))))))))))
                                                                                                          
#Quedarnos con las columas del Código del municipio, la media de N y el código y
#el nombre científico de la especie
pies_ha2 <-pies_ha[, c(1, 4,5,3)]

#Crear otra tabla con la especie dominante por municipio 
pies_ha2_max<- pies_ha2 %>% 
  group_by(IDmuni) %>% 
  top_n(n = 1, wt = meanN)

#Guardar tabla resultado en un csv
write.csv(pies_ha2, "pies_ha_max.csv")

##########################################################################
############## Especies dominantes área basimétrica  #####################
##########################################################################

#Cargar datos 
areaB <- read.csv("G.csv")

#Crear columna del código INE extrayendolo del IRI del municipio
areaB$IDmuni <- substr(areaB$muni, start = 79, stop = 83)

#Crear columna del código de la especie extrayendolo del IRI de la especie
areaB$SP <- substr(areaB$species, start = 69, stop = 70)

#Sustituir código de especie por nombre científico
areaB$Nombre_SP <- ifelse(areaB$SP == 7, "Acacia spp.",
                    ifelse(areaB$SP == 8, "Phillyrea latifolia",
                    ifelse(areaB$SP == 15, "Crataegus spp.", 
                    ifelse(areaB$SP == 18, "Chamaecyparis lawsoniana",
                    ifelse(areaB$SP == 19, "Otras coníferas",
                    ifelse(areaB$SP == 21, "Pinus sylvestris",
                    ifelse(areaB$SP == 22, "Pinus uncinata", 
                    ifelse(areaB$SP == 23, "Pinus pinea",
                    ifelse(areaB$SP == 24, "Pinus halepensis",
                    ifelse(areaB$SP == 25, "Pinus nigra",
                    ifelse(areaB$SP == 26, "Pinus pinaster",
                    ifelse(areaB$SP == 27, "Pinus canariensis",
                    ifelse(areaB$SP == 28, "Pinus radiata",
                    ifelse(areaB$SP == 29, "Otros pinos",
                    ifelse(areaB$SP == 31, "Abies alba",
                    ifelse(areaB$SP == 32, "Larix spp.",
                    ifelse(areaB$SP == 34, "Pseudotsuga menziesii",
                    ifelse(areaB$SP == 35, "Otros pinos",
                    ifelse(areaB$SP == 36, "Cupressus sempervirens", 
                    ifelse(areaB$SP == 38, "Juniperus thurifera", 
                    ifelse(areaB$SP == 39, "Juniperus phoenicea", 
                    ifelse(areaB$SP == 41, "Quercus robur", 
                    ifelse(areaB$SP == 42, "Quercus petraea", 
                    ifelse(areaB$SP == 43, "Quercus pyrenaica", 
                    ifelse(areaB$SP == 44, "Quercus faginea", 
                    ifelse(areaB$SP == 45, "Quercus ilex", 
                    ifelse(areaB$SP == 46, "Quercus suber",
                    ifelse(areaB$SP == 47, "Quercus canariensis",
                    ifelse(areaB$SP == 48, "Quercus rubra", 
                    ifelse(areaB$SP == 50, "Mezcla de árboles de ribera", 
                    ifelse(areaB$SP == 51, "Populus alba", 
                    ifelse(areaB$SP == 53, "Tamarix spp.", 
                    ifelse(areaB$SP == 54, "Alnus glutinosa",
                    ifelse(areaB$SP == 55, "Fraxinus angustifolia",
                    ifelse(areaB$SP == 57, "Salix spp.",
                    ifelse(areaB$SP == 58, "Populus nigra", 
                     areaB$SP))))))))))))))))))))))))))))))))))))

areaB2$Nombre_SP <- ifelse(areaB$SP == 60, "Mezcla de eucaliptos",
                     ifelse(areaB$SP == 61, "Eucalyptus globulus",
                     ifelse(areaB$SP == 62, "Eucalyptus camaldulensis", 
                     ifelse(areaB$SP == 64, "Eucalyptus nitens",
                     ifelse(areaB$SP == 65, "Ilex aquifolium",
                     ifelse(areaB$SP == 66, "Olea europaea",
                     ifelse(areaB$SP == 67, "Ceratonia siliqua",
                     ifelse(areaB$SP == 68, "Arbutus unedo",
                     ifelse(areaB$SP == 70, "Mezcla de frondosas de gran porte", 
                     ifelse(areaB$SP == 71, "Fagus sylvatica",
                     ifelse(areaB$SP == 72, "Castanea sativa", 
                     ifelse(areaB$SP == 73, "Betula spp.", 
                     ifelse(areaB$SP == 74, "Corylus avellana",
                     ifelse(areaB$SP == 76, "Acer campestre",
                     ifelse(areaB$SP == 77, "Tilia spp.",
                     ifelse(areaB$SP == 78, "Sorbus spp.", 
                     ifelse(areaB$SP == 81, "Myrica faya",
                     ifelse(areaB$SP == 82, "Ilex canariensis",
                     ifelse(areaB$SP == 83, "Erica arborea", 
                     ifelse(areaB$SP == 84, "Persea indica", 
                     ifelse(areaB$SP == 89, "Otras laurisilvas", 
                     ifelse(areaB$SP == 90, "Mezcla de pequenas frondosas",
                     ifelse(areaB$SP == 91, "Buxus sempervirens",
                     ifelse(areaB$SP == 92, "Robinia pseudacacia",
                     ifelse(areaB$SP == 93, "Pistacia terebinthus",
                     ifelse(areaB$SP == 95, "Prunus spp.", 
                     ifelse(areaB$SP == 97, "Sambucus nigra",
                     ifelse(areaB$SP == 98, "Carpinus betulus",
                     ifelse(areaB$SP == 99, "Otras frondosas", 
                      areaB$Nombre_SP)))))))))))))))))))))))))))))


#Quedarnos con las columas del Código del municipio la media de N y la especie (Código 
# y nombre científico)
areaB2 <-areaB[, c(1, 4,5,3)]

#Crear otra tabla con la especie dominante por municipio 
areaB2_max<- areaB2 %>% 
  group_by(IDmuni) %>% 
  top_n(n = 1, wt = meanG)

#Guardar tabla resultado en un csv
write.csv(areaB2_max, "area_basimetrica_max.csv")
