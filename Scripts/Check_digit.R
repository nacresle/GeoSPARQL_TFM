#cargar todos los municipios
muni <- read.csv("Muni_ES.csv")

#Pegar un 0 delante de los identificadores de municipio menores de 5 caracteres
muni$IDmuni<-ifelse((nchar(muni$IDmuni))<5, 
                       paste0("0",muni$IDmuni), 
                       muni$IDmuni)

#comprobar que todos los identificadores de municipio tienen 5 caracteres
unique(nchar(muni$IDmuni))

#Dividir los digitos del identificador del municipio
muni$uno <- substr(muni$IDmuni, start = 1, stop = 1)
muni$dos <- substr(muni$IDmuni, start = 2, stop = 2)
muni$tres <- substr(muni$IDmuni, start = 3, stop = 3)
muni$cuatro <- substr(muni$IDmuni, start = 4, stop = 4)
muni$cinco <- substr(muni$IDmuni, start = 5, stop = 5)


#Transformar los digitos segun tabla INE
muni$check1 <- ifelse (muni$uno == 0, 0, 
                      ifelse(muni$uno == 1, 2,
                             ifelse(muni$uno == 2, 4,
                                    ifelse(muni$uno == 3, 6, 
                                           ifelse(muni$uno == 4, 8,
                                                  ifelse(muni$uno == 5, 1,
                                                         ifelse(muni$uno == 6, 3,
                                                                ifelse(muni$uno == 7, 5,
                                                                       ifelse(muni$uno ==8, 7,
                                                                              ifelse(muni$uno == 9, 9, NA))))))))))

muni$check2 <- ifelse (muni$dos == 0, 0, 
                       ifelse(muni$dos == 1, 3,
                              ifelse(muni$dos == 2, 8,
                                     ifelse(muni$dos == 3, 2, 
                                            ifelse(muni$dos == 4, 7,
                                                   ifelse(muni$dos == 5, 4,
                                                          ifelse(muni$dos == 6, 1,
                                                                 ifelse(muni$dos == 7, 5,
                                                                        ifelse(muni$dos ==8, 9,
                                                                               ifelse(muni$dos == 9, 6, NA))))))))))

muni$check3 <- ifelse (muni$tres == 0, 0, 
                       ifelse(muni$tres == 1, 1,
                              ifelse(muni$tres == 2, 2,
                                     ifelse(muni$tres == 3, 3, 
                                            ifelse(muni$tres == 4, 4,
                                                   ifelse(muni$tres == 5, 5,
                                                          ifelse(muni$tres == 6, 6,
                                                                 ifelse(muni$tres == 7, 7,
                                                                        ifelse(muni$tres ==8, 8,
                                                                               ifelse(muni$tres == 9, 9, NA))))))))))


muni$check4 <- ifelse (muni$cuatro == 0, 0, 
                       ifelse(muni$cuatro == 1, 2,
                              ifelse(muni$cuatro == 2, 4,
                                     ifelse(muni$cuatro == 3, 6, 
                                            ifelse(muni$cuatro == 4, 8,
                                                   ifelse(muni$cuatro == 5, 1,
                                                          ifelse(muni$cuatro == 6, 3,
                                                                 ifelse(muni$cuatro == 7, 5,
                                                                        ifelse(muni$cuatro ==8, 7,
                                                                               ifelse(muni$cuatro == 9, 9, NA))))))))))

muni$check5 <- ifelse (muni$cinco == 0, 0, 
                       ifelse(muni$cinco == 1, 3,
                              ifelse(muni$cinco == 2, 8,
                                     ifelse(muni$cinco == 3, 2, 
                                            ifelse(muni$cinco == 4, 7,
                                                   ifelse(muni$cinco == 5, 4,
                                                          ifelse(muni$cinco == 6, 1,
                                                                 ifelse(muni$cinco == 7, 5,
                                                                        ifelse(muni$cinco ==8, 9,
                                                                               ifelse(muni$cinco == 9, 6, NA))))))))))  
#sumar los 5 digitos transformados
muni$sum <- rowSums(muni[, 16:20]) 

#Dividir la suma anterior entre 10 excepto si la suma da 0 que se mantiene como 0
muni$div <- ifelse(muni$sum ==0, 0, muni$sum/10)

#Extraer el dividendo de la disvision anterior
muni$res <- substr(muni$div, start = 3, stop = 3)

#Al substraer los números pasan a ser caracteres por lo que los volvemos a pasar a numeros
muni$res <- as.numeric(muni$res) 

#Los que son NA los transformamos en 0
muni$res[is.na(muni$res)] <- 0

#los que son 0 se mantienen como 0 y a los demas se les resta 10
muni$dc <- ifelse(muni$res == 0, 0, (10 - muni$res))

Check_digit_T <- muni[, c(1:10, 24)]

write.csv(Check_digit_T, "Check_digit_Todos.csv")

falsos <- read.csv("Check_digit_53_54.csv")

