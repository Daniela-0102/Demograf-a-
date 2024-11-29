
# Eliminar todos los objetos del entorno

rm(list=ls())
# Cargar dplyr

#install.packages("tidyverse")
#install.packages("pyramid")
#install.packages("dplyr")
library(tidyverse)
library(pyramid)
library(dplyr)
setwd("C:/Users/hello/OneDrive/Escritorio/FC/Demografia")

#### TEMA 1.1: EVALUACION DE LA POBLACION:

# Cargamos las bases de datos: 

Pob2010 <- read.csv("Pob2010.csv", header = TRUE)
Pob2010[is.na(Pob2010)] = 0
##view(Pob2010)

# Se obtiene el índice de Myers para la población de hombres
Mh <- matrix(NA, 10, 1)
for (j in 0:9) {
  Pj1_Hombres <- sum(as.numeric(Pob2010$Hombres[seq(11 + j, 61 + j, by = 10)]))
  Pj2_Hombres <- sum(as.numeric(Pob2010$Hombres[seq(21 + j, 71 + j, by = 10)]))
  Mh[j + 1] <- (j + 1) * Pj1_Hombres + (9 - j) * Pj2_Hombres
}

m2 <- sum(Mh)
Hj_Hombres <- matrix(NA, 10, 1)
for (i in 1:10) {
  Hj_Hombres[i] <- (Mh[i] / m2) - 0.10
}
Hj_Hombres <- Hj_Hombres * 100
Hj_Hombres
# Índice de Myers para hombres (resumido)
Myers_Hombres <- sum(abs(Hj_Hombres))
Myers_Hombres  # Muestra el valor final


# Para la poblacion de mujeres: 

# Reemplazar valores no numéricos con NA
Pob2010$Mujeres <- as.numeric(gsub("[^0-9.]", "", Pob2010$Mujeres))

Mm <- matrix(NA, 10, 1)

for (j in 0:9) {
  Pj1_Mujeres <- sum(Pob2010$Mujeres[seq(11+j, 61+j, by=10)])
  Pj2_Mujeres <- sum(Pob2010$Mujeres[seq(21+j, 71+j, by=10)])
  Mm[j+1] <- (j+1) * Pj1_Mujeres + (9-j) * Pj2_Mujeres
}
m3 <- sum(Mm)
Mj_Mujeres <- matrix(NA, 10, 1)
for (i in 1:10) {
  Mj_Mujeres[i] <- (Mm[i]/m3) - 0.10
}
Mj_Mujeres <- Mj_Mujeres * 100
#view (Mj_Mujeres) #se observa la tabla 

# Índice de Myers para mujeres (resumido):
Myers_Mujeres <- sum(abs(Mj_Mujeres))
Myers_Mujeres

#para poblacion 2020
# Cargamos las bases de datos: 
Pob2020 <- read.csv("Pob2020.csv", header = TRUE)
#view(Pob2020)

Pob2020[is.na(Pob2020)] = 0

# Reemplazar valores no numéricos con NA
# Se obtiene el índice de Myers para la población de hombres
Mh <- matrix(NA, 10, 1)
for (j in 0:9) {
  Pj1_Hombres <- sum(as.numeric(Pob2020$Hombres[seq(11 + j, 61 + j, by = 10)]))
  Pj2_Hombres <- sum(as.numeric(Pob2020$Hombres[seq(21 + j, 71 + j, by = 10)]))
  Mh[j + 1] <- (j + 1) * Pj1_Hombres + (9 - j) * Pj2_Hombres
}

m2 <- sum(Mh)
Hj_Hombres <- matrix(NA, 10, 1)
for (i in 1:10) {
  Hj_Hombres[i] <- (Mh[i] / m2) - 0.10
}
Hj_Hombres <- Hj_Hombres * 100
#view(Hj_Hombres)
# Índice de Myers para hombres (resumido)
Myers_Hombres <- sum(abs(Hj_Hombres))
Myers_Hombres  # Muestra el valor final


# Para la poblacion de mujeres: 

# Reemplazar valores no numéricos con NA
Pob2020$Mujeres <- as.numeric(gsub("[^0-9.]", "", Pob2020$Mujeres))

Mm <- matrix(NA, 10, 1)

for (j in 0:9) {
  Pj1_Mujeres <- sum(Pob2020$Mujeres[seq(11+j, 61+j, by=10)])
  Pj2_Mujeres <- sum(Pob2020$Mujeres[seq(21+j, 71+j, by=10)])
  Mm[j+1] <- (j+1) * Pj1_Mujeres + (9-j) * Pj2_Mujeres
}
m3 <- sum(Mm)
Mj_Mujeres <- matrix(NA, 10, 1)
for (i in 1:10) {
  Mj_Mujeres[i] <- (Mm[i]/m3) - 0.10
}
Mj_Mujeres <- Mj_Mujeres * 100
#view(Mj_Mujeres)
# Índice de Myers para mujeres (resumido):
Myers_Mujeres <- sum(abs(Mj_Mujeres))
Myers_Mujeres



#*****************************1. INDICE DE WHIPPLE **************************************

#para la población 2010
# Cálculo del Índice de Whipple para Hombres
w1 <- sum(Pob2010$Hombres[seq(26, 61, by=5)]) # Suma de las edades terminadas en 0 y 5
w2 <- sum(Pob2010$Hombres[seq(24, 63)])       # Suma total de las edades de 23 a 62
Whipple_Hombres <- (5 * w1 / w2) * 100
Whipple_Hombres

# Cálculo del Índice de Whipple para Mujeres
w3 <- sum(Pob2010$Mujeres[seq(26, 61, by=5)]) # Suma de las edades terminadas en 0 y 5
w4 <- sum(Pob2010$Mujeres[seq(24, 63)])       # Suma total de las edades de 23 a 62
Whipple_Mujeres <- (5 * w3 / w4) * 100
Whipple_Mujeres

#para la población 2020
# Cálculo del Índice de Whipple para Hombres
w1 <- sum(Pob2020$Hombres[seq(26, 61, by=5)]) # Suma de las edades terminadas en 0 y 5
w2 <- sum(Pob2020$Hombres[seq(24, 63)])       # Suma total de las edades de 23 a 62
Whipple_Hombres <- (5 * w1 / w2) * 100
Whipple_Hombres

# Cálculo del Índice de Whipple para Mujeres
w3 <- sum(Pob2020$Mujeres[seq(26, 61, by=5)]) # Suma de las edades terminadas en 0 y 5
w4 <- sum(Pob2020$Mujeres[seq(24, 63)])       # Suma total de las edades de 23 a 62
Whipple_Mujeres <- (5 * w3 / w4) * 100
Whipple_Mujeres

######**********      3. INDICE DE NACIONES UNIDAS      **********######
# Función para calcular grupos quinquenales
grupos_quinquenales <- function(x, columna, edad_final){
  extremo_inferior <- seq(0, edad_final, 5)
  extremo_superior <- extremo_inferior + 4
  Grupo <- paste0(extremo_inferior,"-",extremo_superior)
  Grupo <- c(Grupo, paste0(edad_final+1," y mas"))
  
  num_pers <- rep(0, length(extremo_inferior))
  for (i in 1:length(extremo_inferior)) {
    a <- extremo_inferior[i] + 1
    b <- extremo_superior[i] + 1
    num_pers[i] <- sum(x[a:b, columna])
  }
  num_pers <- c(num_pers, sum(x[(edad_final+2):nrow(x), columna]) )
  
  tabla_quin <- data.frame(Grupo, num_pers)
  
  return(tabla_quin)
}

# Función para calcular grupos quinquenales
grupos_quinquenales <- function(x, columna, edad_final){
  extremo_inferior <- seq(0, edad_final, 5)
  extremo_superior <- extremo_inferior + 4
  Grupo <- paste0(extremo_inferior,"-",extremo_superior)
  Grupo <- c(Grupo, paste0(edad_final+1," y mas"))
  
  num_pers <- rep(0, length(extremo_inferior))
  for (i in 1:length(extremo_inferior)) {
    a <- extremo_inferior[i] + 1
    b <- extremo_superior[i] + 1
    num_pers[i] <- sum(x[a:b, columna])
  }
  num_pers <- c(num_pers, sum(x[(edad_final+2):nrow(x), columna]) )
  
  tabla_quin <- data.frame(Grupo, num_pers)
  
  return(tabla_quin)
}

# Cálculo para Hombres
grupos2010_H <- grupos_quinquenales(Pob2010, "Hombres", 74)
grupos2010_H <- grupos2010_H[-nrow(grupos2010_H), ]


sumandos_IH <- rep(0, 13)
for (j in 1:13) {
  sumandos_IH[j] <- abs( (2 * grupos2010_H$num_pers[j+1] / 
                            (grupos2010_H$num_pers[j] + grupos2010_H$num_pers[j+2])) - 1 ) 
}

IH_G <- (sum(sumandos_IH) / 13) * 100
IH_G

# Cálculo para Mujeres
grupos2010_M <- grupos_quinquenales(Pob2010, "Mujeres", 74)
grupos2010_M <- grupos2010_M[-nrow(grupos2010_M), ]

sumandos_IM <- rep(0, 13)
for (k in 1:13) {
  sumandos_IM[k] <- abs( (2 * grupos2010_M$num_pers[k+1] / 
                            (grupos2010_M$num_pers[k] + grupos2010_M$num_pers[k+2])) - 1 ) 
}

IM_G <- sum(sumandos_IM) / 13 * 100
IM_G

# Cálculo para Ambos Sexos (Hombres y Mujeres)
sumandos_IS <- rep(0, 13)
for (l in 1:13) { 
  sumandos_IS[l] <- abs( ( grupos2010_H$num_pers[l+1] / grupos2010_M$num_pers[l+1]) - 
                           (grupos2010_H$num_pers[l+2] / grupos2010_M$num_pers[l+2]) )
}

# Resultado del indice para ambos sexos
IS <- sum(sumandos_IS) / 13 * 100
IS
# Cálculo del INU
INU2010 <- (IH_G + IM_G + IS) / 3
print(INU2010)

# Cálculo para Hombres
grupos2020_H <- grupos_quinquenales(Pob2020, "Hombres", 74)
grupos2020_H <- grupos2020_H[-nrow(grupos2020_H), ]


sumandos_IH <- rep(0, 13)
for (j in 1:13) {
  sumandos_IH[j] <- abs( (2 * grupos2020_H$num_pers[j+1] / 
                            (grupos2020_H$num_pers[j] + grupos2020_H$num_pers[j+2])) - 1 ) 
}

IH_G <- sum(sumandos_IH) / 13 * 100
IH_G

# Cálculo para Mujeres
grupos2020_M <- grupos_quinquenales(Pob2020, "Mujeres", 74)
grupos2020_M <- grupos2020_M[-nrow(grupos2020_M), ]

sumandos_IM <- rep(0, 13)
for (k in 1:13) {
  sumandos_IM[k] <- abs( (2 * grupos2020_M$num_pers[k+1] / 
                            (grupos2020_M$num_pers[k] + grupos2020_M$num_pers[k+2])) - 1 ) 
}

IM_G <- sum(sumandos_IM) / 13 * 100
IM_G

# Cálculo para Ambos Sexos (Hombres y Mujeres)
sumandos_IS <- rep(0, 13)
for (l in 1:13) { 
  sumandos_IS[l] <- abs( ( grupos2020_H$num_pers[l+1] / grupos2020_M$num_pers[l+1]) - 
                           (grupos2020_H$num_pers[l+2] / grupos2020_M$num_pers[l+2]) )
}

# Resultado del indice para ambos sexos
IS <- sum(sumandos_IS) / 13 * 100
IS

# Cálculo del INU
INU2020 <- (IH_G + IM_G + IS) / 3
print(INU2020)


######**********      5. INDICE DE MASCULINIDAD         **********######
#para población 2010
# Agrupar los hombres en grupos quinquenales y renombrar la columna
Hombres2010_grupos <- grupos_quinquenales(Pob2010, "Hombres", 84)
colnames(Hombres2010_grupos)[2] <- "Hombres"

# Agrupar las mujeres en grupos quinquenales y renombrar la columna
Mujeres2010_grupos <- grupos_quinquenales(Pob2010, "Mujeres", 84)
colnames(Mujeres2010_grupos)[2] <- "Mujeres"

# Unir ambos grupos por la columna "Grupo" usando merge()
Poblacion2010_grupos <- cbind(Hombres2010_grupos, Mujeres2010_grupos$Mujeres)
colnames(Poblacion2010_grupos) = c("Edad", "Hombres", "Mujeres")
# Calcular el índice de masculinidad
IMasculinidad_2010 <- Poblacion2010_grupos |>
  transform(I_Masc = Hombres / Mujeres * 100)
IMasculinidad_2010

# Visualizar el resultado
#view(IMasculinidad_2010)

######**********      6. RAZON DE DEPENDENCIA           **********######

# Cálculo de la razón de dependencia para hombres
RDep_Hombres2010 <- (sum(Poblacion2010_grupos$Hombres[c(1:3, 14:18)]) / sum(Poblacion2010_grupos$Hombres[4:13])) * 100

# Cálculo de la razón de dependencia para mujeres
RDep_Mujeres2010 <- (sum(Poblacion2010_grupos$Mujeres[c(1:3, 14:18)]) / sum(Poblacion2010_grupos$Mujeres[4:13])) * 100

# Cálculo de la razón de dependencia total (corrigiendo el denominador)
RDep_Total2010 <- ((sum(Poblacion2010_grupos$Hombres[c(1:3, 14:18)]) + sum(Poblacion2010_grupos$Mujeres[c(1:3, 14:18)])) 
               / (sum(Poblacion2010_grupos$Hombres[4:13]) + sum(Poblacion2010_grupos$Mujeres[4:13]))) * 100

# Visualizar los resultados
RDep_Hombres2010
RDep_Mujeres2010
RDep_Total2010

#para población 2020
# Agrupar los hombres en grupos quinquenales y renombrar la columna
Hombres2020_grupos <- grupos_quinquenales(Pob2020, "Hombres", 84)
colnames(Hombres2020_grupos)[2] <- "Hombres"

# Agrupar las mujeres en grupos quinquenales y renombrar la columna
Mujeres2020_grupos <- grupos_quinquenales(Pob2020, "Mujeres", 84)
colnames(Mujeres2020_grupos)[2] <- "Mujeres"

# Unir ambos grupos por la columna "Grupo" usando merge()
Poblacion2020_grupos <- cbind(Hombres2020_grupos, Mujeres2020_grupos$Mujeres)
colnames(Poblacion2020_grupos) = c("Edad", "Hombres", "Mujeres")

# Calcular el índice de masculinidad
IMasculinidad_2020 <- Poblacion2020_grupos |>
  transform(I_Masc = Hombres / Mujeres * 100)
IMasculinidad_2020

# Visualizar el resultado
view(IMasculinidad_2020)

# Cálculo de la razón de dependencia para hombres
RDep_Hombres2020 <- (sum(Poblacion2020_grupos$Hombres[c(1:3, 14:18)]) / sum(Poblacion2020_grupos$Hombres[4:13])) * 100

# Cálculo de la razón de dependencia para mujeres
RDep_Mujeres2020 <- (sum(Poblacion2020_grupos$Mujeres[c(1:3, 14:18)]) / sum(Poblacion2020_grupos$Mujeres[4:13])) * 100

# Cálculo de la razón de dependencia total (corrigiendo el denominador)
RDep_Total2020 <- ((sum(Poblacion2020_grupos$Hombres[c(1:3, 14:18)]) + sum(Poblacion2020_grupos$Mujeres[c(1:3, 14:18)])) 
               / (sum(Poblacion2020_grupos$Hombres[4:13]) + sum(Poblacion2020_grupos$Mujeres[4:13]))) * 100

# Visualizar los resultados
RDep_Hombres2020
RDep_Mujeres2020
RDep_Total2020


# ************************ 1. PRORRATEO DE LA INFORMACION ********************************
# Calcular el porcentaje de concentración de la población No Especificada para Hombres
Pob_Total_Hombres <- sum(Pob2010$Hombres)  # Total de hombres en todas las edades
Pob_NE_Hombres <- Pob2010$Hombres[length(Pob2010$Edad)]  # Hombres en la categoría "No Especificada"
alfa2010_Hombres = (Pob_NE_Hombres/(Pob_Total_Hombres-Pob_NE_Hombres))  # Proporción de Hombres No Especificados
alfa2010_Hombres

# Calcular el porcentaje de concentración de la población No Especificada para Mujeres
Pob_Total_Mujeres <- sum(Pob2010$Mujeres)  # Total de mujeres en todas las edades
Pob_NE_Mujeres <- Pob2010$Mujeres[length(Pob2010$Edad)]  # Mujeres en la categoría "No Especificada"
alfa2010_Mujeres = (Pob_NE_Mujeres/(Pob_Total_Mujeres-Pob_NE_Mujeres))  # Proporción de Mujeres No Especificadas
alfa2010_Mujeres

Pob2010_Prorrateo <- matrix(NA,length(Pob2010$Edad)-1,3)
for(i in 1:(length(Pob2010$Edad)-1)){
  if(alfa2010_Hombres>=0.1){
    Pob2010_Prorrateo[i,2]=Pob2010$Hombres[i]+Pob2010$Hombres[i]*alfa2010_Hombres
  }
  else{
    Pob2010_Prorrateo[,2]=Pob2010$Hombres[-length(Pob2010$Edad)]
  }
}
for(i in 1:(length(Pob2010$Edad)-1)){
  if(alfa2010_Mujeres>=0.1){
    Pob2010_Prorrateo[i,3]=Pob2010$Mujeres[i]+Pob2010$Mujeres[i]*alfa2010_Mujeres
  }
  else{
    Pob2010_Prorrateo[,3]=Pob2010$Mujeres[-length(Pob2010$Edad)]
  }
}
Pob2010_Prorrateo[,1] <- Pob2010[1:101,1] 
Pob2010_Prorrateo <- as.data.frame(Pob2010_Prorrateo)
colnames(Pob2010_Prorrateo) <- c("Edad", "Hombres", "Mujeres")

# Guardar el prorrateo en un archivo CSV
write.csv(Pob2010_Prorrateo, "Pob2010_Prorrateo.csv", row.names = FALSE)
Correccion2010 <- read.csv("Pob2010_Prorrateo.csv")
#view(Correccion2010)

# ************************ 1. PRORRATEO DE LA INFORMACION2020 ********************************
# Calcular el porcentaje de concentración de la población No Especificada para Hombres
Pob_Total_Hombres <- sum(Pob2020$Hombres)  # Total de hombres en todas las edades
Pob_NE_Hombres <- Pob2020$Hombres[length(Pob2020$Edad)]  # Hombres en la categoría "No Especificada"
alfa2020_Hombres = (Pob_NE_Hombres/(Pob_Total_Hombres-Pob_NE_Hombres))  # Proporción de Hombres No Especificados
alfa2020_Hombres

# Calcular el porcentaje de concentración de la población No Especificada para Mujeres
Pob_Total_Mujeres <- sum(Pob2020$Mujeres)  # Total de mujeres en todas las edades
Pob_NE_Mujeres <- Pob2020$Mujeres[length(Pob2020$Edad)]  # Mujeres en la categoría "No Especificada"
alfa2020_Mujeres = (Pob_NE_Mujeres/(Pob_Total_Mujeres-Pob_NE_Mujeres))  # Proporción de Mujeres No Especificadas
alfa2020_Mujeres

Pob2020_Prorrateo <- matrix(NA,length(Pob2020$Edad)-1,3)
for(i in 1:(length(Pob2020$Edad)-1)){
  if(alfa2020_Hombres>=0.1){
    Pob2020_Prorrateo[,2]=Pob2020$Hombres[i]+Pob2020$Hombres[i]*alfa_Hombres
  }
  else{
    Pob2020_Prorrateo[,2]=Pob2020$Hombres[-length(Pob2020$Edad)]
  }
}
for(i in 1:(length(Pob2020$Edad)-1)){
  if(alfa2020_Mujeres>=0.1){
    Pob2020_Prorrateo[i,3]=Pob2020$Mujeres[i]+Pob2020$Mujeres[i]*alfa2020_Mujeres
  }
  else{
    Pob2020_Prorrateo[,3]=Pob2020$Mujeres[-length(Pob2020$Edad)]
  }
}
Pob2020_Prorrateo[,1] <- Pob2020$Edad[1:101] 
Pob2020_Prorrateo <- as.data.frame(Pob2020_Prorrateo)
colnames(Pob2020_Prorrateo) <- c("Edad", "Hombres", "Mujeres")
# Guardar el prorrateo en un archivo CSV
write.csv(Pob2020_Prorrateo, "Pob2020_Prorrateo.csv", row.names = FALSE)
Correccion2020 <- read.csv("Pob2020_Prorrateo.csv")
#view(Correccion2020)

write.csv(Poblacion2010_grupos, "Poblacion2010_grupos.csv", row.names = FALSE)
# Correccion2010 <- read.csv("Poblacion2010_grupos.csv")

# ************************ 2. METODO DE 1/16 ********************************
# Corrección del objeto 'Correccion2010' basado en 'Poblacion2010_grupos'
Correccion2010 <- Poblacion2010_grupos

# Evitar problemas con los índices fuera de rango en el bucle
for (i in 3:15) {
    Correccion2010[i, 2:3] <- (1/16) * (-1 * Poblacion2010_grupos[i+2, 2:3] + 
                                          4 * Poblacion2010_grupos[i+1, 2:3] + 
                                          10 * Poblacion2010_grupos[i, 2:3] + 
                                          4 * Poblacion2010_grupos[i-1, 2:3] - 
                                          1 * Poblacion2010_grupos[i-2, 2:3])
  }

Correccion2010
#view(Correccion2010)

# Cálculo de la cuadratura y normalización
fac_cuadratura <- Correccion2010
fac_cuadratura$Hombres <- Correccion2010$Hombres / sum(Correccion2010$Hombres)
fac_cuadratura$Mujeres <- Correccion2010$Mujeres / sum(Correccion2010$Mujeres)

# Cuadratura corregida
Pob2010_Prorrateo$Hombres <- as.numeric(gsub("[^0-9]", "", Pob2010_Prorrateo$Hombres))
Pob2010_Prorrateo$Mujeres <- as.numeric(gsub("[^0-9]", "", Pob2010_Prorrateo$Mujeres))
# Pob2010_Prorrateo$Edad <- gsub("\xa0", " ", Pob2010_Prorrateo$Edad)
Correccion2010_cuadratura <- Correccion2010

Correccion2010_cuadratura$Mujeres <- sum(Pob2010_Prorrateo$Mujeres)*fac_cuadratura$Mujeres
Correccion2010_cuadratura$Hombres <- sum(Pob2010_Prorrateo$Hombres)*fac_cuadratura$Hombres

#Corroborar:
sum(Pob2010_Prorrateo$Hombres) == sum(Correccion2010_cuadratura$Hombres)
sum(Pob2010_Prorrateo$Mujeres) == sum(Correccion2010_cuadratura$Mujeres)

# Redondear:
Correccion2010_cuadratura$Hombres <- round(Correccion2010_cuadratura$Hombres)
Correccion2010_cuadratura$Mujeres <- round(Correccion2010_cuadratura$Mujeres)

# Renombrar las filas correctamente (nombres explícitos del rango 1 a 18)
rownames(Correccion2010_cuadratura) <- seq(1, nrow(Correccion2010_cuadratura))

# Mostrar la tabla
#view(Correccion2010_cuadratura)

write.csv(Correccion2010_cuadratura, "Correccion2010_cuadratura.csv", row.names = FALSE)
Correccion2010 <- read.csv("Correccion2010_cuadratura.csv")
#view(Correccion2010)





###################################################################################



# Corrección del objeto 'Correccion2020' basado en 'Poblacion2020_grupos'
Correccion2020 <- Poblacion2020_grupos
for (i in 3:15) {
  Correccion2020[i, 2:3] <- (1/16) * (-1 * Poblacion2020_grupos[i+2, 2:3] + 
                                        4 * Poblacion2020_grupos[i+1, 2:3] + 
                                        10 * Poblacion2020_grupos[i, 2:3] + 
                                        4 * Poblacion2020_grupos[i-1, 2:3] - 
                                        1 * Poblacion2020_grupos[i-2, 2:3])
}

Correccion2020
#view(Correccion2020)

# Cálculo de la cuadratura y normalización
fac_cuadratura <- Correccion2020
fac_cuadratura$Hombres <- Correccion2020$Hombres / sum(Correccion2020$Hombres)
fac_cuadratura$Mujeres <- Correccion2020$Mujeres / sum(Correccion2020$Mujeres)

# Cuadratura corregida
 Pob2020_Prorrateo$Hombres <- as.numeric(gsub("[^0-9]", "", Pob2020_Prorrateo$Hombres))
 Pob2020_Prorrateo$Mujeres <- as.numeric(Pob2020_Prorrateo$Hombres)
 
 Pob2020_Prorrateo$Mujeres <- as.numeric(gsub("[^0-9]", "", Pob2020_Prorrateo$Mujeres))
 Pob2020_Prorrateo$Mujeres <- as.numeric(Pob2020_Prorrateo$Mujeres)
# Pob2010_Prorrateo$Edad <- gsub("\xa0", " ", Pob2010_Prorrateo$Edad)
Correccion2020_cuadratura <- Correccion2020

Correccion2020_cuadratura$Mujeres <- sum(Pob2020_Prorrateo$Mujeres)*fac_cuadratura$Mujeres
Correccion2020_cuadratura$Hombres <- sum(Pob2020_Prorrateo$Hombres)*fac_cuadratura$Hombres

#Corroborar:
sum(Pob2020_Prorrateo$Hombres) == sum(Correccion2020_cuadratura$Hombres)
sum(Pob2020_Prorrateo$Mujeres) == sum(Correccion2020_cuadratura$Mujeres)

# Redondear:
Correccion2020_cuadratura$Hombres <- round(Correccion2020_cuadratura$Hombres)
Correccion2020_cuadratura$Mujeres <- round(Correccion2020_cuadratura$Mujeres)

# Renombrar las filas correctamente (nombres explícitos del rango 1 a 18)
rownames(Correccion2020_cuadratura) <- seq(1, nrow(Correccion2020_cuadratura))

# Mostrar la tabla
#view(Correccion2020_cuadratura)

write.csv(Correccion2020_cuadratura, "Correccion2020_cuadratura.csv", row.names = FALSE)
Correccion2020 <- read.csv("Correccion2020_cuadratura.csv")
#view(Correccion2020)

######**********      1. PIRAMIDE POBLACIONAL        **********######

#2010
Correccion2010 <- Correccion2010_cuadratura
Correccion2010
data2010 <- Correccion2010 %>% select(Hombres,
                                      Mujeres,
                                      Edad
)

# Escalar los valores dividiéndolos por 1000
data2010$Hombres <- data2010$Hombres / 1000
data2010$Mujeres <- data2010$Mujeres / 1000

# Generar la pirámide nuevamente
pyramid(data2010, Llab = "Hombres (en miles)", Rlab = "Mujeres (en miles)", 
        Clab = "Grupo", Rcol = "slateblue4", Lcol = "deepskyblue4")

data2010$Hombres <- data2010$Hombres * 1000
data2010$Mujeres <- data2010$Mujeres * 1000


data2010 <- Correccion2010 %>% select()


#2020
Correccion2020 <- Correccion2020_cuadratura
Correccion2020
data2020 <- Correccion2020 %>% select(Hombres,
                                      Mujeres,
                                      Edad
)

# Escalar los valores dividiéndolos por 1000
data2020$Hombres <- data2020$Hombres / 1000
data2020$Mujeres <- data2020$Mujeres / 1000

# Generar la pirámide nuevamente
pyramid(data2020, Llab = "Hombres (en miles)", Rlab = "Mujeres (en miles)", 
        Clab = "Grupo", Rcol = "deeppink1", Lcol = "dodgerblue2")

data2020$Hombres <- data2020$Hombres * 1000
data2020$Mujeres <- data2020$Mujeres * 1000


data2020 <- Correccion2020 %>% select()

####Laboratorio 2
# **************************** POBLACION CORREGIDA A MITAD DE AÑO CENSAL ********************************

# A partir del crecimiento geometrico y exponencial se llevara a mitad de año 2015 y 2020, 
# a la poblacion de hombres y de mujeres en cada grupo de edad menores de 1, 1 a 4, 5 a 9,...,80 a 84, 85 y mas años de edad.

# Cargamos las bases de datos: 
Correccion2010_cuadratura <- read.csv("Correccion2010_cuadratura.csv", header =  TRUE)
Correccion2020_cuadratura <- read.csv("Correccion2020_cuadratura.csv", header = TRUE)

######**********              FECHAS                 **********######
# Censo 2010, fecha de referencia: 12 junio 2010
# Censo 2020, fecha de referencia: 15 marzo 2020

fcensos <- as.Date(c("2010-06-12", "2020-03-15"))
dias = as.numeric(fcensos[2]-fcensos[1])
n = dias/365

fechas <- as.Date(c("2015-06-30", "2020-06-30"))
n1 <- as.numeric(fechas[1] - fcensos[1])/365
n2 <- as.numeric(fechas[2] - fcensos[2])/365

# ************************ 2. CRECIMIENTO POBLACIONAL GEOMETRICO ********************************
#2010
colnames(Correccion2010_cuadratura) <- c("Edad", "Hombres", "Mujeres")
Correccion2010_cuadratura = rbind(Pob2010_Prorrateo[1,], Correccion2010_cuadratura)
Correccion2010_cuadratura[1,1] = c("Menor a 1 año")
Correccion2010_cuadratura[2,2:3] = Correccion2010_cuadratura[2,2:3] - Correccion2010_cuadratura[1,2:3]
Correccion2010_cuadratura[2,1] = c("1 a 4 años")

#2020
colnames(Correccion2020_cuadratura) <- c("Edad", "Hombres", "Mujeres")
Correccion2020_cuadratura = rbind(Pob2020_Prorrateo[1,], Correccion2020_cuadratura)
Correccion2020_cuadratura[1,1] = c("Menor a 1 año")
Correccion2020_cuadratura[2,2:3] = Correccion2020_cuadratura[2,2:3] - Correccion2020_cuadratura[1,2:3]
Correccion2020_cuadratura[2,1] = c("1 a 4 años")

r_Geom = Correccion2020_cuadratura
r_Geom$Hombres = ((Correccion2020_cuadratura$Hombres/Correccion2010_cuadratura$Hombres)^(1/n))-1
r_Geom$Mujeres = ((Correccion2020_cuadratura$Mujeres/Correccion2010_cuadratura$Mujeres)^(1/n))-1
r_Geom

# ************************ 2. CRECIMIENTO POBLACIONAL EXPONENCIAL ********************************
r_Exp = Correccion2020_cuadratura
r_Exp$Hombres = ((log(Correccion2020_cuadratura$Hombres/Correccion2010_cuadratura$Hombres))/(n))
r_Exp$Mujeres = ((log(Correccion2020_cuadratura$Mujeres/Correccion2010_cuadratura$Mujeres))/(n))
r_Exp

# ************************ 3.a. PROYECCION DE POBLACION GEOMETRICO ********************************

# Llevamos a mitad de año (30/junio/t) a los hombres y a las mujeres para el 2015 y 2020: 
f2 <- as.Date(c("2010-06-12", "2015-06-30"))
f3 <- as.Date(c("2020-03-15", "2020-06-30"))
n1 = as.numeric(f2[2]-f2[1])/365  
n2 = as.numeric(f3[2]-f3[1])/365  #obtenemos la diferencias de años

Pob_Mitad2015_Geom = Correccion2010_cuadratura
Pob_Mitad2015_Geom$Hombres=Correccion2010_cuadratura$Hombres*(1+r_Geom$Hombres)^(n1)
Pob_Mitad2015_Geom$Mujeres=Correccion2010_cuadratura$Mujeres*(1+r_Geom$Mujeres)^(n1)

write.csv(Pob_Mitad2015_Geom, "Población_Mitad_2015_Geom.csv")
Pob_Mitad2015_Geom <- read.csv("Población_Mitad_2015_Geom.csv")
#view(Pob_Mitad2015_Geom)

Pob_Mitad2015_Exp = Correccion2010_cuadratura
Pob_Mitad2015_Exp$Hombres=Correccion2010_cuadratura$Hombres*exp((r_Exp$Hombres)*(n1))
Pob_Mitad2015_Exp$Mujeres=Correccion2010_cuadratura$Mujeres*exp((r_Exp$Mujeres)*(n1))

write.csv(Pob_Mitad2015_Exp, "Población_Mitad_2015_Exp.csv")
Pob_Mitad2015_Exp <- read.csv("Población_Mitad_2015_Exp.csv")
#view(Pob_Mitad2015_Exp)

Pob_Mitad2020_Geom = Correccion2010_cuadratura
Pob_Mitad2020_Geom$Hombres=Correccion2020_cuadratura$Hombres*(1+r_Geom$Hombres)^(n2)
Pob_Mitad2020_Geom$Mujeres=Correccion2020_cuadratura$Mujeres*(1+r_Geom$Mujeres)^(n2)

write.csv(Pob_Mitad2015_Geom, "Población_Mitad_2020_Geom.csv")
Pob_Mitad2020_Geom <- read.csv("Población_Mitad_2020_Geom.csv")
#view(Pob_Mitad2020_Geom)

Pob_Mitad2020_Exp = Correccion2010_cuadratura
Pob_Mitad2020_Exp$Hombres=Correccion2020_cuadratura$Hombres*exp((r_Exp$Hombres)*(n2))
Pob_Mitad2020_Exp$Mujeres=Correccion2020_cuadratura$Mujeres*exp((r_Exp$Mujeres)*(n2))

write.csv(Pob_Mitad2020_Exp, "Población_Mitad_2020_Exp.csv")
Pob_Mitad2020_Exp <- read.csv ("Población_Mitad_2020_Exp.csv")
#view(Pob_Mitad2020_Exp)

###*****************************Laboratorio 3 **************************************


# ************************ MORTALIDAD ****************************
setwd("C:/Users/hello/OneDrive/Escritorio/FC/Demografia")

####Carga de bases de datos####
Def_2015 <- read_csv("Defunciones Durango_2015.csv", show_col_types = FALSE)
Nac_2015 <- read_csv("Nacimientos Durango_2015.csv", show_col_types = FALSE)
Pob_2015 <- read_csv("Población_Mitad_2015_Geom.csv", show_col_types = FALSE)
Def_2020 <- read_csv("Defunciones Durango_2020.csv", show_col_types = FALSE)
Nac_2020 <- read_csv("Nacimientos Durango_2020.csv", show_col_types = FALSE)
Pob_2020 <- read_csv("Población_Mitad_2020_Geom.csv", show_col_types = FALSE)

Pob_2015 = Pob_2015[,-1] #Solo si es necesario

colnames(Def_2015) <- c("Edad", "Hombres", "Mujeres")
colnames(Nac_2015) <- c("Edad", "Hombres", "Mujeres")
colnames(Def_2020) <- c("Edad", "Hombres", "Mujeres")
colnames(Nac_2020) <- c("Edad", "Hombres", "Mujeres")

Def_2015[is.na(Def_2015)] = 0
Nac_2015[is.na(Nac_2015)] = 0
Def_2020[is.na(Def_2020)] = 0
Nac_2020[is.na(Nac_2020)] = 0

####*******************************MEDIDAS DE MORTALIDAD************************####

# Tasa Bruta de Mortalidad
TBM_H_2015 = (sum(Def_2015$Hombres)/sum(Pob_2015$Hombres))*1000; TBM_H_2015
TBM_M_2015 = (sum(Def_2015$Mujeres)/sum(Pob_2015$Mujeres))*1000; TBM_M_2015
TBM_H_2020 = (sum(Def_2020$Hombres)/sum(Pob_2020$Hombres))*1000; TBM_H_2020
TBM_M_2020 = (sum(Def_2020$Mujeres)/sum(Pob_2020$Mujeres))*1000; TBM_M_2020

# Tasas Especificas de Mortalidad
TEM_2015 = Def_2015
TEM_2015$Hombres = (Def_2015$Hombres/Pob_2015$Hombres)*1000
TEM_2015$Mujeres = (Def_2015$Mujeres/Pob_2015$Mujeres)*1000
TEM_2015
#view(TEM_2015)

TEM_2020 = Def_2020
TEM_2020$Hombres = (Def_2020$Hombres/Pob_2020$Hombres)*1000
TEM_2020$Mujeres = (Def_2020$Mujeres/Pob_2020$Mujeres)*1000
TEM_2020
#view(TEM_2020)


# Tasa Tipificada de Mortalidad
TTM_H_2020_2015 = (sum((TEM_2015$Hombres/1000)*Pob_2020$Hombres)/sum(Pob_2020$Hombres))*1000; TTM_H_2020_2015
TTM_M_2020_2015 = (sum((TEM_2015$Mujeres/1000)*Pob_2020$Mujeres)/sum(Pob_2020$Mujeres))*1000; TTM_M_2020_2015

# Tasa de Mortalidad Infantil
TMI_H_2015 <- (Def_2015$Hombres[1]/sum(Nac_2015$Hombres))*1000; TMI_H_2015
TMI_M_2015 <- (Def_2015$Mujeres[1]/sum(Nac_2015$Mujeres))*1000; TMI_M_2015

# Tasa de Mortalidad Infantil
TMI_H_2020 <- (Def_2020$Hombres[1]/sum(Nac_2020$Hombres))*1000; TMI_H_2020
TMI_M_2020 <- (Def_2020$Mujeres[1]/sum(Nac_2020$Mujeres))*1000; TMI_M_2020

Def_2015 <- read_csv("Defunciones Durango_2015.csv", show_col_types = FALSE)
Pob_2020 <- read_csv("Pob2020.csv", show_col_types = FALSE)
Pob_2015 <- read_csv("Población_Mitad_2015_Geom.csv", show_col_types = FALSE)
Def_2020 <- read_csv("Defunciones Durango_2020.csv", show_col_types = FALSE)

######**********      1. MEDIDAS DE MORTALIDAD         **********######

# Tasa Bruta de Mortalidad
colnames(Def_2015) <- c("Edad", "D_hombres", "D_mujeres")
colnames(Def_2020) <- c("Edad", "D_hombres", "D_mujeres")
colnames(Pob_2015) <- c("Edad", "P_hombres", "P_mujeres")
colnames(Pob_2020) <- c("Edad", "P_hombres", "P_mujeres")

TBM_H_2015 <- (sum(Def_2015$D_hombres)/sum(Pob_2015$P_hombres))*1000
TBM_M_2015 <- (sum(Def_2015$D_mujeres)/sum(Pob_2015$P_mujeres))*1000
TBM_H_2020 <- (sum(Def_2020$D_hombres)/sum(Pob_2020$P_hombres))*1000
TBM_M_2020 <- (sum(Def_2020$D_mujeres)/sum(Pob_2020$P_mujeres))*1000

# Tasas Especificas de Mortalidad
Pob_2015$Edad <- Def_2015$Edad

TEM_2015 <- left_join(Def_2015, Pob_2015, by="Edad") %>% 
  mutate(M_hombres := D_hombres/P_hombres*1000,
         M_mujeres := D_mujeres/P_mujeres*1000) %>% 
  select(Edad, M_hombres, M_mujeres)


Pob_2020$Edad <- Def_2020$Edad

TEM_2020 <- left_join(Def_2020, Pob_2020, by="Edad") %>% 
  mutate(M_hombres := D_hombres/P_hombres*1000,
         M_mujeres := D_mujeres/P_mujeres*1000) %>% 
  select(Edad, M_hombres, M_mujeres)


# Tasa Tipificada de Mortalidad
Pob_2020$Edad <- Def_2020$Edad


# Considerando población tipo: CDMX 2020
DE_2015 <- left_join(TEM_2015, Pob_2020, by="Edad") %>% 
  mutate(D_e_hombres := M_hombres*P_hombres/1000,
         D_e_mujeres := M_mujeres*P_mujeres/1000) 

TTM_H_2015 <- (sum(DE_2015$D_e_hombres)/sum(Pob_2020$P_hombres))*1000
TTM_M_2015 <- (sum(DE_2015$D_e_mujeres)/sum(Pob_2020$P_mujeres))*1000

# Tasa de Mortalidad Infantil
TMI_H_2015 <- Def_2015$D_hombres[1]/sum(Nac_2015$N_hombres) * 1000
TMI_M_2015 <- Def_2015$D_mujeres[1]/sum(Nac_2015$N_mujers) * 1000

Def_causa <- Def_causa %>% 
  select(Causa, Edad, Hombre, Mujer) %>% 
  filter(Causa != " Total",
         Edad != " Total",
         Edad != " No especificado")

Def_causa_h <- Def_causa %>%  select(Causa, Edad, Hombre)

cuadro_h <- Def_causa_h %>% 
  mutate(causa_cantidad = paste(Causa, Hombre, sep = "\n")) %>%
  arrange(Edad, desc(Hombre)) %>%
  group_by(Edad) %>%
  mutate(posicion = row_number()) %>%
  ungroup() %>%
  select(Edad, posicion, causa_cantidad) %>%
  pivot_wider(names_from = Edad, values_from = causa_cantidad) %>%
  select(-posicion) %>% 
  select(19, 1, 10, 2:9, 11:18) %>% 
  slice(1:5)
Def_causa <- Def_causa %>% 
  select(Causa, Edad, Hombre, Mujer) %>% 
  filter(Causa != " Total",
         Edad != " Total",
         Edad != " No especificado")

Def_causa_h <- Def_causa %>%  select(Causa, Edad, Mujer)

cuadro_m <- Def_causa_m %>% 
  mutate(causa_cantidad = paste(Causa, Mujer, sep = "\n")) %>%
  arrange(Edad, desc(Mujer)) %>%
  group_by(Edad) %>%
  mutate(posicion = row_number()) %>%
  ungroup() %>%
  select(Edad, posicion, causa_cantidad) %>%
  pivot_wider(names_from = Edad, values_from = causa_cantidad) %>%
  select(-posicion) %>% 
  select(19, 1, 10, 2:9, 11:18) %>% 
  slice(1:5)

####*******************************TABLA DE VIDA************************####

#### TABLA DE VIDA

Tabla_Vida <- function(Defunciones, Poblacion, Edad, TMI){
  #Definimos la columna correspondiente a los grupos de edades
  Edades <-Edad
  Edades = c(0,1,seq(5,85, by=5))
  #Definimos n
  n<-c()
  n[1] = 1
  n[2] = 4
  n[3:18] = 5
  n[19] = NA
  
  #Definimos nDx
  nDx = Defunciones
  
  #Definimos nNx
  nNx = Poblacion
  
  #Definimos nMx
  nMx = nDx/nNx
  
  #Definimos nmx
  nmx <- c()
  nmx[1] = TMI
  nmx[2:19] = nMx[2:19]
  
  #Definimos nax
  nax = n/2
  nax[2] = 1.4
  nax[19] = NA
  
  #Definimos nqx
  nqx=c()
  nqx[2:18] = (n[2:18]*nmx[2:18])/(1+(n[2:18]-nax[2:18])*nmx[2:18])
  nqx[1] = nmx[1]
  nqx[19] = 1
  
  #Definimos npx 
  npx = 1-nqx
  
  #Definimos lx
  lx<-c()
  lx[1] = 100000
  for(i in 2:19){
    lx[i] = lx[i-1]*npx[i-1]
  }
  
  #Definimos ndx
  ndx = lx*nqx
  
  #Definimos nLx
  nLx=c()
  for(i in 1:18){
    nLx[i] = (n[i]*lx[i+1])+(nax[i]*ndx[i])
  }
  nLx[19] = lx[19]/nMx[19]
  
  
  #Definimos Tx
  Tx = c()
  for(i in 1:19){
    Tx[i] = sum(nLx[i:19])
  }
  
  #Definimos ex
  ex = Tx/lx
  
  #Juntamos todos los elementos de la tabla de vida
  Tabla_Vida <- data.frame(Edades,n,nax,nDx,nNx,nMx,nmx,nqx,npx,lx,ndx,nLx,Tx,ex)
  
}

Def_2015$Hombres
Def_2015$Mujeres
Def_2020$Hombres
Def_2020$Mujeres


TV_H_2015 = Tabla_Vida(Def_2015$Hombres,Pob_2015$Hombres,Pob_2015$Edad,TMI_H_2015/1000)
TV_M_2015 = Tabla_Vida(Def_2015$Mujeres,Pob_2015$Mujeres,Pob_2015$Edad,TMI_M_2015/1000)#Colocar entre las comas la información que entra en la función
TV_H_2020 = Tabl0123+ a_Vida(Def_2020$Hombres,Pob_2020$Hombres,Pob_2020$Edad,TMI_H_2020/1000)
TV_M_2020 = Tabla_Vida(Def_2020$Mujeres,Pob_2020$Mujeres,Pob_2020$Edad,TMI_M_2020/1000)

# ************************ Relacion de sobrevivencia ****************************
Rel_Sobrevivencia = function (nLx){
  nLx[1] = nLx [1] + nLx [2]
  nLx = nLx[-2]
  
  x=c()
  x = seq(0,80,by = 5)
  n = rep(5,17)
  n[17]=NA
  S = c()
  for (i in 1:16){
    S[i] = nLx[i+1]/nLx[i]
  }
  S[17] = nLx[18]/(nLx[17] + nLx[18])
  
  Rel_Sob = data.frame(x,n,S)
  
}

Rel_Sobrevivencia_Nac = function (nLx, radix){
  nLx[1] = nLx [1] + nLx [2]
  nLx = nLx[-2]
  radix = radix
  Rel_Sob_Nac = nLx[1]/(5*radix)
}

S_H_2015 = Rel_Sobrevivencia(TV_H_2015$nLx);  S_H_2015
S_Nac_H_2015 = Rel_Sobrevivencia_Nac(TV_H_2015$nLx, TV_H_2015$lx[1]); S_Nac_H_2015

S_M_2015 = Rel_Sobrevivencia(TV_M_2015$nLx);  S_M_2015
S_Nac_M_2015 = Rel_Sobrevivencia_Nac(TV_M_2015$nLx, TV_H_2015$lx[1]); S_Nac_M_2015

S_H_2020 = Rel_Sobrevivencia(TV_H_2020$nLx);  S_H_2020
S_Nac_H_2020 = Rel_Sobrevivencia_Nac(TV_H_2020$nLx, TV_H_2015$lx[1]); S_Nac_H_2020

S_M_2020 = Rel_Sobrevivencia(TV_M_2020$nLx);  S_M_2020
S_Nac_M_2020 = Rel_Sobrevivencia_Nac(TV_M_2020$nLx, TV_H_2015$lx[1]); S_Nac_M_2020


write.csv(TV_H_2015, "TV_M_2015.csv")
write.csv(TV_H_2015, "TV_M_2020.csv")

# ************************ FECUNDIDAD ****************************


format(9999,scientific = FALSE)
options(digits=4)

####Carga de bases de datos####
Def_2015 <- read_csv("Defunciones Durango_2015.csv")
Def_2020 <- read_csv("Defunciones Durango_2020.csv")
Nac_2015 <- read_csv("Nacimientos Durango_2015.csv")
Nac_2020 <- read_csv("Nacimientos Durango_2020.csv")
Pob_2015 <- read_csv("Poblacion_Mitad_2015_Geo.csv")
Pob_2020 <- read_csv("Poblacion_Mitad_2020_Geo.csv")
TV_M_2015 <- read_csv("TV_M_2015.csv")
TV_M_2020 <- read_csv("TV_M_2020.csv")

#########TASA BRUTA DE NATALIDAD  
TBN_2015 = (sum(Nac_2015[2:3])/sum(Pob_2015[2:3]))*1000; TBN_2015
TBN_2020 = (sum(Nac_2015[2:3])/sum(Pob_2020[2:3]))*1000; TBN_2020

#####Tasa de fecundidad general 
TFG_2015= (sum(Nac_2015[2:3])/sum(Pob_2015$Mujeres[5:11]))*1000; TFG_2015
TFG_2015= (sum(Nac_2015[2:3])/sum(Pob_2015$Hombre[5:11]))*1000; TFG_2015
TFG_2020= (sum(Nac_2015[2:3])/sum(Pob_2015$Mujeres[5:11]))*1000; TFG_2020
TFG_2020= (sum(Nac_2015[2:3])/sum(Pob_2015$Hombres[5:11]))*1000; TFG_2020

#####Tasa de Fecundidad por edades 
TFE_2015 = ((Nac_2015$Hombres + Nac_2015$Mujeres)/Pob_2015$Mujeres[5:11]); TFE_2015
TFE_2015 = ((Nac_2015$Hombres + Nac_2015$Mujeres)/Pob_2015$Hombres[5:11]); TFE_2015
TFE_2020 = ((Nac_2020$Hombres + Nac_2020$Mujeres)/Pob_2020$Mujeres[5:11]); TFE_2020
TFE_2020 = ((Nac_2020$Hombres + Nac_2020$Mujeres)/Pob_2020$Hombres[5:11]); TFE_2020

#####Grafica
#2015
plot(TFE_2015, type="o", col="blue", main="Tasa de Fecundidad por Edad en la CDMX en 2015", xlab="Edad", xaxt="n")
axis(1, at=seq(1,7), labels = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"))
#2020
plot(TFE_2025, type="o", col="yellow", main="Tasa de Fecundidad por Edad en la CDMX en 2015", xlab="Edad", xaxt="n")
axis(1, at=seq(1,7), labels = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"))
#Ver que patron de fecundidad tiene cada uno 

#####Tasa Global de Fecundidad 
TGF_2015 = 5*sum(TFE_2015);TGF_2015
TGF_2020 = 5*sum(TFE_2020);TGF_2020

#####Factor de Femineidad 
#es la proporcion de mujeres nacidas en ese año 
k_2015=sum(Nac_2015$Mujeres)/sum(Nac_2015[2:3]);k_2015
k_2020=sum(Nac_2020$Mujeres)/sum(Nac_2020[2:3]);k_2020

#####Tasa bruta de reproduccion 
TBR_2015 = k_2015*TGF_2015; TBR_2015
TBR_2020 = k_2020*TGF_2020; TBR_2020

#####Tasa Neta de Reproduccion 
px_2015=TV_M_2015$nLx[5:11]/(5*TV_M_2015$lx[1]); px_2015
TNR_2015 =5*k_2015* sum(px_2015*TFE_2015); TNR_2015 

px_2020=TV_M_2020$nLx[5:11]/(5*TV_M_2020$lx[1]); px_2020
TNR_2020 =5*k_2020* sum(px_2020*TFE_2020); TNR_2020
