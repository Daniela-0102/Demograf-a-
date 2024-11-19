 # Instalar y cargar el paquete 'pyramid'
install.packages("pyramid")
library(pyramid)
# Instalar dplyr si no lo tienes instalado
  install.packages("dplyr")

# Eliminar todos los objetos del entorno
rm(list=ls())
# Cargar dplyr
install.packages("tidyverse")
install.packages("pyramid")
install.packages("dplyr")
library(tidyverse)
library(pyramid)
library(dplyr)
setwd("C:/Users/hello/OneDrive/Escritorio/FC/Demografia")

#### TEMA 1.1: EVALUACION DE LA POBLACION:

# Cargamos las bases de datos: 
Pob2010 <- read.csv("Pob2010.csv", header = TRUE)
View(Pob2010)

Pob2010[is.na(Pob2010)] = 0
Pob2010 <- Pob2010 %>% slice(-1)
colnames(Pob2010)[1] <- "Edad"
View(Pob2010)


data2010 <- Pob2010 %>% select(Hombres, Mujeres, Edad)

Pob2010$Hombres <- as.numeric(gsub("[^0-9.]", "", Pob2010$Hombres))
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


######**********      3. INDICE DE NACIONES UNIDAS      **********######
# Función para calcular grupos quinquenales
rupos_quinquenales <- function(x, columna, edad_final){
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

IH_G <- sum(sumandos_IH) / 13 * 100
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
  sumandos_IS[l] <- abs( (2 * grupos2010_H$num_pers[l+1] / grupos2010_M$num_pers[l+1]) - 
                           (grupos2010_H$num_pers[l+1] / grupos2010_M$num_pers[l+1]) )
}

# Resultado del indice para ambos sexos
IS <- sum(sumandos_IS) / 13 * 100

# Cálculo del INU
INU <- (IH_G + IM_G + IS) / 3
print(INU)


######**********      5. INDICE DE MASCULINIDAD         **********######
#para población 2010
# Agrupar los hombres en grupos quinquenales y renombrar la columna
Hombres_grupos <- grupos_quinquenales(Pob2010, "Hombres", 84)
colnames(Hombres_grupos)[2] <- "Hombres"

# Agrupar las mujeres en grupos quinquenales y renombrar la columna
Mujeres_grupos <- grupos_quinquenales(Pob2010, "Mujeres", 84)
colnames(Mujeres_grupos)[2] <- "Mujeres"

# Unir ambos grupos por la columna "Grupo" usando merge()
Poblacion_grupos <- merge(Hombres_grupos, Mujeres_grupos, by = "Grupo", all.x = TRUE)


# Calcular el índice de masculinidad
IMasculinidad <- Poblacion_grupos |>
  transform(I_Masc = Hombres / Mujeres * 100)


# Visualizar el resultado
View(IMasculinidad)


######**********      6. RAZON DE DEPENDENCIA           **********######

# Cálculo de la razón de dependencia para hombres
RDep_Hombres <- (sum(Poblacion_grupos$Hombres[c(1:3, 14:18)]) / sum(Poblacion_grupos$Hombres[4:13])) * 100

# Cálculo de la razón de dependencia para mujeres
RDep_Mujeres <- (sum(Poblacion_grupos$Mujeres[c(1:3, 14:18)]) / sum(Poblacion_grupos$Mujeres[4:13])) * 100

# Cálculo de la razón de dependencia total (corrigiendo el denominador)
RDep_Total <- ((sum(Poblacion_grupos$Hombres[c(1:3, 14:18)]) + sum(Poblacion_grupos$Mujeres[c(1:3, 14:18)])) 
               / (sum(Poblacion_grupos$Hombres[4:13]) + sum(Poblacion_grupos$Mujeres[4:13]))) * 100

# Visualizar los resultados
RDep_Hombres
RDep_Mujeres
RDep_Total

# ************************ 1. PRORRATEO DE LA INFORMACION ********************************
# Calcular el porcentaje de concentración de la población No Especificada para Hombres
Pob_Total_Hombres <- sum(Pob2010$Hombres)  # Total de hombres en todas las edades
Pob_NE_Hombres <- Pob2010$Hombres[length(Pob2010$Edad)]  # Hombres en la categoría "No Especificada"
alfa_Hombres = (Pob_NE_Hombres/(Pob_Total_Hombres-Pob_NE_Hombres))  # Proporción de Hombres No Especificados
alfa_Hombres

# Calcular el porcentaje de concentración de la población No Especificada para Mujeres
Pob_Total_Mujeres <- sum(Pob2010$Mujeres)  # Total de mujeres en todas las edades
Pob_NE_Mujeres <- Pob2010$Mujeres[length(Pob2010$Edad)]  # Mujeres en la categoría "No Especificada"
alfa_Mujeres = (Pob_NE_Mujeres/(Pob_Total_Mujeres-Pob_NE_Mujeres))  # Proporción de Mujeres No Especificadas
alfa_Mujeres

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
view(Correccion2010)
######**********      1. PIRAMIDE POBLACIONAL        **********######

# Seleccionar datos para 2010 (asegúrate de tener las mismas columnas)
data2010 <- Correccion2010 %>% select(Hombres, Mujeres, Edad)

# Escalar los valores dividiéndolos por 1000
data2010$Hombres <- data2010$Hombres / 1000
data2010$Mujeres <- data2010$Mujeres / 1000


# Crear la pirámide de población para 2010
pyramid(data2010, Llab = "Hombres", Rlab = "Mujeres", Clab = "Edad", 
       Rcol = "lightgreen", Lcol = "lightseagreen")

# ************************ 2. METODO DE 1/16 ******************************** 
#### quinquenios
#Funcion para quinquenio
grupos_quinquenales <- function(x, columna, edad_final){
  extremo_inferior <- seq(0, edad_final, 5)
  extremo_superior <- extremo_inferior + 4
  Grupo <- paste0(extremo_inferior,"-",extremo_superior)
  Grupo <- c(Grupo, paste0(edad_final+1," y mas"))
  
  num_pers <- rep(0,length(extremo_inferior))
  
  for (i in 1:length(extremo_inferior)) {
    a <- extremo_inferior[i]+1
    b <- extremo_superior[i]+1
    num_pers[i] <- sum(x[a:b, columna])
  }
  num_pers <- c(num_pers, sum(x[(edad_final+2):nrow(x), columna]) )
  
  tabla_quin <- data.frame(Grupo, num_pers)
  
  return(tabla_quin)
}

#2010
Poblacion2010_corregida <- Pob2010 %>% slice(1:(n() - 1))
Poblacion2010_corregida
## Hombres 2010
grupos2010_H <- grupos_quinquenales(Poblacion2010_corregida, "Hombres", 84)
grupos2010_H
## Mujeres 2010
grupos2010_M <- grupos_quinquenales(Poblacion2010_corregida, "Mujeres", 84)
grupos2010_M

quinquenios2010 <- left_join(grupos2010_H, grupos2010_M, by="Grupo")
colnames(quinquenios2010) <- c("Grupo", "Hombres", "Mujeres")
view(quinquenios2010)
# Corrección del objeto 'Correccion2010' basado en 'quinquenios2010'
Correccion2010 <- quinquenios2010

# Evitar problemas con los índices fuera de rango en el bucle
for (i in 3:15) {
    Correccion2010[i, 2:3] <- (1/16) * (-1 * quinquenios2010[i+2, 2:3] + 
                                          4 * quinquenios2010[i+1, 2:3] + 
                                          10 * quinquenios2010[i, 2:3] + 
                                          4 * quinquenios2010[i-1, 2:3] - 
                                          1 * quinquenios2010[i-2, 2:3])
  }

Correccion2010
view(Correccion2010)

# Cálculo de la cuadratura y normalización
fac_cuadratura <- Correccion2010
fac_cuadratura$Hombres <- Correccion2010$Hombres / sum(Correccion2010$Hombres)
fac_cuadratura$Mujeres <- Correccion2010$Mujeres / sum(Correccion2010$Mujeres)

# Cuadratura corregida
Correccion2010_cuadratura <- Correccion2010
Correccion2010_cuadratura$Hombres <- sum(Pob2010_Prorrateo[, 2]) * fac_cuadratura$Hombres
Correccion2010_cuadratura$Mujeres <- sum(Pob2010_Prorrateo[, 3]) * fac_cuadratura$Mujeres

# Corroboración:
print(sum(Pob2010_Prorrateo[, 2]) == sum(Correccion2010_cuadratura$Hombres))
print(sum(Pob2010_Prorrateo[, 3]) == sum(Correccion2010_cuadratura$Mujeres))

# Redondear:
Correccion2010_cuadratura$Hombres <- round(Correccion2010_cuadratura$Hombres)
Correccion2010_cuadratura$Mujeres <- round(Correccion2010_cuadratura$Mujeres)

# Renombrar las filas correctamente (nombres explícitos del rango 1 a 18)
rownames(Correccion2010_cuadratura) <- seq(1, nrow(Correccion2010_cuadratura))

# Mostrar la tabla
View(Correccion2010_cuadratura)

write.csv(Correccion2010_cuadratura, "Correccion2010_cuadratura.csv", row.names = FALSE)
Correccion2010 <- read.csv("Correccion2010_cuadratura.csv")


#2010
Correccion2010 <- Correccion2010_cuadratura
Correccion2010
data2010 <- Correccion2010 %>% select(Hombres,
                                      Mujeres,
                                      Grupo
)

# Escalar los valores dividiéndolos por 1000
data2010$Hombres <- data2010$Hombres / 1000
data2010$Mujeres <- data2010$Mujeres / 1000

# Generar la pirámide nuevamente
pyramid(data2010, Llab = "Hombres (en miles)", Rlab = "Mujeres (en miles)", 
        Clab = "Grupo", Rcol = "deeppink1", Lcol = "dodgerblue2")

data2010$Hombres <- data2010$Hombres * 1000
data2010$Mujeres <- data2010$Mujeres * 1000


data2010 <- Correccion2010 %>% select()


#### Para la población 2020
#### TEMA 1.1: EVALUACION DE LA POBLACION:

# Cargamos las bases de datos: 
Pob2020 <- read.csv("Pobl2020.csv", header = TRUE)
View(Pob2020)

Pob2020[is.na(Pob2020)] = 0
Pob2020 <- Pob2020 %>% slice(-nrow( Pob2020 ))
View(Pob2020)

# Reemplazar valores no numéricos con NA
Pob2020$Hombres <- as.numeric(gsub("[^0-9.]", "", Pob2020$Hombres))
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

# Índice de Myers para mujeres (resumido):
Myers_Mujeres <- sum(abs(Mj_Mujeres))
Myers_Mujeres
 

###*****************************1. INDICE DE WHIPPLE **************************************

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
 rupos_quinquenales <- function(x, columna, edad_final){
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
  sumandos_IS[l] <- abs( (2 * grupos2020_H$num_pers[l+1] / grupos2020_M$num_pers[l+1]) - 
                           (grupos2020_H$num_pers[l+1] / grupos2020_M$num_pers[l+1]) )
}

# Resultado del indice para ambos sexos
IS <- sum(sumandos_IS) / 13 * 100

# Cálculo del INU
INU <- (IH_G + IM_G + IS) / 3
print(INU)


  ######**********      5. INDICE DE MASCULINIDAD         **********######
#para población 2020
# Agrupar los hombres en grupos quinquenales y renombrar la columna
Hombres_grupos <- grupos_quinquenales(Pob2020, "Hombres", 84)
colnames(Hombres_grupos)[2] <- "Hombres"

# Agrupar las mujeres en grupos quinquenales y renombrar la columna
Mujeres_grupos <- grupos_quinquenales(Pob2020, "Mujeres", 84)
colnames(Mujeres_grupos)[2] <- "Mujeres"

# Unir ambos grupos por la columna "Grupo" usando merge()
Poblacion_grupos <- merge(Hombres_grupos, Mujeres_grupos, by = "Grupo", all.x = TRUE)


# Calcular el índice de masculinidad
IMasculinidad <- Poblacion_grupos |>
  transform(I_Masc = Hombres / Mujeres * 100)


# Visualizar el resultado
View(IMasculinidad)


######**********      6. RAZON DE DEPENDENCIA           **********######

# Cálculo de la razón de dependencia para hombres
RDep_Hombres <- (sum(Poblacion_grupos$Hombres[c(1:3, 14:18)]) / sum(Poblacion_grupos$Hombres[4:13])) * 100

# Cálculo de la razón de dependencia para mujeres
RDep_Mujeres <- (sum(Poblacion_grupos$Mujeres[c(1:3, 14:18)]) / sum(Poblacion_grupos$Mujeres[4:13])) * 100

# Cálculo de la razón de dependencia total (corrigiendo el denominador)
RDep_Total <- ((sum(Poblacion_grupos$Hombres[c(1:3, 14:18)]) + sum(Poblacion_grupos$Mujeres[c(1:3, 14:18)])) 
               / (sum(Poblacion_grupos$Hombres[4:13]) + sum(Poblacion_grupos$Mujeres[4:13]))) * 100

# Visualizar los resultados
RDep_Hombres
RDep_Mujeres
RDep_Total

# ************************ 1. PRORRATEO DE LA INFORMACION ********************************
# Calcular el porcentaje de concentración de la población No Especificada para Hombres
Pob_Total_Hombres <- sum(Pob2020$Hombres)  # Total de hombres en todas las edades
Pob_NE_Hombres <- Pob2020$Hombres[length(Pob2020$Edad)]  # Hombres en la categoría "No Especificada"
alfa_Hombres = (Pob_NE_Hombres/(Pob_Total_Hombres-Pob_NE_Hombres))  # Proporción de Hombres No Especificados
alfa_Hombres

# Calcular el porcentaje de concentración de la población No Especificada para Mujeres
Pob_Total_Mujeres <- sum(Pob2020$Mujeres)  # Total de mujeres en todas las edades
Pob_NE_Mujeres <- Pob2020$Mujeres[length(Pob2020$Edad)]  # Mujeres en la categoría "No Especificada"
alfa_Mujeres = (Pob_NE_Mujeres/(Pob_Total_Mujeres-Pob_NE_Mujeres))  # Proporción de Mujeres No Especificadas
alfa_Mujeres


Pob2020_Prorrateo <- matrix(NA,length(Pob2020$Edad)-1,3)
for(i in 1:(length(Pob2010$Edad)-1)){
  if(alfa_Hombres>=0.1){
    Pob2020_Prorrateo[i,2]=Pob2020$Hombres[i]+Pob2020$Hombres[i]*alfa_Hombres
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
# Guardar el prorrateo en un archivo CSV
write.csv(Pob2020_Prorrateo, "Pob2020_Prorrateo.csv", row.names = FALSE)
Correccion2020 <- read.csv("Pob2020_Prorrateo.csv")

######**********      1. PIRAMIDE POBLACIONAL        **********######

# Seleccionar las columnas deseadas
data2020 <- Correccion2020 %>% select(Hombres, Mujeres, Edad)

# Crear la pirámide de población para 2020
pyramid(data2020, Llab = "Hombres", Rlab = "Mujeres", Clab = "Edad", 
        Rcol = "deepskyblue4", Lcol = "slateblue4")


# Seleccionar datos para 2010 (asegúrate de tener las mismas columnas)
#data2010 <- Correccion2010 %>% select(Hombres, Mujeres, Edad)


# Crear la pirámide de población para 2010
#pyramid(data2010, Llab = "Hombres", Rlab = "Mujeres", Clab = "Edad", 
#        Rcol = "lightgreen", Lcol = "lightseagreen")


####Laboratorio 2
# ************************ 2. METODO DE 1/16 ******************************** 

# Corrección del objeto 'Correccion2020' basado en 'quinquenios2020'
Correccion2020 <- quinquenios2020

# Evitar problemas con los índices fuera de rango en el bucle
for (i in 3:15) {
  if(i > 2 && (i+2) <= nrow(quinquenios2020)){
    Correccion2020[i, 2:3] <- (1/16) * (-1 * quinquenios2020[i+2, 2:3] + 
                                          4 * quinquenios2020[i+1, 2:3] + 
                                          10 * quinquenios2020[i, 2:3] + 
                                          4 * quinquenios2020[i-1, 2:3] - 
                                          1 * quinquenios2020[i-2, 2:3])
  }
}
Correccion2020

# Cálculo de la cuadratura y normalización
fac_cuadratura <- Correccion2020
fac_cuadratura$Hombres <- Correccion2020$Hombres / sum(Correccion2020$Hombres)
fac_cuadratura$Mujeres <- Correccion2020$Mujeres / sum(Correccion2020$Mujeres)

# Cuadratura corregida
Correccion2020_cuadratura <- Correccion2020
Correccion2020_cuadratura$Hombres <- sum(Pob2020_Prorrateo[, 2]) * fac_cuadratura$Hombres
Correccion2020_cuadratura$Mujeres <- sum(Pob2020_Prorrateo[, 3]) * fac_cuadratura$Mujeres

# Corroboración:
print(sum(Pob2020_Prorrateo[, 2]) == sum(Correccion2020_cuadratura$Hombres))
print(sum(Pob2020_Prorrateo[, 3]) == sum(Correccion2020_cuadratura$Mujeres))

# Redondear:
Correccion2020_cuadratura$Hombres <- round(Correccion2020_cuadratura$Hombres)
Correccion2020_cuadratura$Mujeres <- round(Correccion2020_cuadratura$Mujeres)

# Renombrar las filas correctamente (nombres explícitos del rango 1 a 18)
rownames(Correccion2020_cuadratura) <- seq(1, nrow(Correccion2020_cuadratura))

# Mostrar la tabla
View(Correccion2020_cuadratura)

write.csv(Correccion2020_cuadratura, "Correccion2020_cuadratura.csv", row.names = FALSE)
Correccion2010 <- read.csv("Correccion2010_cuadratura.csv")
Correccion2020 <- read.csv("Correccion2020_cuadratura.csv")

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
######**********      3. CRECIMIENTO EXPONENCIAL     **********#####
colnames(Correccion2020) <- c("Edad", "Hombres", "Mujeres")
r_Exp <- rbind(Pob2020_Prorrateo[1,], Correccion2020)
r_Exp[1,1] = c("Menor a 1 año")
r_Exp[2,2:3] = Correccion2010[2,2:3] - Correccion2010[1,2:3]
r_Exp[2,1] = c("1 a 4 años"); 
str(r_Exp)

r_Exp$Hombres = ((log(Correccion2020_cuadratura$Hombres/Correccion2010_cuadratura$Hombres))/(n))
r_Exp$Mujeres = ((log(Correccion2020_cuadratura$Mujeres/Correccion2010_cuadratura$Mujeres))/(n))
r_Exp

Pob_Exp <- left_join(Correccion2010_cuadratura, Correccion2020_cuadratura, by="Edad")
colnames(Pob_Exp) <- c("Edad", "H2010", "M2010", "H2020", "M2020")
Pob_Exp <- Pob_Exp %>% mutate(tasah := log(H2020/H2010)/n,
                              tasam := log(M2020/M2010)/n)

r_Exp <- Pob_Exp %>% select(tasah, tasam)

Pob_Mitad2015_Exp <- Pob_Exp %>% mutate(Hombres := round( H2010*exp(tasah*n1) ),
                                        Mujeres := round( M2010*exp(tasah*n1) )) %>%
  select("Edad", "Hombres", "Mujeres")

Pob_Mitad2020_Exp <- Pob_Exp %>% mutate(Hombres := round( H2010*exp(tasah*n1) ),
                                        Mujeres := round( M2010*exp(tasah*n1)  )) %>%
  select( "Edad", "Hombres", "Mujeres" )



######**********      4. CRECIMIENTO GEOMETRICO     **********#####
#Obtenemos la tasa del crecimiento geometrico intercensal

colnames(Correccion2020) <- c("Edad", "Hombres", "Mujeres")
r_Geom = rbind(Pob2010_Prorrateo[1,], Correccion2010)
r_Geom[1,1] = c("Menor a 1 año")
r_Geom[2,2:3] = Correccion2010[2,2:3] - Correccion2010[1,2:3]
r_Geom[2,1] = c("1 a 4 años"); r_Geom 

r_Geom$Hombres = ((Correccion2020_cuadratura$Hombres/Correccion2010_cuadratura$Hombres)^(1/n))-1
r_Geom$Mujeres = ((Correccion2020_cuadratura$Mujeres/Correccion2010_cuadratura$Mujeres)^(1/n))-1
r_Geom
# Llevamos a mitad de año (30/junio/t) a los hombres y a las mujeres para el 2015 y 2020: 
f2 <- as.Date(c("2010-06-12", "2015-06-30"))
f3 <- as.Date(c("2020-03-15", "2020-06-30"))
n1 = as.numeric(f2[2]-f2[1])/365  
n2 = as.numeric(f3[2]-f3[1])/365  #obtenemos la diferencias de años

Pob_Mitad2015_Geom = Correccion2010_cuadratura
Pob_Mitad2015_Geom$Hombres=Correccion2010_cuadratura$Hombres*(1+r_Geom$Hombres)^(n1)
Pob_Mitad2015_Geom$Mujeres=Correccion2010_cuadratura$Mujeres*(1+r_Geom$Mujeres)^(n1)

write.csv(Pob_Mitad2015_Geom, "Población_Mitad_2015.csv")

Pob_Mitad2020_Geom = Correccion2020_cuadratura
Pob_Mitad2020_Geom$Hombres=Correccion2020_cuadratura$Hombres*(1+r_Geom$Hombres)^(n2)
Pob_Mitad2020_Geom$Mujeres=Correccion2020_cuadratura$Mujeres*(1+r_Geom$Mujeres)^(n2)

write.csv(Pob_Mitad2020_Geom, "Población_Mitad_2020.csv")


###*****************************Laboratorio 3 **************************************


# ************************ MORTALIDAD ****************************

####Carga de bases de datos####
Def_2015 <- read_csv("Defunciones CDMX_2015.csv")
Nac_2015 <- read_csv("Nacimientos CDMX_2015.csv")
Pob_2015 <- read_csv("Poblacion_Mitad_2015.csv")

Pob_2015 = Pob_2015[,-1] #Solo si es necesario

colnames(Def_2015) <- c("Edad", "Hombres", "Mujeres")
colnames(Nac_2015) <- c("Edad", "Hombres", "Mujeres")

Def_2015[is.na(Def_2015)] = 0
Nac_2015[is.na(Nac_2015)] = 0

####*******************************MEDIDAS DE MORTALIDAD************************####

# Tasa Bruta de Mortalidad
TBM_H_2015 = (sum(Def_2015$Hombres)/sum(Pob_2015$Hombres))*1000; TBM_H_2015

# Tasas Especificas de Mortalidad
TEM_2015 = Def_2015
TEM_2015$Hombres = (Def_2015$Hombres/Pob_2015$Hombres)*1000
#View(TEM_2015)

# Tasa Tipificada de Mortalidad
TTM_H_2020_2015 = (sum((TEM_2015$Hombres/1000)*Pob_2020$Hombres)/sum(Pob_2020$Hombres))*1000; TTM_H_2020_2015

# Tasa de Mortalidad Infantil
TMI_H_2015 <- (Def_2015$Hombres[1]/sum(Nac_2015$Hombres))*1000; TMI_H_2015
setwd()

Def_2015 <- read_csv("Defunciones CDMX_2015.csv")
Nac_2015 <- read_csv("Nacimientos CDMX_2015.csv")
Pob_2015 <- read.xlsx("Pob2015..xlsx")

######**********      1. MEDIDAS DE MORTALIDAD         **********######
# Tasa Bruta de Mortalidad
TBM_H_2015 <- (sum(Def_2015$D_hombres)/sum(Pob_2015$P_hombres))*1000

# Tasas Especificas de Mortalidad
Pob_2015$Edad <- Def_2015$Edad

TEM_2015 <- left_join(Def_2015, Pob_2015, by="Edad") %>% 
  mutate(M_hombres := D_hombres/P_hombres*1000,
         M_mujeres := D_mujeres/P_mujeres*1000) %>% 
  select(Edad, M_hombres, M_mujeres)

# Tasa Tipificada de Mortalidad
Pob_2020$Edad <- Def_2015$Edad
# Considerando población tipo: CDMX 2020
DE_2015 <- left_join(TEM_2015, Pob_2020, by="Edad") %>% 
  mutate(D_e_hombres := M_hombres*P_hombres/1000,
         D_e_mujeres := M_mujeres*P_mujeres/1000) 
TTM_H_2015 <- (sum(DE_2015$D_e_hombres)/sum(Pob_2020$P_hombres))*1000


# Tasa de Mortalidad Infantil
TMI_H_2015 <- Def_2015$D_hombres[1]/sum(Nac_2015$N_hombres) * 1000

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

Def2015$Hombres


TV_H_2015 = Tabla_Vida(Def_2015$Hombres,Pob_2015$Hombres,Pob_2015$Edad,TMI_H_2015/1000) #Colocar entre las comas la información que entra en la función

# ************************ FECUNDIDAD ****************************


format(9999,scientific = FALSE)
options(digits=4)

####Carga de bases de datos####
Def_2015 <- read_csv("Defunciones CDMX_2015.csv")
Def_2020 <- read_csv("Defunciones CDMX_2020.csv")
Nac_2015 <- read_csv("Nacimientos CDMX_2015.csv")
Nac_2020 <- read_csv("Nacimientos CDMX_2020.csv")
Pob_2015 <- read_csv("Poblacion_Mitad_2015.csv")
Pob_2020 <- read_csv("Poblacion_Mitad_2020.csv")
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
plot(TFE_2025, type="o", col="blue", main="Tasa de Fecundidad por Edad en la CDMX en 2015", xlab="Edad", xaxt="n")
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
