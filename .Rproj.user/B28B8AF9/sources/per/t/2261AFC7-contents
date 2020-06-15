pacientes <- read.csv('pacientes.csv')
pacientes <- pacientes[,-1]
doctor <- read.csv('doctor.csv')
doctor <- doctor[,-1]

# Algunos plot y análisis ------------------------------------------------------------

hist(pacientes$presion, col = 'red',border = "red")
hist(pacientes$azucar, col='blue',border='blue')

sum(is.na(pacientes$presion))
sum(is.na(pacientes$azucar))


# SOLUCIÓN ------------------------------------------------------------
femenino <- doctor[doctor$genero=="Mujer",] #obteng solo las obs que corresponden a mujeres
masculino <- doctor[doctor$genero=="Hombre",]#obteng solo las obs que corresponden a hombres


# si se observala base de datos doctor se ve que los id de las mujeres son pares y de los hombres
# son impares por tanto la base pacientes de dividira en dos asi:
pacientesf <- pacientes[pacientes$id%%2==0,]
pacientesm <- pacientes[pacientes$id%%2!=0,]
mes <- rep(c(rep(1,30), rep(2,30)),50) # se realiza una repeticion de 1:mes 1 2:mes dos 
mes


pacientesf <- data.frame(pacientesf,mes) # creo los dataframes de hombre, mujer con el mes
pacientesm <- data.frame(pacientesm,mes)

# luego de tener las bases anterior se dividiran por id y mes para obtener las respectivas medias
# de cada medicion por paciente y mes
pacientesf_mid <- split(pacientesf, list(pacientesf$id,pacientesf$mes))
pacientesf_mid_mean <- lapply(pacientesf_mid,colMeans)
pacientesm_mid <-  split(pacientesm, list(pacientesm$id,pacientesm$mes))
pacientesm_mid_mean <- lapply(pacientesm_mid,colMeans)

# las listas que contienen las respectivas medias se vuelven data frames
pacientesf_mid_mean <- data.frame(t(data.frame(pacientesf_mid_mean)))
pacientesm_mid_mean <- data.frame(t(data.frame(pacientesm_mid_mean)))
View(pacientesm_mid_mean)
View(pacientesf_mid_mean)

# organizamos cada data frame por id
library(dplyr)
library(tidyverse)
pacientesf_mid_mean <- pacientesf_mid_mean %>% arrange(id)
pacientesm_mid_mean <- pacientesm_mid_mean %>% arrange(id)


# agrupando los datos por id
medias_mujeres_hombres <- rbind(pacientesf_mid_mean,pacientesm_mid_mean)
medias_mujeres_hombres <- medias_mujeres_hombres %>% arrange(id)

# agrupando los datos de hombres y mujeres con la del doctor
base_final <- right_join(medias_mujeres_hombres, doctor, by="id")
write.csv(base_final,'C:/Users/JESSICA/Desktop/TEST-DE-HABILIDADES/base_final.csv')


