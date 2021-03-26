#Hecho por Alan Ramirez, 2019-8331

#Aqui vamos a instalar y leer los paquetes requeridos para que el programa funcione.
install.packages("readr")
install.packages("dplyr")
install.packages("ggplot2")
library(readr)
library(dplyr)
library(ggplot2)

#Luego, vamos a pasar los datos de nuestras tablas a nuestro programa convirtiendolos en variables.

DatosEst <- read_csv2("Asistencia_Jovenes.csv")
DatosEco <- read_csv2("Actividad_Economica.csv")
DatosOMSA <- read_csv2("Nomina_Emp_OMSA.csv")

#Vamos ahora a confirmar si las variables han recibido los datos exitosamente.

View(DatosEst)
View(DatosEco)
View(DatosOMSA)

#Ahora que tenemos los datos, vamos a remover los valores nulos que se encuentran en estos, para tener una mejor y mas limpia estadistica.

datos1 <- na.omit(DatosEst)
datos2 <- na.omit(DatosEco)
datos3 <- na.omit(DatosOMSA)

#Voy a renombrar las columnas para que sea mas facil hacer las graficas.

datos1 = datos1[complete.cases(datos1),]
names(datos1)=c("TIPO", "CANTIDAD", "MES", "YEAR")

datos2 = datos2[complete.cases(datos2),]
names(datos2)=c("SUJETOS", "CANTIDAD", "YEAR")

datos3 = datos3[complete.cases(datos3),]
names(datos3)=c("NOMBRE", "DEPT", "ROL", "SUELDO", "STATE", "MES", "YEAR")

View(datos1)
View(datos2)
View(datos3)

#Ahora que tenemos nuestras tablas, podemos hacer nuestras Graficas.

#Ejercicio 1- Asistencia de Jovenes

attach(datos1)

#Buscamos la Media, Mediana, y Moda de la cantidad los datos.

mean(CANTIDAD) #La media es 5.39
median(CANTIDAD) #La mediana es 3.5
table(CANTIDAD)
which.max(table(CANTIDAD)) #La moda es 0

hist(CANTIDAD) #Hacemos la primera grafica, un Histograma.

densidad_cant <- density(CANTIDAD)
plot(densidad_cant, 
     main = "Histograma de densidad de la cantidad de Jovenes",
     xlab = "Cantidad",
     ylab = "Frecuencia")

plot(CANTIDAD, main = "Gráfico demostrando la cantidad de los Jovenes.",
     xlab = "Cantidad", ylab = "Frecuencia")

boxplot(CANTIDAD ~ MES, main = "Gráfico de Diagrama de Cajas de los Jovenes",
        xlab = "Mes",
        ylab = "Cantidad",
        col="green"
)

plot(x = CANTIDAD, y = YEAR,
     main = "Gráfico de Dispercion de los Jovenes.",
     ylab = "Año",
     xlab = "Cantidad",
     sub = "Evaluando su cantidad y El año.",
     col = c("#99cc99", "#cc9999", "#9999cc", "#9c9c9c")
)

#Ejercicio 2- Sujetos Obligados por Actividad Económica

attach(datos2)

#Buscamos la Media, Mediana, y Moda de la cantidad los datos.

mean(CANTIDAD) #La media es 41.89
median(CANTIDAD) #La mediana es 7
table(CANTIDAD)
which.max(table(CANTIDAD)) #La moda es 1

hist(CANTIDAD) #Hacemos la primera grafica, un Histograma.

densidad_cant <- density(CANTIDAD)
plot(densidad_cant, 
     main = "Histograma de densidad de la cantidad de los Sujetos",
     xlab = "Cantidad",
     ylab = "Frecuencia")

plot(CANTIDAD, main = "Gráfico demostrando la cantidad de los Sujetos.",
     xlab = "Cantidad", ylab = "Frecuencia")

boxplot(CANTIDAD ~ YEAR, main = "Gráfico de Diagrama de Cajas de los Sujetos",
        xlab = "Año",
        ylab = "Cantidad",
        col="green"
)

plot(x = CANTIDAD, y = YEAR,
     main = "Gráfico de Dispercion de los Jovenes.",
     ylab = "Año",
     xlab = "Cantidad",
     sub = "Evaluando su cantidad y El año.",
     col = c("#99cc99", "#cc9999", "#9999cc", "#9c9c9c")
)

#Ejercicio 3- Nómina de Empleados, 2018 - 2021 

attach(datos3)

#Buscamos la Media, Mediana, y Moda de los sueldos de los Empleados.

mean(SUELDO) #La media es 69063.41
median(SUELDO) #La mediana es 10465
table(SUELDO)
which.max(table(SUELDO)) #La moda es 10000

hist(SUELDO) #Hacemos la primera grafica, un Histograma.

densidad_sueld <- density(SUELDO)
plot(densidad_sueld, 
     main = "Histograma de densidad de los sueldos de los Empleados",
     xlab = "Cantidad",
     ylab = "Frecuencia")

plot(SUELDO, main = "Gráfico demostrando la frecuencia de ciertos Sueldos",
     xlab = "Cantidad", ylab = "Frecuencia")

boxplot(SUELDO ~ YEAR, main = "Gráfico de Diagrama de Los Sueldos de los Empleados",
        xlab = "Año",
        ylab = "Sueldo",
        col="green"
)

plot(x = SUELDO, y = YEAR,
     main = "Gráfico de Dispercion de los Sueldos 2018-2020.",
     ylab = "Año",
     xlab = "Sueldo",
     sub = "Evaluando el sueldo de los empleados y El año que lo recibieron.",
     col = c("#99cc99", "#cc9999", "#9999cc", "#9c9c9c")
)
