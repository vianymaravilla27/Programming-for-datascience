#install.packages("nortest")
#Declatacion de las librerias que seras necesarias para este problema
library(tidyverse) 
library (ggplot2)
library (nortest)

#se abre el archivo y se filtran los datos quitandole los valores negativos y se almacena en otro dataframe
r <- read.csv(file = file.choose())
sin_nega <- filter(r, r$Occupancy >= 0)

#se sacan las normalidades de las dos variables que vamos a emplear
norma_capa <- lillie.test(sin_nega$Capacity)
norma_ocu <- lillie.test(sin_nega$Occupancy)

#Ordenar los datos de forma decreciente
sin_nega <- sort(sin_nega, decreasing = FALSE, na.last = FALSE)
View(sin_nega)

#Se grafica el histograma y se procede a medir la normalidad de las dos variables 
hist(sin_nega$Occupancy)
cat("\nDebido al grafico podemos ver que no contiene una normalidad muy buena, se ve que esta muy pegada a la derecha,\nSin embargo usamos el lillie test para comprobar:")
norma_Ocupa <- lillie.test(sin_nega$Occupancy)
norma_Ocupa
cat("\nComo el p-value es menor a 0.5 se descarta ya que no contiene una distribucion normal")

hist(sin_nega$Capacity)
cat("\nComo vemos en grafico la variable de la capacidad no contien una distribucion normal, pero usaremos el test para comprobarlo:")
norma_capa <- lillie.test(sin_nega$Capacity)
norma_capa
cat("\nComo el p-value es menor a 0.5 se descarta")

#Se crea un dataframe para almacenar solo las dos variables para ocuparla en la 
pearson <- cor(x = sin_nega$Capacity, y = sin_nega$Occupancy, method = "pearson")
kendall <- cor(x = sin_nega$Capacity, y = sin_nega$Occupancy, method = "kendall")
dos <- data.frame("Capacidad" = sin_nega$Capacity, "Ocupacion" =sin_nega$Occupancy)
cat("\nCorrelación:")
correlacion_p <- round(cor(dos, method = "pearson"),2)
cat("\nCoeficiente de perason",pearson,"tiene una correlación significativa\n","\nTabla de correlacion con el coeficiente Pearson\n")
correlacion_p

correlacion_k <- round(cor(dos, method = "kendall"),2)
cat("\nCoeficiente Kendall:",kendall,"tiene una correlación moderada\n", "\nTabla de correlacion con el coeficiente Kendall\n")
correlacion_k


#Grafica de dispercion de ambas variables
f <- ggplot(dos) + geom_point(dos, mapping = aes(x = Capacidad, y = Ocupacion), color = "blue")
f
cat("Como hay relacion fuerte se obtiene la regresion lineal para este modelo")
model1 <- lm(data = dos, dos$Capacidad~dos$Ocupacion)
summary(model1)
h <- ggplot(dos) + geom_point(dos, mapping = aes(x = Capacidad, y = Ocupacion), color = "blue") + geom_smooth(formula = y ~ x, method = "lm", colour = "red",aes(x = Capacidad, y = Ocupacion))
h
                    