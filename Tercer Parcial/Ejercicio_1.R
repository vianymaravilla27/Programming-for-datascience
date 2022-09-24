#Practica de analisis de componentes principales ejercicio número 1
#Se carga el data set correspondinete 
file <- read.csv(file.choose())

#Descripcion de los datos
media <- apply(file, MARGIN = 2, FUN = mean)
varianza <- apply(file, MARGIN = 2, FUN = var)
desviacion <- apply(file, MARGIN = 2, FUN = sd)

descr_file <- data.frame(media, varianza, desviacion)
descr_file

#Se estandarizan las variables 
pca <- prcomp(file, scale = TRUE)
names(pca)
pca$center #media
pca$scale #Sd
pca$rotation #componentes principales

biplot(x = pca, scale = 0.0, cex = 0.5, col = c("blue4", "brown"))

#se invierten los signos de las variables
pca$rotation <- -pca$rotation
pca$x        <- -pca$x
biplot(x = pca, scale = 0, cex = 0.6, col = c("blue4", "brown3"))
